-module(hydna_lib_domain).

-behaviour(gen_server).

-export([start_link/2]).
-export([open/5]).
-export([open/4]).
-export([send/4]).
-export([emit/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
        connection_state,
        handlers = [],
        resolve_buf = [],
        socket,
        hostname,
        port
    }).

-define(HEARTBEAT, 0).
-define(OPEN, 1).
-define(DATA, 2).
-define(EMIT, 3).
-define(RSLV, 4).

-define(OPEN_OK, 0).
-define(OPEN_REDIRECT, 1).
-define(OPEN_DENY, 3).

-define(EMIT_SIGNAL, 0).
-define(EMIT_END, 1).

-define(MODE_LISTEN, 0).
-define(MODE_READ, 1).
-define(MODE_WRITE, 2).
-define(MODE_EMIT, 4).

%% External API

start_link(Hostname, Port) ->
    gen_server:start_link(?MODULE, [Hostname, Port], []).

open(Pid, Path, Mode, Token, Mod) ->
    gen_server:call(Pid, {open, Path, Mode, Token, Mod}).

open(Pid, Path, Mode, Token) ->
    gen_server:cast(Pid, {open, Path, Mode, Token}).

send(Pid, Path, utf8, Message) ->
    gen_server:cast(Pid, {send, Path, 1, Message});
send(Pid, Path, raw, Message) ->
    gen_server:cast(Pid, {send, Path, 0, Message}).

emit(Pid, Path, Message) ->
    gen_server:cast(Pid, {emit, Path, Message}).

%% Callbacks

init([Hostname, Port]) ->
    State = #state{
        hostname = Hostname,
        port = Port
    },
    {ok, State}.

handle_call({open, Path, Mode, Token, Mod}, _From, State) ->
    Hostname = State#state.hostname,
    case hydna_lib_channel:start_link(Mod, Hostname, self(), Path,
                                      Mode, Token) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            State0 = add_resolve_request(Path, Pid, State),
            State1 = maybe_connect(self(), State0),
            {reply, {ok, Pid}, State1};
        Other ->
            {reply, Other, State}
    end;

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast({open, Pointer, Mode, Token}, State) ->
    Data = <<Pointer:32, 0:2, ?OPEN:3, Mode:3, Token/binary>>,
    Len = byte_size(Data) + 2,
    Packet = <<Len:16, Data/binary>>,
    ok = gen_tcp:send(State#state.socket, Packet),
    {noreply, State};

handle_cast({send, Pointer, Encoding, Message}, State) ->
    Data = <<Pointer:32, 0:2, ?DATA:3, Encoding:3, Message/binary>>,
    Len = byte_size(Data) + 2,
    Packet = <<Len:16, Data/binary>>,
    ok = gen_tcp:send(State#state.socket, Packet),
    {noreply, State};

handle_cast({emit, Pointer, Message}, State) ->
    Data = <<Pointer:32, 0:2, ?EMIT:3, ?EMIT_SIGNAL:3, Message/binary>>,
    Len = byte_size(Data) + 2,
    Packet = <<Len:16, Data/binary>>,
    ok = gen_tcp:send(State#state.socket, Packet),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({connected, Socket}, State) ->
    State0 = State#state{ socket = Socket, connection_state = connected },
    State1 = send_resolve_requests(State0),
    {noreply, State1};

handle_info({resolved, Pointer, Path}, State) ->
    {ok, Handler} = get_handler(Path, State),
    NewState = State#state{
        handlers = lists:keyreplace(Path, 2, State#state.handlers,
                                    {Pointer, Path, Handler})
    },
    Handler ! {resolved, Pointer},
    {noreply, NewState};

handle_info({open_allowed, Pointer, Message}, State) ->
    {ok, Handler} = get_handler(Pointer, State),
    Handler ! {open_allowed, Pointer, Message},
    {noreply, State};

handle_info({open_denied, Pointer, Message}, State) ->
    {ok, Handler} = get_handler(Pointer, State),
    Handler ! {open_denied, Pointer, Message},
    {noreply, State};

handle_info({data, Pointer, Prio, Encoding, Message}, State) ->
    {ok, Handler} = get_handler(Pointer, State),
    Handler ! {data, Pointer, Prio, Encoding, Message},
    {noreply, State};

handle_info({emit, 0, Message}, State) ->
    [Handler ! {emit, 0, Message} || {_, _, Handler} <- State#state.handlers],
    {noreply, State};

handle_info({emit, Pointer, Message}, State) ->
    {ok, Handler} = get_handler(Pointer, State),
    Handler ! {emit, Pointer, Message},
    {noreply, State};

handle_info({disconnect, Reason}, State) ->
    [Handler ! {disconnect, Reason} || {_Pointer, _Path, Handler}
        <- State#state.handlers],
    {noreply, State#state{connection_state = disconnected,
            resolve_buf = [], handlers = []}};

handle_info({error, Reason}, State) ->
    [Handler ! {error, Reason} || {_, _, Handler}
        <- State#state.handlers],
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewState = remove_pid(Pid, State),
    {noreply, NewState};

handle_info(Msg, State) ->
    lager:info("Other: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

remove_pid(Pid, State) ->
    State#state{handlers = lists:keydelete(Pid, 3, State#state.handlers)}.

maybe_connect(_Pid, #state{connection_state = connected} = State) ->
    send_resolve_requests(State);
maybe_connect(_Pid, #state{connection_state = connecting} = State) ->
    State;
maybe_connect(Pid, State) ->
    spawn(fun() ->
        connect(Pid, State#state.hostname, State#state.port)
    end),
    State#state{connection_state = connecting}.

connect(Pid, Hostname, Port) ->
    lager:info("Connecting"),
    Opts = [{active, false}, {mode, binary}],
    case gen_tcp:connect(Hostname, Port, Opts) of
        {ok, Socket} ->
            send_handshake(Pid, Socket, Hostname);
        {error, Reason} ->
            Pid ! {disconnect, Reason}
    end.

send_handshake(Pid, Socket, Hostname) ->
    inet:setopts(Socket, [{packet, http_bin}]),
    gen_tcp:send(Socket, encode_handshake_packet(Hostname)),
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_response, _, 101, _}} ->
            discard_header(Pid, Socket, 3);
        {ok, {http_response, _, 400, _}} ->
            Pid ! {disconnect, invalid_domain}
    end.

discard_header(Pid, Socket, 0) ->
    case gen_tcp:recv(Socket, 0) of 
        {ok, http_eoh} ->
            %% we're done with the http protocol now and can switch back to
            %% using the raw mode.
            inet:setopts(Socket, [{packet, raw}]),
            Pid ! {connected, Socket},
            recv_header(Pid, Socket)
    end;

discard_header(Pid, Socket, N) ->
    case gen_tcp:recv(Socket, 0) of 
        {ok, {http_header, _ ,_ ,_ ,_ }} ->
            discard_header(Pid, Socket, N - 1)
    end.

recv_header(Pid, Socket) ->
    case gen_tcp:recv(Socket, 2) of 
        {ok, <<Len:16/integer>>} ->
            recv_payload(Pid, Socket, Len - 2);
        {error, Reason} ->
            Pid ! {disconnect, Reason}
    end.

recv_payload(Pid, Socket, Len) ->
    Payload = case gen_tcp:recv(Socket, Len) of 
        {ok, <<Ch:32, _:2, ?RSLV:3, 0:3, Path/binary>>} ->
            {resolved, Ch, Path};
        {ok, <<Ch:32, _:2, ?OPEN:3, ?OPEN_OK:3, Msg/binary>>} ->
            {open_allowed, Ch, Msg};
        {ok, <<0:32, _:2, ?HEARTBEAT:3, 0:3, Msg/binary>>} ->
            {heartbeat, Msg};
        {ok, <<Ch:32, _:2, ?OPEN:3, ?OPEN_REDIRECT:3, Msg/binary>>} ->
            {open_redirect, Ch, Msg};
        {ok, <<Ch:32, _:2, ?OPEN:3, ?OPEN_DENY:3, Reason/binary>>} ->
            {open_denied, Ch, Reason};
        {ok, <<Ch:32, _:2, ?DATA:3, Prio:2, Enc:1, Message/binary>>} ->
            Encoding = case Enc of 0 -> utf8; 1 -> raw end,
            {data, Ch, Prio, Encoding, Message};
        {ok, <<Ch:32, _:2, ?EMIT:3, ?EMIT_END:3/integer, Message/binary>>} ->
            {close, Ch, Message};
        {ok, <<Ch:32, _:2, ?EMIT:3, ?EMIT_SIGNAL:3/integer, Message/binary>>} ->
            {emit, Ch, Message};
        {error, Reason} ->
            {disconnect, Reason}
    end,
    case Payload of
        {disconnect, _} ->
            Pid ! Payload;
        {heartbeat, _} ->
            recv_header(Pid, Socket);
        _ ->
            Pid ! Payload,
            recv_header(Pid, Socket)
    end.

encode_handshake_packet(Hostname) ->
    string:join([
        "GET / HTTP/1.1",
        "Connection: Upgrade",
        "Upgrade: winksock/1",
        "Host: " ++ Hostname,
        "X-Accept-Redirects: yes",
        "X-SubProtocol: comet",
        "\r\n\r\n"
    ], "\r\n").

packet(Pointer, Op, Flag, Data) ->
    Len = byte_size(Data) + 7,
    <<Len:16, Pointer:32, 0:2, Op:3, Flag:3, Data/binary>>.

send_resolve_requests(State) ->
    #state{
        resolve_buf = ResolveBuf,
        socket = Socket
    } = State,
    [begin
        Packet = packet(0, ?RSLV, 0, Path),
        gen_tcp:send(Socket, Packet)
    end || Path <- ResolveBuf],
    State#state{resolve_buf = []}.    

add_resolve_request(Path, Handler, State) ->
    #state{
       handlers = Handlers,
       resolve_buf = ResolveBuf
    } = State,
    case get_handler(Path, State) of 
        {ok, _Handler} ->
            Handler ! {error, handler_already_registered},
            State;
        _ ->
            State#state{
                handlers = [{unassigned, Path, Handler}|Handlers],
                resolve_buf = [Path|ResolveBuf]
            }
    end.

get_handler(Path, State) when is_binary(Path) ->
    case lists:keyfind(Path, 2, State#state.handlers) of
        {_Pointer, _Path, Handler} ->
            {ok, Handler};
        false ->
            not_found
    end;

get_handler(Pointer, State) when is_integer(Pointer) ->
    case lists:keyfind(Pointer, 1, State#state.handlers) of
        {_Pointer, _Path, Handler} ->
            {ok, Handler};
        false ->
            not_found
    end.
