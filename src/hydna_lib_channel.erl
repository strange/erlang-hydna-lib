-module(hydna_lib_channel).

-behaviour(gen_server).

-export([start_link/6]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
        handler_mod,
        handler_state,
        domain_name,
        domain_pid,
        channel,
        channel_id,
        path,
        mode,
        token
    }).

%% External API

start_link(Mod, DomainName, DomainPid, Path, Mode, Token) ->
    Args = [Mod, DomainName, DomainPid, Path, Mode, Token],
    gen_server:start_link(?MODULE, Args, []).

%% Callbacks

init([Mod, DomainName, DomainPid, Path, Mode, Token]) ->
    try Mod:init(DomainName, Path) of
        {ok, HandlerState} ->
            State = #state{
                handler_state = HandlerState,
                handler_mod = Mod,
                domain_name = DomainName,
                domain_pid = DomainPid,
                path = Path,
                mode = Mode,
                token = Token
            },
            {ok, State};
        {stop, Reason} ->
            {stop, Reason}
    catch
        ErrorClass:Reason ->
            lager:info("Error: ~p", [erlang:get_stacktrace()]),
            {stop, {ErrorClass, Reason}}
    end.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({resolved, Channel}, State) ->
    hydna_lib_domain:open(State#state.domain_pid, Channel, State#state.mode,
                          State#state.token),
    {noreply, State#state{channel = Channel}};

handle_info({open_allowed, _Channel, Message}, State) ->
    handle_normal_response(handle_open, [Message], State);

handle_info({open_denied, _Channel, Message}, State) ->
    handle_error_response(handle_error, [{denied, Message}], State);

handle_info({data, _Channel, Prio, Encoding, Message}, State) ->
    Meta = [{priority, Prio}, {encoding, Encoding}],
    handle_normal_response(handle_message, [Message, Meta], State);

handle_info({emit, _Channel, Message}, State) ->
    handle_normal_response(handle_signal, [Message], State);

handle_info({error, Reason}, State) ->
    handle_error_response(handle_error, [Reason], State);

handle_info({error, _Channel, Reason}, State) ->
    handle_error_response(handle_error, [Reason], State);

handle_info({disconnect, Reason}, State) ->
    handle_error_response(handle_error, [Reason], State);

handle_info(Message, State) ->
    handle_normal_response(handle_info, [Message], State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

handle_error_response(CallbackName, Args, State) ->
    try callback(CallbackName, Args, State) of
        {ok, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            terminate_handler(normal, NewState);
        Other ->
            terminate_handler({bad_return_value, Other}, State)
    catch
        ErrorClass:Reason ->
            terminate_handler({ErrorClass, Reason}, State)
    end.

handle_normal_response(CallbackName, Args, State) ->
    try callback(CallbackName, Args, State) of
        {stop, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            terminate_handler(normal, NewState);
        {stop, Reason, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            terminate_handler(Reason, NewState);
        {ok, NewHandlerState} ->
            {noreply, State#state{handler_state = NewHandlerState}};
        {message, Msg, NewHandlerState} when is_binary(Msg) ->
            hydna_lib_domain:send(State#state.domain_pid,
                State#state.channel, utf8, Msg),
            {noreply, State#state{handler_state = NewHandlerState}};
        {message, Msg, Encoding, NewHandlerState} when is_binary(Msg) andalso
                                                  (Encoding =:= utf8 orelse
                                                   Encoding =:= raw) ->
            hydna_lib_domain:send(State#state.domain_pid,
                State#state.channel, Encoding, Msg),
            {noreply, State#state{handler_state = NewHandlerState}};
        {signal, Msg, NewHandlerState} when is_binary(Msg) ->
            hydna_lib_domain:emit(State#state.domain_pid,
                State#state.channel, Msg),
            {noreply, State#state{handler_state = NewHandlerState}};
        Other ->
            terminate_handler({bad_return_value, Other}, State)
    catch
        ErrorClass:Reason ->
            lager:info("Error: ~p", [erlang:get_stacktrace()]),
            terminate_handler({ErrorClass, Reason}, State)
    end.

terminate_handler(Reason, State) ->
    try callback(terminate, [Reason], State) of
        ok ->
            {stop, Reason, State};
        Other ->
            {stop, {bad_return_value, Other}, State}
    catch
        ErrorClass:Reason ->
            lager:info("Error: ~p", [erlang:get_stacktrace()]),
            {stop, {ErrorClass, Reason}, State}
    end.

callback(CallbackName, Args, State) ->
    #state{handler_mod = HandlerMod, handler_state = HandlerState} = State,
    apply(HandlerMod, CallbackName, Args ++ [HandlerState]).
