-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/1]).
-export([test/2]).
-export([testn/2]).
-export([testl/0]).

-export([init/3]).
-export([handle_open/2]).
-export([handle_message/3]).
-export([handle_signal/2]).
-export([handle_close/2]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([receive_loop/1]).

test(URI) -> test(URI, [read, write]).

test(URI, Modes) ->
    hydna_lib:start(),
    hydna_lib:open(URI, Modes, ?MODULE, [erlang:now()]).

testn(Domain, N) ->
    hydna_lib:start(),
    Now = erlang:now(),
    State = [{open, 0}, {closed, 0}, {error, 0}],
    Receiver = spawn(?MODULE, receive_loop, [{N, Now, State}]),
    testn(Domain, Receiver, N),
    Receiver.

testn(_Domain, _Receiver, 0) -> ok;
testn(Domain, Receiver, N) ->
    Path = list_to_binary(integer_to_list(N)),
    hydna_lib:open(<<Domain/binary, "/", Path/binary>>, [read, write], ?MODULE, [Receiver]),
    testn(Domain, Receiver, N - 1).

testl() ->
    hydna_lib:start(),
    hydna_lib:open(<<"pipsq.com:3000">>, [read, write], ?MODULE, [x]).

receive_loop({N, When, State}) ->
    NewState  = receive
        info ->
            lager:info("State: ~p", [State]),
            State;
        open -> 
            {open, Value} = lists:keyfind(open, 1, State),
            NewValue = Value + 1,
            if NewValue =:= N ->
               lager:info("Completed in: ~p", [timer:now_diff(erlang:now(), When)]);
               true -> nop
            end,
            lists:keyreplace(open, 1, State, {open, NewValue});
        What -> 
            {What, Value} = lists:keyfind(What, 1, State),
            lists:keyreplace(What, 1, State, {What, Value + 1})
    end,
    receive_loop({N, When, NewState}).


%% Callbacks

init(Domain, Channel, Opts) ->
    [Receiver] = Opts,
    {ok, [{channel, Channel}, {receiver, Receiver}]}.

handle_open(Message, State) ->
    Receiver = proplists:get_value(receiver, State),
    Receiver ! open,
    %% lager:info("Opened channel!"),
    %% <<M:65530/binary, _/binary>> = base64:encode(crypto:strong_rand_bytes(66000)),
    %% self() ! {send, M},
    {ok, State}.

handle_message(Message, Meta, State) ->
    lager:info("Message: ~p, ~p, ~p (~p)", [Message, Meta, State, byte_size(Message)]),
    {ok, State}.

handle_signal(Message, State) ->
    lager:info("Signal: ~p (~p)", [Message, State]),
    {ok, State}.

handle_close(Reason, State) ->
    lager:info("Close: ~p", [Reason]),
    Receiver = proplists:get_value(receiver, State),
    Receiver ! closed,
    {ok, State}.

handle_error(Reason, State) ->
    lager:info("Error: ~p", [Reason]),
    Receiver = proplists:get_value(receiver, State),
    Receiver ! error,
    {ok, State}.

handle_info({send, Message}, State) ->
    {message, Message, utf8, State};
handle_info(Message, State) ->
    lager:info("Other message: ~p", [Message]),
    {ok, State}.

terminate(Reason, _State) ->
    %% lager:info("Handler module was terminated: ~p", [Reason]),
    ok.
