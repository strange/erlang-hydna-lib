-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/0]).
-export([test2/0]).

-export([init/2]).
-export([handle_open/2]).
-export([handle_message/3]).
-export([handle_signal/2]).
-export([handle_close/2]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

test2() ->
    hydna_lib:start(),
    start_n(2000).

start_n(0) -> ok;
start_n(N) ->
    hydna_lib:open("test-aug-2013.hydna.net/" ++ integer_to_list(N), <<"rw">>, ?MODULE),
    start_n(N - 1).

test() ->
    hydna_lib:start(),
    hydna_lib:open("localhost:7010/1", <<"rw">>, ?MODULE),
    ok.
    %% hydna_lib:send("http://localhost:7010/1", <<"test">>).

%% Callbacks

init(Domain, Channel) ->
    {ok, [{domain, Domain}, {channel, Channel}]}.

handle_open(_Message, State) ->
    %% lager:info("Channel opened! ~p", [now()]),
    %% self() ! {send, <<"Hello world!">>},
    {ok, State}.

handle_message(Message, _Meta, State) ->
    lager:info("Message: ~p", [Message]),
    {ok, State}.

handle_signal(Message, State) ->
    lager:info("Signal: ~p", [Message]),
    {ok, State}.

handle_close(Reason, State) ->
    lager:info("Close: ~p", [Reason]),
    {ok, State}.

handle_error(Reason, State) ->
    lager:info("Error: ~p", [Reason]),
    {ok, State}.

handle_info({send, Message}, State) ->
    {message, Message, State};
handle_info(_Message, State) ->
    {ok, State}.

terminate(Reason, _State) ->
    lager:info("Handler module was terminated: ~p", [Reason]),
    ok.
