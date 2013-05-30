-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/0]).

-export([init/2]).
-export([handle_open/2]).
-export([handle_message/3]).
-export([handle_signal/2]).
-export([handle_close/2]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

test() ->
    application:start(lager),
    application:start(hydna_lib),
    hydna_lib:open("localhost:7010/1", <<"rw">>, ?MODULE).

%% Callbacks

init(Domain, Channel) ->
    {ok, [{domain, Domain}, {channel, Channel}]}.

handle_open(_Message, State) ->
    lager:info("Channel opened! ~p", [now()]),
    {message, <<"test">>, State}.

handle_message(Message, Meta, State) ->
    Encoding = proplists:get_value(encoding, Meta),
    lager:info("Message: ~p in ~p", [Message, Encoding]),
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

handle_info(Message, State) ->
    lager:info("Other message: ~p", [Message]),
    {ok, State}.

terminate(_Reason, _State) ->
    lager:info("Handler module was terminated."),
    ok.
