-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/0]).

-export([init/3]).
-export([handle_open/2]).
-export([handle_message/3]).
-export([handle_signal/2]).
-export([handle_close/2]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

test() ->
    hydna_lib:start(),
    Opts = [test],
    hydna_lib:open("public.hydna.net/cli-test", <<"rw">>, ?MODULE, Opts).

%% Callbacks

init(Domain, Channel, Opts) ->
    lager:info("Opts: ~p", [Opts]),
    {ok, [{domain, Domain}, {channel, Channel}]}.

handle_open(_Message, State) ->
    lager:info("Channel opened! ~p", [State]),
    {ok, State}.

handle_message(Message, Meta, State) ->
    lager:info("Message: ~p, ~p, ~p", [Message, Meta, State]),
    {ok, State}.

handle_signal(Message, State) ->
    lager:info("Signal: ~p (~p)", [Message, State]),
    {ok, State}.

handle_close(Reason, State) ->
    lager:info("Close: ~p", [Reason]),
    {ok, State}.

handle_error(Reason, State) ->
    lager:info("Error: ~p", [Reason]),
    {ok, State}.

handle_info({send, Message}, State) ->
    {message, Message, binary, State};
handle_info(_Message, State) ->
    {ok, State}.

terminate(Reason, _State) ->
    lager:info("Handler module was terminated: ~p", [Reason]),
    ok.
