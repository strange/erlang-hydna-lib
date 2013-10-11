-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/0]).
-export([load/1]).

-export([init/2]).
-export([handle_open/2]).
-export([handle_message/3]).
-export([handle_signal/2]).
-export([handle_close/2]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

load(Path) ->
    hydna_lib:start(),
    hydna_lib:open("localhost:7010/" ++ Path, <<"rw">>, ?MODULE).

test() ->
    hydna_lib:start(),
    [hydna_lib:open("localhost:7010/" ++ integer_to_list(I) , <<"rw">>, ?MODULE)
     || I <- lists:seq(0, 10)].

    %% hydna_lib:open("localhost:7010/hello", <<"rw">>, ?MODULE).

%% Callbacks

init(Domain, Channel) ->
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
