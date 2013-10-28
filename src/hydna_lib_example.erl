-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/1]).
-export([testn/2]).

-export([init/3]).
-export([handle_open/2]).
-export([handle_message/3]).
-export([handle_signal/2]).
-export([handle_close/2]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

test(URI) ->
    hydna_lib:start(),
    hydna_lib:open(URI, [read, write], ?MODULE, [erlang:now()]).

testn(Domain, N) ->
    hydna_lib:start(),
    Now = erlang:now(),
    testn(Domain, Now, N).

testn(_Domain, _Now, 0) -> ok;
testn(Domain, Now, N) ->
    Path = list_to_binary(integer_to_list(N)),
    hydna_lib:open(<<Domain/binary, "/", Path/binary>>, [read, write], ?MODULE, [Now]),
    testn(Domain, N - 1).

%% Callbacks

init(Domain, Channel, Opts) ->
    [Now] = Opts,
    {ok, [{channel, Channel}, {now, Now}]}.

handle_open(Message, State) ->
    lager:info("Opened channel!"),
    case proplists:get_value(channel, State) of
        <<"/1">> ->
            Now = proplists:get_value(now, State),
            Diff = timer:now_diff(erlang:now(), Now),
            lager:info("Channels opened in: ~p", [Diff]);
        _Other ->
            npp
    end,
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
