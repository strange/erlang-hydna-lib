# Erlang bindings for Hydna

The bindings are now fairly stable. There are still a few things missing, some
adjustments to the API to make, and more tests to run before I'll make this an
official library.

Read more about Hydna here: http://www.hydna.com/

## Usage

Example:

```erlang
-module(hydna_lib_example).

-behaviour(hydna_lib_handler).

-export([test/1]).

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
    hydna_lib:open(URI, [read, write], ?MODULE).

%% Callbacks

init(Domain, Channel, Opts) ->
    lager:info("Opts: ~p", [Opts]),
    {ok, [{domain, Domain}, {channel, Channel}]}.

handle_open(Message, State) ->
    lager:info("Channel opened! ~p ~p", [Message, State]),
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
```

## Usage (push)

```erlang
hydna_lib:start().
hydna_lib:send("http://public.hydna.net/", <<"A message!">>).
hydna_lib:emit("http://public.hydna.net/", <<"A message!">>).
```
