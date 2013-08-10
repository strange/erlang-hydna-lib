# Erlang bindings for Hydna

www.hydna.com

A work in progress!

## Usage

Example:

```erlang
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
    self() ! {send, <<"Hello world!">>},
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
```

## Usage (push)

```erlang
hydna_lib:start().
hydna_lib:send("http://public.hydna.net/", <<"A message!">>).
hydna_lib:emit("http://public.hydna.net/", <<"A message!">>).
```


## TODO

* I just wrote this ... will need to do some testing :)
* `handle_message/3` takes a proplist (`Meta`) as it's second argument. I'm
  considering expanding the list into two separate arguments: `Encoding` and
  `Priority`.
* Most callbacks used to take a `Channel`-argument. I moved the argument to
  `init/2` as it cannot change within the lifespan of a process. This resulted
  in shorter function signatures, but makes it harder to pattern match on
  channel (when using the same handler module for multiple channels) and I
  need to create a way to distinguish between channel-related- and domain-wide
  errors.
* Get rid of some OTP cruft and look over the role of the supervisor (should
  probably do the work of `hydna_lib_proxy`).
