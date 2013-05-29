# Erlang bindings for Hydna

www.hydna.com

A work in progress!

## Usage

Example:

```erlang
-module(my_handler).

-export([start/0]).

-export([init/1]).
-export([handle_open/3]).
-export([handle_message/3]).
-export([handle_signal/3]).
-export([handle_close/3]).
-export([handle_error/3]).
-export([handle_error/2]).
-export([handle_info/2]).
-export([terminate/2]).

start() ->
    {ok, Pid} = hydna_lib:open("localhost:7010/2", <<"rw">>, ?MODULE).

%% Callbacks

init(_Domain) ->
    {ok, state}.

handle_open(_Channel, Message, State) ->
    lager:info("Channel opened! ~p", [now()]),
    {message, <<"test">>, State}.

handle_message(_Channel, Message, State) ->
    lager:info("Message: ~p", [Message]),
    {ok, State}.

handle_signal(Channel, Message, State) ->
    lager:info("Signal: ~p ~p", [Message, Channel]),
    {ok, State}.

handle_close(_Channel, Reason, State) ->
    lager:info("Close: ~p", [Reason]),
    {ok, State}.

handle_error(_Channel, Reason, State) ->
    lager:info("Error: ~p", [Reason]),
    {ok, State}.

handle_error(Reason, State) ->
    lager:info("Domain-error: ~p", [Reason]),
    {ok, State}.

handle_info(Message, State) ->
    lager:info("Other message: ~p", [Message]),
    {ok, State}.

terminate(Channel, State) ->
    lager:info("Handler module was terminated."),
    ok.
```

## TODO

* Test stuff :)
* Handle encoding (utf8/binary)
* Add a behaviour specification