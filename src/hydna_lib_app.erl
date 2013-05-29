-module(hydna_lib_app).

-behaviour(application).

-export([start/2, stop/1]).

%% External API

start(_StartType, _StartArgs) ->
    hydna_lib_sup:start_link().

stop(_State) ->
    ok.
