-module(hydna_lib_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% External API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Callbacks

init([]) ->
    Procs = [?CHILD(hydna_lib_proxy, worker)],
    {ok, {
        {one_for_one, 10, 10},
        Procs}}.
