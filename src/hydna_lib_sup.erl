-module(hydna_lib_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_domain/3]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% External API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Callbacks

init([]) ->
    Procs = [],
    {ok, {
        {one_for_one, 10, 10},
        Procs}}.

start_domain(Protocol, Hostname, Port) ->
    Ref = create_domain_ref(Protocol, Hostname, Port),
    Args = [Protocol, Hostname, Port],
    Spec = {Ref, {hydna_lib_domain, start_link, Args},
            transient, 5000, worker, [hydna_lib_domain]},
    case supervisor:start_child(hydna_lib_sup, Spec) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        _ ->
            %% deal with this
            error
    end.

create_domain_ref(Protocol, Hostname, Port) ->
    list_to_atom(string:join(["hydna_lib_domain", atom_to_list(Protocol),
                              Hostname, integer_to_list(Port)], "_")).
