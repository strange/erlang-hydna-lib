-module(hydna_lib_proxy).

-behaviour(gen_server).

-export([start_link/0]).
-export([open/6]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% External API

open(Hostname, Port, Channel, Mode, Token, Mod) ->
    gen_server:call(?MODULE, {open, Hostname, Port, Channel, Mode, Token,
            Mod}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callbacks

init([]) ->
    State = dict:new(),
    {ok, State}.

handle_call({open, Hostname, Port, Channel, Mode, Token, Mod}, _From, State) ->
    case maybe_connect(Hostname, Port, State) of
        {ok, Pid, NewState} ->
            Response = hydna_lib_domain:open(Pid, Channel, Mode, Token, Mod),
            {reply, Response, NewState};
        Other ->
            {reply, Other, State}
    end;

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

%% maybe_connect(Hostname, Port, State) ->
%%     {ok, Pid} = hydna_lib_domain:start_link(Hostname, Port),
%%     {ok, Pid, dict:store({Hostname, Port}, Pid, State)}.

maybe_connect(Hostname, Port, State) ->
    case dict:find({Hostname, Port}, State) of
        {ok, Pid} ->
            {ok, Pid, State};
        error ->
            {ok, Pid} = hydna_lib_domain:start_link(Hostname, Port),
            {ok, Pid, dict:store({Hostname, Port}, Pid, State)}
    end.
