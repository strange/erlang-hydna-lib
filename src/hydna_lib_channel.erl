-module(hydna_lib_channel).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
        handler_mod,
        handler_state,
        domain_name,
        domain_pid
    }).

%% External API

start_link(Mod, DomainName, DomainPid) ->
    Args = [Mod, DomainName, DomainPid],
    gen_server:start_link(?MODULE, Args, []).

%% Callbacks

init([Mod, DomainName, DomainPid]) ->
    try Mod:init(DomainName) of
        {ok, HandlerState} ->
            State = #state{
                handler_state = HandlerState,
                handler_mod = Mod,
                domain_name = DomainName,
                domain_pid = DomainPid
            },
            {ok, State};
        {stop, Reason} ->
            {stop, Reason}
    catch
        ErrorClass:Reason ->
            {stop, {ErrorClass, Reason}}
    end.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({open_allowed, Channel, Message}, State) ->
    handle_normal_response(handle_open, Channel, [Message], State);

handle_info({open_denied, Channel, Message}, State) ->
    handle_error_response(handle_error, [Channel, {denied, Message}], State);

handle_info({data, Channel, Message}, State) ->
    handle_normal_response(handle_message, Channel, [Message], State);

handle_info({emit, Channel, Message}, State) ->
    handle_normal_response(handle_signal, Channel, [Message], State);

handle_info({error, Reason}, State) ->
    handle_error_response(handle_error, [Reason], State);

handle_info({error, Channel, Reason}, State) ->
    handle_error_response(handle_error, [Channel, Reason], State);

handle_info({disconnect, Reason}, State) ->
    handle_error_response(handle_error, [Reason], State);

handle_info(Msg, State) ->
    %% Relay all other messages to the handler.
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

handle_error_response(CallbackName, Args, State) ->
    try callback(handle_error, Args, State) of
        {ok, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            terminate_handler(normal, NewState);
        Other ->
            terminate_handler({bad_return_value, Other}, State)
    catch
        ErrorClass:Reason ->
            terminate_handler({ErrorClass, Reason}, State)
    end.

handle_normal_response(CallbackName, Channel, Args, State) ->
    try callback(CallbackName, [Channel|Args], State) of
        {stop, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            terminate_handler(normal, NewState);
        {ok, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            {noreply, NewState};
        {message, Msg, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            hydna_lib_domain:send(NewState#state.domain_pid, Channel, Msg),
            {noreply, NewState};
        {signal, Msg, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState},
            hydna_lib_domain:emit(NewState#state.domain_pid, Channel, Msg),
            {noreply, NewState};
        Other ->
            terminate_handler({bad_return_value, Other}, State)
    catch
        ErrorClass:Reason ->
            terminate_handler({ErrorClass, Reason}, State)
    end.

terminate_handler(Reason, State) ->
    try callback(terminate, [Reason], State) of
        ok ->
            {stop, Reason, State};
        Other ->
            {stop, {bad_return_value, Other}, State}
    catch
        ErrorClass:Reason ->
            {stop, {ErrorClass, Reason}, State}
    end.

callback(CallbackName, Args, State) ->
    #state{handler_mod = HandlerMod, handler_state = HandlerState} = State,
    apply(HandlerMod, CallbackName, Args ++ [HandlerState]).
