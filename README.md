Erlang bindings for Hydna (www.hydna.com).

A work in progress!

Example:

    -module(my_handler).

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
