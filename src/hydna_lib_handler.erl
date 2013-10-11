-module(hydna_lib_handler).

-type state() :: term().
-type reason() :: term().
-type message() :: binary().
-type channel() :: binary().
-type domain() :: binary().
-type opts() :: any().
-type priority() :: 0..3.
-type encoding() :: binary | utf8.
-type meta_option() :: {encoding, encoding()} | {priority, priority()}.
-type meta() :: [meta_option()].
-type normal_return() :: {stop, state()}
    | {stop, reason(), state()}
    | {ok, state()}
    | {message, message(), state()}
    | {message, message(), encoding(), state()}
    | {signal, message(), state()}.
-type error_return() :: {ok, state()}.

-callback init(domain(), channel(), opts()) ->
    {ok, state()} | {stop, reason()}.

-callback handle_open(message(), state()) ->
    normal_return().

-callback handle_message(message(), meta(), state()) ->
    normal_return().

-callback handle_signal(message(), state()) ->
    normal_return().

-callback handle_info(term(), state()) ->
    normal_return().

-callback handle_close(reason(), state()) ->
    error_return().

-callback handle_error(reason(), state()) ->
    error_return().

-callback terminate(reason(), state()) ->
    ok.
