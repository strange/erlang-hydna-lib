-module(hydna_lib_push).

-export([send/2]).
-export([emit/2]).

-include("hydna_lib.hrl").

send(URL, Message) ->
    request(URL, Message, []).

emit(URL, Message) ->
    request(URL, Message, [{"x-emit", "yes"}]).

%% Private API

request(_URL, Message, _Headers) when length(Message) > ?PAYLOAD_MAX_LENGTH ->
    {error, invalid_payload_size};
request(URL, Message, Headers) ->
    ContentType = "application/x-www-form-urlencoded",
    case httpc:request(post, {URL, Headers, ContentType, Message}, [], []) of
        {ok, {{_Ver, 200, _Msg}, _, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.
