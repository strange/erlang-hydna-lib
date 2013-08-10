-module(hydna_lib_push).

-export([send/2]).
-export([emit/2]).

send(URL, Message) ->
    request(URL, Message, []).

emit(URL, Message) ->
    request(URL, Message, [{"x-emit", "yes"}]).

%% Private API

request(URL, Message, Headers) ->
    ContentType = "application/x-www-form-urlencoded",
    case httpc:request(post, {URL, Headers, ContentType, Message}, [], []) of
        {ok, {{_Ver, 200, _Msg}, _, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.
