-module(hydna_lib).

-export([start/0]).

-export([open/3]).
-export([open/4]).
-export([send/2]).
-export([emit/2]).

start() ->
    ensure_started(lager),
    ensure_started(inets),
    application:start(hydna_lib),
    ok.

open(URI, Modes, HandlerMod) ->
    open(URI, Modes, HandlerMod, []).

open(URI, Modes, HandlerMod, Opts) ->
    case parse_uri(URI) of
        {ok, _Protocol, Domain, Port, Path, Token, _URI2} ->
            hydna_lib_proxy:open(Domain, Port, Path, parse_mode(Modes),
                                 Token, HandlerMod, Opts);
        {error, invalid_uri} ->
            {error, invalid_uri}
    end.

send(URI, Message) ->
    case parse_uri(URI) of
        {ok, _Protocol, _Domain, _Port, _Chan, _Token, URI2} ->
            hydna_lib_push:send(URI2, Message);
        {error, invalid_uri} ->
            {error, invalid_uri}
    end.

emit(URI, Message) ->
    case parse_uri(URI) of
        {ok, _Protocol, _Domain, _Port, _Chan, _Token, URI2} ->
            hydna_lib_push:emit(URI2, Message);
        {error, invalid_uri} ->
            {error, invalid_uri}
    end.

%% Internal API

parse_mode(Modes) when is_list(Modes) ->
    Modes2 = sets:to_list(sets:from_list(Modes)),
    lists:foldl(fun(Mode, Acc) -> Acc + parse_mode(Mode) end, 0, Modes2);

parse_mode(read)  -> 1;
parse_mode(write) -> 2;
parse_mode(emit)  -> 4;
parse_mode(_)     -> erlang:error(badarg).

parse_channel([]) ->
    {ok, <<"/">>};
parse_channel(Path) ->
    {ok, list_to_binary(Path)}.

parse_uri(URI) when is_binary(URI) ->
    parse_uri(binary_to_list(URI));
parse_uri(URI) ->
    case http_uri:parse(URI) of
        {error, {not_supported_scheme , _}} ->
            parse_uri("http://" ++ URI);
        {error, no_scheme} ->
            parse_uri("http://" ++ URI);
        {error, {malformed_url, _, _}} ->
            case string:tokens(URI, ":") of
                Parts when length(Parts) =:= 2 ->
                    parse_uri("http://" ++ URI);
                _ ->
                    {error, invalid_uri}
            end;
        {ok, {Protocol, _Credentials, Host, Port, Path, Q}} ->
            {ok, Channel} = parse_channel(Path),
            Token = clean_token(Q),
            {ok, Protocol, Host, Port, Channel, Token, URI};
        _Other ->
            {error, invalid_uri}
    end.

clean_token([]) -> <<>>;
clean_token([$?|T]) -> list_to_binary(T).

ensure_started(AppName) ->
    case application:start(AppName) of
        ok -> ok;
        {error, {already_started, AppName}} -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
channel_parse_test() ->
    ?assertEqual({ok, <<"/">>}, parse_channel("/")),
    ?assertEqual({ok, <<"/">>}, parse_channel("")),
    ?assertEqual({ok, <<"/1234">>}, parse_channel("/1234")),
    ok.

mode_parse_test() ->
    ?assertEqual(1, parse_mode([read])),
    ?assertEqual(1, parse_mode([read, read])),
    ?assertEqual(3, parse_mode([read, write])),
    ?assertEqual(7, parse_mode([read, write, emit])),
    ok.
        
-endif.
