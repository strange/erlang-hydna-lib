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

open(URI, RawMode, HandlerMod) ->
    open(URI, RawMode, HandlerMod, []).

open(URI, RawMode, HandlerMod, Opts) ->
    case {parse_uri(URI), parse_mode(RawMode)} of
        {{ok, _Protocol, Domain, Port, Path, Token, _URI2}, {ok, Mode}} ->
            hydna_lib_proxy:open(Domain, Port, Path, Mode, Token,
                                 HandlerMod, Opts);
        Other ->
            error_tuple(Other)
    end.

send(URI, Message) ->
    case parse_uri(URI) of
        {ok, _Protocol, _Domain, _Port, _Chan, _Token, URI2} ->
            hydna_lib_push:send(URI2, Message);
        Other ->
            error_tuple(Other)
    end.

emit(URI, Message) ->
    case parse_uri(URI) of
        {ok, _Protocol, _Domain, _Port, _Chan, _Token, URI2} ->
            hydna_lib_push:emit(URI2, Message);
        Other ->
            error_tuple(Other)
    end.

%% Internal API

error_tuple({{error, Reason}, _}) -> {error, Reason};
error_tuple({_, {error, Reason}}) -> {error, Reason}.

parse_mode(Mode) when is_list(Mode) ->
    parse_mode(list_to_binary(Mode));
parse_mode(<<"r">>)   -> {ok, 1};
parse_mode(<<"w">>)   -> {ok, 2};
parse_mode(<<"rw">>)  -> {ok, 3};
parse_mode(<<"e">>)   -> {ok, 4};
parse_mode(<<"er">>)  -> {ok, 5};
parse_mode(<<"erw">>) -> {ok, 7};
parse_mode(<<>>)      -> {ok, 0};
parse_mode(_)         -> {error, invalid_mode}. 

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
            case parse_channel(Path) of
                {ok, Channel} ->
                    {ok, Protocol, Host, Port, Channel, clean_token(Q), URI};
                Other ->
                    Other
            end;
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
        
-endif.
