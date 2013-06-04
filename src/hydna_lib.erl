-module(hydna_lib).

-export([start/0]).
-export([open/3]).

start() ->
    ensure_started(lager),
    ensure_started(inets),
    application:start(hydna_lib),
    ok.

open(URI, RawMode, HandlerMod) ->
    case {parse_uri(URI), parse_mode(RawMode)} of
        {{ok, Domain, Port, Chan, Token}, {ok, Mode}} ->
            hydna_lib_proxy:open(Domain, Port, Chan, Mode, Token, HandlerMod);
        Other ->
            Other
    end.

send(URI, Message) ->
    ok.

emit(URI, Message) ->
    ok.

%% Internal API

parse_mode(Mode) when is_list(Mode) ->
    parse_mode(list_to_binary(Mode));
parse_mode(<<"r">>) ->
    {ok, 1};
parse_mode(<<"w">>) ->
    {ok, 2};
parse_mode(<<"rw">>) ->
    {ok, 3};
parse_mode(<<"e">>) ->
    {ok, 4};
parse_mode(<<"er">>) ->
    {ok, 5};
parse_mode(<<"erw">>) ->
    {ok, 7};
parse_mode(<<>>) ->
    {ok, 0};
parse_mode(_) ->
    {error, invalid_mode}. 

parse_channel(Path) ->
    case string:tokens(Path, "/") of
        [] ->
            {ok, 1};
        [Token] ->
            try list_to_integer(Token) of
                Channel ->
                    {ok, Channel}
            catch
                error:badarg ->
                    {error, invalid_channel}
            end;
        _ ->
            {error, invalid_channel}
    end.

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
                Tokens when length(Tokens) =:= 2 ->
                    parse_uri("http://" ++ URI);
                _ ->
                    {error, invalid_uri}
            end;
        {ok, {_Protocol, _Credentials, Host, Port, Path, Q}} ->
            case parse_channel(Path) of
                {ok, Channel} ->
                    {ok, Host, Port, Channel, list_to_binary(Q)};
                Other ->
                    Other
            end;
        _Other ->
            {error, invalid_uri}
    end.

ensure_started(AppName) ->
    case application:start(AppName) of
        ok -> ok;
        {error, {already_started, AppName}} -> ok
    end.
