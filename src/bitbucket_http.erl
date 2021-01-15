%%==============================================================================
%% Low-level HTTP API
%%==============================================================================
-module(bitbucket_http).

-compile([{parse_transform, lager_transform}]).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ delete_request/1
        , delete_request/2
        , get_request/1
        , post_request/2
        , put_request/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type method()        :: 'get' | 'post' | 'put' | 'delete'.
-type url()           :: string().
-type http_version()  :: string().
-type status_code()   :: integer().
-type reason_phrase() :: string().
-type status_line()   :: {http_version(), status_code(), reason_phrase()}.
-type header()        :: {string(), string()}.
-type body()          :: binary().
-type httpc_result()  :: {'ok', {status_line(), [header()], body()}}
                       | {'error', term()}.
-type request()       :: {url(), [header()], string(), body()}
                       | {url(), [header()]}.

%%==============================================================================
%% DELETE
%%==============================================================================
-spec delete_request(url()) -> {ok, map()} | {error, any()}.
delete_request(Url) ->
  do_request('delete', Url, <<"{}">>).

-spec delete_request(url(), body()) -> {ok, map()} | {error, any()}.
delete_request(Url, Body) ->
  do_request('delete', Url, Body).

%%==============================================================================
%% GET
%%==============================================================================
-spec get_request(url()) -> {ok, map()} | {ok, [map()]} | {error, any()}.
get_request(Url) ->
  do_request('get', Url).

%%==============================================================================
%% POST
%%==============================================================================
-spec post_request(url(), binary()) -> {ok, map()} | {error, any()}.
post_request(Url, Body) ->
  do_request('post', Url, Body).

%%==============================================================================
%% PUT
%%==============================================================================
-spec put_request(url(), binary()) -> {ok, map()} | {error, any()}.
put_request(Url, Body) ->
  do_request('put', Url, Body).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec do_request(method(), url()) ->
        {ok, map()} | {ok, [map()]} | {error, any()}.
do_request(Method, Url) ->
  Headers = headers(),
  Request = {Url, Headers},
  ok      = lager:debug("HTTP Request: (~p) ~p~n", [Method, Url]),
  do_http_request(Method, Request).

-spec do_request(method(), url(), body()) -> {ok, map()} | {error, any()}.
do_request(Method, Url, Body) ->
  Headers = headers(),
  Type    = "application/json",
  Request = {Url, Headers, Type, Body},
  ok      = lager:debug("HTTP Request: (~p) ~p~n~p~n", [Method, Url, Headers]),
  do_http_request(Method, Request).

-spec do_http_request(method(), request()) ->
        {ok, map()} | {ok, [map()]} | {error, any()}.
do_http_request(Method, Request) ->
  HTTPOptions = [{autoredirect, true}],
  Options     = [],
  %% Disable pipelining to avoid the socket getting closed during long runs
  ok          = httpc:set_options([ {max_keep_alive_length, 0}
                                  , {max_pipeline_length, 0}
                                  , {max_sessions, 0}
                                  ]),
  Result      = httpc:request(Method, Request, HTTPOptions, Options),
  ok          = lager:debug("HTTP Result: ~p~n", [Result]),
  handle_result(Result).

-spec headers() -> [{string(), string()}].
headers() ->
  Username = application:get_env(bec, bitbucket_username, ""),
  Password = application:get_env(bec, bitbucket_password, ""),
  Credentials = base64:encode_to_string(Username ++ ":" ++ Password),
  [
    {"Authorization", "Basic " ++ Credentials}
  , {"Accept",        "application/json"}
  ].

-spec handle_result(httpc_result()) ->
        {ok, map()} | {ok, [map()]} | {error, any()}.
handle_result({ok, {{_Ver, Status, _Phrase}, _H, Body}}) when Status =:= 200;
                                                              Status =:= 201;
                                                              Status =:= 204 ->
  {ok, decode_body(Body)};
handle_result({ok, {{_Version, _Status, _Phrase}, _Headers, Resp}}) ->
  {error, decode_error(Resp)};
handle_result({error, Reason}) ->
  {error, Reason}.

-spec decode_body(string()) -> map() | [map()].
decode_body([])    ->
  #{};
decode_body(Body0) ->
  try jsx:decode(unicode:characters_to_binary(Body0), [return_maps])
  catch _:_ ->
      #{<<"errors">> =>
        [ #{ <<"message">> => <<"Could not parse response">> }
        , #{ <<"message">> => unicode:characters_to_binary(Body0) }
        ]}
  end.

-spec decode_error(string()) -> [binary()].
decode_error(Body) ->
  Errors = maps:get(<<"errors">>, decode_body(Body)),
  [M || #{<<"message">> := M} <- Errors].
