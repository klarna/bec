%%==============================================================================
%% Low-level HTTP API
%%==============================================================================
-module(bitbucket_http).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ delete_request/1
        , delete_request/2
        , get_request/1
        , post_request/2
        , put_request/2
        ]).

-include_lib("kernel/include/logger.hrl").

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

-type retry_state()   :: #{n := non_neg_integer(),
                           base_sleep_time := pos_integer(),
                           cap_sleep_time := pos_integer()}.

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
  ok      = ?LOG_DEBUG("HTTP Request: (~p) ~p~n", [Method, Url]),
  do_http_request(Method, Request, default_retry_state()).

-spec do_request(method(), url(), body()) -> {ok, map()} | {error, any()}.
do_request(Method, Url, Body) ->
  Headers = headers(),
  Type    = "application/json",
  Request = {Url, Headers, Type, Body},
  ok      = ?LOG_DEBUG("HTTP Request: (~p) ~p~n~p~n",
                       [Method, Url, Headers]),
  do_http_request(Method, Request, default_retry_state()).

-spec do_http_request(method(), request(), retry_state()) ->
        {ok, map()} | {ok, [map()]} | {error, any()}.
do_http_request(Method, Request, RetryState) ->
  HTTPOptions = [{autoredirect, true}],
  Options     = [],
  %% Disable pipelining to avoid the socket getting closed during long runs
  ok          = httpc:set_options([ {max_keep_alive_length, 0}
                                  , {max_pipeline_length, 0}
                                  , {max_sessions, 0}
                                  ]),
  Result      = httpc:request(Method, Request, HTTPOptions, Options),
  ok          = ?LOG_DEBUG("HTTP Result: ~p~n", [Result]),
  case handle_result(Result) of
    {error, retry} ->
      ?LOG_DEBUG("Request was rate-limited, retrying...", []),
      {ok, RetryState0} = should_retry(RetryState),
      do_http_request(Method, Request, RetryState0);
    Other ->
      Other
  end.

-spec headers() -> [{string(), string()}].
headers() ->
  [ {"Authorization", authorization()}
  , {"Accept", "application/json"}
  ].

-spec authorization() -> string().
authorization() ->
  case application:get_env(bec, bitbucket_token, "") of
    "" ->
      Username = application:get_env(bec, bitbucket_username, ""),
      Password = application:get_env(bec, bitbucket_password, ""),
      "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password);
    Token ->
      "Bearer " ++ Token
  end.

-spec handle_result(httpc_result()) ->
        {ok, map()} | {ok, [map()]} | {error, any()}.
handle_result({ok, {{_Ver, Status, _Phrase}, _H, Body}}) when Status =:= 200;
                                                              Status =:= 201;
                                                              Status =:= 202;
                                                              Status =:= 204 ->
  {ok, decode_body(Body)};
handle_result({ok, {{_Ver, Status, _Phrase}, _H, _Body}}) when Status =:= 429 ->
  {error, retry};
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

-spec default_retry_state() -> retry_state().
default_retry_state() ->
  {ok, BaseSleepTime} = application:get_env(bec, base_sleep_time),
  {ok, CapSleepTime} = application:get_env(bec, cap_sleep_time),
  #{n => 0,
    base_sleep_time => BaseSleepTime,
    cap_sleep_time => CapSleepTime}.

-spec should_retry(RetryState :: retry_state()) -> {ok, retry_state()}.
should_retry(#{ n := N
              , base_sleep_time := BaseSleepTime
              , cap_sleep_time := CapSleepTime} = RetryState0) ->
  Sleep = calculate_sleep_time(N, BaseSleepTime, CapSleepTime),
  ?LOG_DEBUG("Sleep-time: ~w ms", [Sleep]),
  timer:sleep(Sleep),
  {ok, RetryState0#{ n => N +1 }}.

calculate_sleep_time(N, BaseSleepTime, CapSleepTime) ->
  Temp = min(CapSleepTime, BaseSleepTime bsl N),
  Temp div 2 + rand:uniform(Temp div 2).
