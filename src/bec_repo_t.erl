%%==============================================================================
%% Type definition for the Repo data structure
%%==============================================================================
-module(bec_repo_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type repo() :: #{ public => boolean(),
                   clone_ssh => binary(),
                   clone_http => binary()
                 }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ repo/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> repo().
from_map(#{ <<"public">> := Public
          , <<"links">> := Links
          }) ->
  CloneMethods =
    lists:foldl(fun(#{<<"href">> := Href,
                      <<"name">> := Protocol}, Acc) ->
                    maps:put(Protocol, Href, Acc)
                end, #{}, maps:get(<<"clone">>, Links)),

  #{ public => Public,
     clone_ssh => maps:get(<<"ssh">>, CloneMethods),
     clone_http => maps:get(<<"http">>, CloneMethods)
   }.

-spec to_map(repo()) -> map().
to_map(#{ public := Public}) ->
  %% No support for setting clone methods yet
  #{<<"public">> => Public}.
