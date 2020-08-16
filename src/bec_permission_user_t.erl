%%==============================================================================
%% Type definition for the Permission User structure
%%==============================================================================
-module(bec_permission_user_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1
        , to_map/1
        ]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type permission_type() :: 'REPO_ADMIN' | 'REPO_READ' | 'REPO_WRITE'.
-type permission()      :: #{ username   => bec_user_t:name()
                            , permission => permission_type()
                            }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ permission/0
             , permission_type/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> permission().
from_map(#{ <<"permission">> := Permission
          , <<"user">>       := User
          }) ->
  #{ permission => binary_to_atom(Permission, utf8)
   , username   => bec_user_t:from_map(User)
   }.

-spec to_map(permission()) -> map().
to_map(#{ username   := Username
        , permission := Permission}) ->
  #{ <<"user">>       => bec_user_t:to_map(Username)
   , <<"permission">> => Permission
   }.
