%%==============================================================================
%% Type definition for the Permission Group structure
%%==============================================================================
-module(bec_permission_group_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_config/1
        , from_map/1
        , to_map/1
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type permission_type() :: 'REPO_ADMIN' | 'REPO_READ' | 'REPO_WRITE'.
-type permission()      :: #{ groupname  => bec_group_t:name()
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
-spec from_config(map()) -> permission().
from_config(#{ <<"permission">> := Permission
             , <<"groupname">>  := Groupname
             }) ->
  #{ permission => binary_to_atom(Permission, utf8)
   , groupname   => Groupname
   }.

-spec from_map(map()) -> permission().
from_map(#{ <<"permission">> := Permission
          , <<"group">>      := Group
          }) ->
  #{ permission => binary_to_atom(Permission, utf8)
   , groupname  => bec_group_t:from_map(Group)
   }.

-spec to_map(permission()) -> map().
to_map(#{ groupname  := Groupname
        , permission := Permission}) ->
  #{ <<"group">>      => bec_group_t:to_map(Groupname)
   , <<"permission">> => Permission
   }.
