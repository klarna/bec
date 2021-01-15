%%==============================================================================
%% Type definition for the Workzone Path data structure
%%==============================================================================
-module(bec_wz_path_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type path() :: #{ path   := binary()
                 , users  := [bec_user_t:name()]
                 , groups := [bec_group_t:name()]
                 , 'mandatory-users'  := [bec_wz_user_t:name()]
                 , 'mandatory-groups' := [bec_group_t:name()]
                 }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ path/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> path().
from_map(Map) ->
  #{ <<"filePathPattern">> := Path
   , <<"users">>           := Users
   , <<"groups">>          := Groups
   } = Map,

  MUsers = maps:get(<<"mandatoryUsers">>, Map, []),
  MGroups = maps:get(<<"mandatoryGroups">>, Map, []),

  #{ path   => Path
   , users  => lists:sort([bec_wz_user_t:from_map(U) || U <- Users])
   , groups => lists:sort(Groups)
   , 'mandatory-users' => lists:sort([bec_wz_path_t:from_map(U) || U <- MUsers])
   , 'mandatory-groups' => lists:sort(MGroups)
   }.

-spec to_map(path()) -> map().
to_map(#{ path   := Path
        , users  := Users
        , groups := Groups
        } = Map) ->
  MUsers = maps:get('mandatory-users', Map, []),
  MGroups = maps:get('mandatory-groups', Map, []),

  #{ <<"filePathPattern">> => Path
   , <<"users">>           => [bec_wz_user_t:to_map(U) || U <- Users]
   , <<"groups">>          => Groups
   , <<"mandatoryUsers">> => [bec_wz_path_t:to_map(U) || U <- MUsers]
   , <<"mandatoryGroups">> => MGroups
   }.
