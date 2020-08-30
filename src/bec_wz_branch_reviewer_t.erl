%%==============================================================================
%% Type definition for the Workzone Branch Reviewer data structure
%%==============================================================================
-module(bec_wz_branch_reviewer_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type reviewer() :: #{ 'branch-id'        := bec_branch_t:id()
                     , users              := [bec_wz_user_t:name()]
                     , groups             := [bec_group_t:name()]
                     , paths              := [bec_wz_path_t:path()]
                     , 'mandatory-users'  := [bec_wz_user_t:name()]
                     , 'mandatory-groups' := [bec_group_t:name()]
                     }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ reviewer/0 ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> reviewer().
from_map(#{ <<"refName">>           := RefName
          , <<"users">>             := Users
          , <<"groups">>            := Groups
          } = Map) ->
  %% filePathReviewers are not always present in the response from BitBucket
  FPR = maps:get(<<"filePathReviewers">>, Map, []),
  MUsers = maps:get(<<"mandatoryUsers">>, Map, []),
  MGroups = maps:get(<<"mandatoryGroups">>, Map, []),
  #{ 'branch-id'        => bec_wz_utils:strip_prefix(RefName)
   , users              => lists:sort([bec_wz_user_t:from_map(U) || U <- Users])
   , groups             => lists:sort(Groups)
   , paths              => lists:sort([bec_wz_path_t:from_map(P) || P <- FPR])
   , 'mandatory-users'  => lists:sort([bec_wz_user_t:from_map(U)
                                        || U <- MUsers])
   , 'mandatory-groups' => lists:sort(MGroups)
   }.

-spec to_map(reviewer()) -> map().
to_map(#{ 'branch-id'             := BranchId
        , users                   := Users
        , groups                  := Groups
        , paths                   := Paths
        } = Map) ->
  MUsers = maps:get('mandatory-users', Map, []),
  MGroups = maps:get('mandatory-groups', Map, []),
  #{ <<"refName">>               => bec_wz_utils:add_prefix(BranchId)
   , <<"users">>                 => [bec_wz_user_t:to_map(U) || U <- Users]
   , <<"groups">>                => Groups
   , <<"filePathReviewers">>     => [bec_wz_path_t:to_map(P) || P <- Paths]
   , <<"mandatoryUsers">>        => [bec_wz_user_t:to_map(U) || U <- MUsers]
   , <<"mandatoryGroups">>       => MGroups
   , <<"topSuggestedReviewers">> => 0
   , <<"daysInPast">>            => 0
   }.