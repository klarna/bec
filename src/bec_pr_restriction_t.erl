%%==============================================================================
%% Type definition for the Pull Request Restriction structure
%%==============================================================================
-module(bec_pr_restriction_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type restriction() :: #{ 'merge-config' := bec_merge_config_t:config()
                        , 'required-all-approvers'      := boolean()
                        , 'required-all-tasks-complete' := boolean()
                        , 'required-approvers'          := integer()
                        , 'required-successful-builds'  := boolean()
                        }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ restriction/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> restriction().
from_map(#{ <<"mergeConfig">>              := MergeConfig
          , <<"requiredAllApprovers">>     := RequiredAllApprovers
          , <<"requiredAllTasksComplete">> := RequiredAllTasksComplete
          , <<"requiredApprovers">>        := RequiresApprovers
          , <<"requiredSuccessfulBuilds">> := RequiredSuccessfulBuilds
          }) ->
  #{ 'merge-config'                => bec_merge_config_t:from_map(MergeConfig)
   , 'required-all-approvers'      => RequiredAllApprovers
   , 'required-all-tasks-complete' => RequiredAllTasksComplete
   , 'required-approvers'          => RequiresApprovers
   , 'required-successful-builds'  => RequiredSuccessfulBuilds
   }.

-spec to_map(restriction()) -> map().
to_map(#{ 'merge-config'                := MergeConfig
        , 'required-all-approvers'      := RequiredAllApprovers
        , 'required-all-tasks-complete' := RequiredAllTasksComplete
        , 'required-approvers'          := RequiresApprovers
        , 'required-successful-builds'  := RequiredSuccessfulBuilds
        }) ->
  #{ <<"mergeConfig">>              => bec_merge_config_t:to_map(MergeConfig)
   , <<"requiredAllApprovers">>     => RequiredAllApprovers
   , <<"requiredAllTasksComplete">> => RequiredAllTasksComplete
   , <<"requiredApprovers">>        => RequiresApprovers
   , <<"requiredSuccessfulBuilds">> => RequiredSuccessfulBuilds
   }.
