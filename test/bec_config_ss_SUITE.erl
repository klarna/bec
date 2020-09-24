%%==============================================================================
%% Test Suite for config syntactic sugar
%%==============================================================================

-module(bec_config_ss_SUITE).

%% Common Test Callbacks
-export([ all/0 ]).

%% Testcases
-export([ to_wz_branch_reviewers_adds_default_mandatory/1 
        , to_wz_branch_reviewers_keeps_original_mandatory/1
        ]).

%%==============================================================================
%% Include
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
  [ to_wz_branch_reviewers_adds_default_mandatory
  , to_wz_branch_reviewers_keeps_original_mandatory
  ].

to_wz_branch_reviewers_adds_default_mandatory(_Config) ->
  Reviewers = 
    [ #{ <<"branch-id">> => <<"master">>
       , <<"groups">>    => []
       , <<"paths">>     => []
       , <<"users">>     => []
       }
    ],
  ExpectedResult =
    [ #{ 'branch-id'        => <<"master">>
       , 'groups'           => []
       , 'paths'            => []
       , 'users'            => []
       , 'mandatory-users'  => []
       , 'mandatory-groups' => []
       }
      ],
  ?assertEqual(ExpectedResult, bec_config_ss:to_wz_branch_reviewers(Reviewers)).

to_wz_branch_reviewers_keeps_original_mandatory(_Config) ->
  Reviewers = 
    [ #{ <<"branch-id">>        => <<"master">>
       , <<"groups">>           => []
       , <<"paths">>            => []
       , <<"users">>            => []
       , <<"mandatory-users">>  => [ <<"user.a">> ]
       , <<"mandatory-groups">> => [ <<"group.a">> ]
       }
    ],
  ExpectedResult =
    [ #{ 'branch-id'        => <<"master">>
       , 'groups'           => []
       , 'paths'            => []
       , 'users'            => []
       , 'mandatory-users'  => [ <<"user.a">> ]
       , 'mandatory-groups' => [ <<"group.a">> ]
       }
      ],
  ?assertEqual(ExpectedResult, bec_config_ss:to_wz_branch_reviewers(Reviewers)).
