%%==============================================================================
%% Test Suite for config syntactic sugar
%%==============================================================================

-module(bec_config_ss_SUITE).

%% Common Test Callbacks
-export([ all/0 ]).

%% Testcases
-export([ to_wz_branch_reviewers_adds_default_mandatory/1 
        , to_wz_branch_reviewers_keeps_original_mandatory/1
        , to_wz_branch_reviewers_adds_default_mandatory_in_path/1
        , to_wz_branch_reviewers_ordered_keys/1
        ]).

%%==============================================================================
%% Include
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
  [ to_wz_branch_reviewers_adds_default_mandatory
  , to_wz_branch_reviewers_keeps_original_mandatory
  , to_wz_branch_reviewers_adds_default_mandatory_in_path
  , to_wz_branch_reviewers_ordered_keys
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

to_wz_branch_reviewers_adds_default_mandatory_in_path(_Config) ->
  Reviewers =
    [ #{ <<"branch-id">> => <<"master">>
       , <<"groups">>    => []
       , <<"paths">>     => [#{ <<"file-path-pattern">> => []
                              , <<"users">> => []
                              , <<"groups">> => []
                            }]
       , <<"users">>     => []
       }
    ],
  ExpectedResult =
    [ #{ 'branch-id'        => <<"master">>
       , 'groups'           => []
       , 'paths'            => [#{ 'file-path-pattern' => []
                                 , 'users' => []
                                 , 'groups' => []
                                 , 'mandatory-users' => []
                                 , 'mandatory-groups' => []
                                }
                               ]
       , 'users'            => []
       , 'mandatory-users'  => []
       , 'mandatory-groups' => []
       }
      ],
  ?assertEqual(ExpectedResult, bec_config_ss:to_wz_branch_reviewers(Reviewers)).

  to_wz_branch_reviewers_ordered_keys(_Config) ->
    Reviewers =
      [ #{ <<"branch-id">> => <<"master">>
         , <<"groups">>    => [ <<"group.b">>, <<"group.a">> ]
         , <<"paths">>     => [#{ <<"file-path-pattern">> => [ <<"path.b">>, <<"path.a">> ]
                                , <<"users">> => [ <<"user.d">>, <<"user.c">> ]
                                , <<"groups">> => [ <<"group.d">>, <<"group.c">> ]
                                , <<"mandatory-users">>  => [ <<"user.f">>, <<"user.e">> ]
                                , <<"mandatory-groups">> => [ <<"group.f">>, <<"group.e">> ]
                              }]
         , <<"users">>     => [ <<"user.b">>, <<"user.a">> ]
         , <<"mandatory-users">>  => [ <<"user.h">>, <<"user.g">> ]
         , <<"mandatory-groups">> => [ <<"group.h">>, <<"group.g">> ]
         }
      ],
    ExpectedResult =
      [ #{ 'branch-id'        => <<"master">>
         , 'groups'           => [ <<"group.a">>, <<"group.b">> ]
         , 'paths'            => [#{ 'file-path-pattern' => [ <<"path.a">>, <<"path.b">> ]
                                   , 'users' => [ <<"user.c">>, <<"user.d">> ]
                                   , 'groups' => [ <<"group.c">>, <<"group.d">> ]
                                   , 'mandatory-users' => [ <<"user.e">>, <<"user.f">> ]
                                   , 'mandatory-groups' => [ <<"group.e">>, <<"group.f">> ]
                                   }]
         , 'users'            => [ <<"user.a">>, <<"user.b">> ]
         , 'mandatory-users'  => [ <<"user.g">>, <<"user.h">> ]
         , 'mandatory-groups' => [ <<"group.g">>, <<"group.h">> ]
         }
        ],
    ?assertEqual(ExpectedResult, bec_config_ss:to_wz_branch_reviewers(Reviewers)).