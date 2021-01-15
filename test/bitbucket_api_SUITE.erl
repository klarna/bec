-module(bitbucket_api_SUITE).

-export([ all/0
        , init_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , end_per_suite/1
        ]).

-export([ get_default_branch/1
        , get_permissions_users/1
        , set_default_branch/1
        , get_wz_branch_reviewers/1
        , get_wz_branch_reviewers_when_none_configured/1
        , get_wz_branch_reviewers_with_mandatory/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROJECT_KEY, "a_project").
-define(REPO_SLUG  , "a_repo").

init_per_suite(Config) ->
  {ok, Started} = application:ensure_all_started(bec),
  ok = meck:new(httpc, [unlink]),
  [{started, Started}|Config].

end_per_suite(Config) ->
  [application:stop(A) || A <- ?config(started, Config)],
  ok.

init_per_testcase(get_default_branch, Config) ->
  {ok, Body} = body(Config, get_default_branch),
  Fun = fun('get', _, _, _) ->
            {ok, {{"1.1", 200, "OK"}, [], Body}}
        end,
  ok = meck:expect(httpc, request, Fun),
  Config;
init_per_testcase(get_permissions_users, Config) ->
  {ok, Body} = body(Config, get_permissions_users),
  Fun = fun('get', _, _, _) ->
            {ok, {{"1.1", 200, "OK"}, [], Body}}
        end,
  ok = meck:expect(httpc, request, Fun),
  Config;
init_per_testcase(set_default_branch, Config) ->
  Fun = fun('put', _, _, _) ->
            {ok, {{"1.1", 200, "OK"}, [], []}}
        end,
  ok = meck:expect(httpc, request, Fun),
  Config;
init_per_testcase(get_wz_branch_reviewers, Config) ->
  {ok, Body} = body(Config, get_wz_branch_reviewers),
  Fun = fun('get', _, _, _) ->
            {ok, {{"1.1", 200, "OK"}, [], Body}}
        end,
  ok = meck:expect(httpc, request, Fun),
  Config;
init_per_testcase(get_wz_branch_reviewers_when_none_configured, Config) ->
  Fun = fun('get', _, _, _) ->
            {ok, {{"1.1", 204, "No Content"}, [], []}}
        end,
  ok = meck:expect(httpc, request, Fun),
  Config;
init_per_testcase(get_wz_branch_reviewers_with_mandatory, Config) ->
  {ok, Body} = body(Config, get_wz_branch_reviewers_with_mandatory),
  Fun = fun('get', _, _, _) ->
            {ok, {{"1.1", 200, "OK"}, [], Body}}
        end,
  ok = meck:expect(httpc, request, Fun),
  Config.

end_per_testcase(_TestCase, _Config) ->
  [httpc] = meck:unload(),
  ok.

all() ->
  [ get_default_branch
  , get_permissions_users
  , set_default_branch
  , get_wz_branch_reviewers
  , get_wz_branch_reviewers_when_none_configured
  , get_wz_branch_reviewers_with_mandatory
  ].

get_default_branch(_Config) ->
  {ok, Branch} = bitbucket:get_default_branch(?PROJECT_KEY, ?REPO_SLUG),
  ?assertEqual(<<"integration">>, Branch).

get_permissions_users(_Config) ->
  ?assertEqual( { ok
                , [ #{permission => 'REPO_ADMIN', username => <<"user.c">>}
                  , #{permission => 'REPO_WRITE', username => <<"user.a">>}
                  , #{permission => 'REPO_WRITE', username => <<"user.b">>}
                  ]}
              , bitbucket:get_users(?PROJECT_KEY, ?REPO_SLUG)).

set_default_branch(_Config) ->
  Result = bitbucket:set_default_branch(?PROJECT_KEY, ?REPO_SLUG, "develop"),
  ?assertEqual(ok, Result).

get_wz_branch_reviewers(_Config) ->
  ExpectedResult =
    [ #{ 'branch-id' => <<"integration">>
       , users       => [<<"user.a">>]
       , groups      => [<<"group.a">>]
       , paths       => [ #{ path => <<"lib/**">>
                           , users => [<<"user.b">>]
                           , groups => []
                           , 'mandatory-users' => []
                           , 'mandatory-groups' => []
                           }
                        , #{ path => <<"another_lib/**">>
                           , users =>  []
                           , groups => [<<"group.b">>]
                           , 'mandatory-users' => []
                           , 'mandatory-groups' => []
                           }
                        ]
       , 'mandatory-users' => []
       , 'mandatory-groups' => []
       }
    ],
  ?assertEqual( { ok
                , ExpectedResult
                }
              , bitbucket:get_wz_branch_reviewers(?PROJECT_KEY, ?REPO_SLUG)).

get_wz_branch_reviewers_when_none_configured(_Config) ->
  ?assertEqual( { ok
                , []
                }
              , bitbucket:get_wz_branch_reviewers(?PROJECT_KEY, ?REPO_SLUG)).

get_wz_branch_reviewers_with_mandatory(_Config) ->
  ExpectedResult =
    [ #{ 'branch-id'         => <<"integration">>
       , users               => [<<"user.a">>]
       , groups              => [<<"group.a">>]
       , paths               => [ #{ path => <<"lib/**">>
                                   , users => [<<"user.b">>]
                                   , groups => []
                                   , 'mandatory-users' => []
                                   , 'mandatory-groups' => []
                                   }
                                , #{ path => <<"another_lib/**">>
                                   , users =>  []
                                   , groups => [<<"group.b">>]
                                   , 'mandatory-users' => []
                                   , 'mandatory-groups' => []
                                   }
                                ]
        , 'mandatory-users'  => [<<"user.a">>]
        , 'mandatory-groups' => [<<"group.a">>]
       }
    ],
  ?assertEqual( { ok
                , ExpectedResult
                }
              , bitbucket:get_wz_branch_reviewers(?PROJECT_KEY, ?REPO_SLUG)).


body(Config, TestCase) ->
  DataDir = ?config(data_dir, Config),
  file:read_file(filename:join([DataDir, atom_to_list(TestCase) ++ ".json"])).
