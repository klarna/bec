%% Test utilities for setting up a Bitbucket instance for test purposes.
-module(bec_test_utils).

-export([ init_bitbucket/0
        , deinit_bitbucket/0
        , init_logging/0
        , is_wz_supported/0
        ]).

-compile([{parse_transform, lager_transform}]).

cmd(Fmt, Args) ->
  Cmd = lists:flatten(io_lib:format(Fmt, Args)),
  %%io:format("Command: ~p~n", [Cmd]),
  _Result = os:cmd(Cmd),
  %%io:format("Command output: ~s~n", [Result]),
  ok.

init_repo() ->
  Url           = os:getenv("BB_STAGING_URL", "http://localhost"),
  Username      = os:getenv("BB_STAGING_USERNAME", ""),
  Password      = os:getenv("BB_STAGING_PASSWORD", ""),

  Map = uri_string:parse(Url),

  ProjectKey = os:getenv("BB_STAGING_PROJECT_KEY", ""),
  RepoSlug = os:getenv("BB_STAGING_REPO_SLUG", ""),

  %% TODO see if we can get this url through the REST api instead of
  %% reconstructing it from the Bitbucket Server url.
  GitUrl = io_lib:format("~s://~s:~s@~s:~w/scm/~s/~s.git",
                         [maps:get(scheme, Map),
                          Username,
                          Password,
                          maps:get(host, Map),
                          maps:get(port, Map),
                          ProjectKey,
                          RepoSlug]),

  LocalRepo = "/tmp/bec-test" ++ integer_to_list(rand:uniform(1 bsl 127)),
  ok = cmd("mkdir -p ~s && cd ~s && "
           "git init . && "
           "git config user.email $USER@$HOSTNAME && "
           "git config user.name Anonymous && "
           "echo >README.md && git add README.md && "
           "git commit -a -m 'First commit' && "
           "git remote add origin ~s && git fetch && "
           "git push origin master && "
           "rm -rf ~s",
           [LocalRepo, LocalRepo, GitUrl, LocalRepo]),

  ok = bitbucket:set_default_branch(ProjectKey, RepoSlug, <<"master">>),
  ok = bitbucket:create_branch(ProjectKey, RepoSlug, <<"feature">>, <<"master">>).

init_bitbucket() ->
  ProjectKey = list_to_binary(os:getenv("BB_STAGING_PROJECT_KEY", "")),
  RepoSlug = list_to_binary(os:getenv("BB_STAGING_REPO_SLUG", "")),
  TeamA = list_to_binary(os:getenv("BB_STAGING_TEAM_A", "team.a")),
  TeamB = list_to_binary(os:getenv("BB_STAGING_TEAM_B", "team.b")),

  UserA = list_to_binary(os:getenv("BB_STAGING_USER_A", "user.a")),
  UserEmailA = list_to_binary(os:getenv("BB_STAGING_USER_A", "user.a") ++ "@email.com"),
  UserDisplayNameA = list_to_binary(os:getenv("BB_STAGING_USER_A", "user.a") ++ " (Name)"),

  UserB = list_to_binary(os:getenv("BB_STAGING_USER_B", "user.b")),
  UserEmailB = list_to_binary(os:getenv("BB_STAGING_USER_B", "user.b") ++ "@email.com"),
  UserDisplayNameB = list_to_binary(os:getenv("BB_STAGING_USER_B", "user.b") ++ " (Name)"),

  deinit_bitbucket(),

  try
    ok = bitbucket:create_project(ProjectKey),
    ok = bitbucket:create_repo(ProjectKey, RepoSlug),
    ok = bitbucket:create_group(TeamA),
    ok = bitbucket:create_group(TeamB),
    ok = bitbucket:create_user(UserA, UserEmailA, UserDisplayNameA, <<"password">>),
    ok = bitbucket:create_user(UserB, UserEmailB, UserDisplayNameB, <<"password">>),
    ok = bitbucket:add_user_to_groups(UserA, [TeamA]),
    ok = bitbucket:add_user_to_groups(UserB, [TeamB]),
    init_repo()
  catch C:T:St ->
      %% If the setup function fails, the teardown function is not
      %% executed and we risk leaving a running bitbucket instance in a
      %% inconsistent state.
      deinit_bitbucket(),
      io:format("Caught exception in setup:~n~p~n", [{C,T,St}]),
      throw(T)
  end.

deinit_bitbucket() ->
  ProjectKey = list_to_binary(os:getenv("BB_STAGING_PROJECT_KEY", "")),
  RepoSlug = list_to_binary(os:getenv("BB_STAGING_REPO_SLUG", "")),
  TeamA = list_to_binary(os:getenv("BB_STAGING_TEAM_A", "team.a")),
  TeamB = list_to_binary(os:getenv("BB_STAGING_TEAM_B", "team.b")),
  UserA = list_to_binary(os:getenv("BB_STAGING_USER_A", "user.a")),
  UserB = list_to_binary(os:getenv("BB_STAGING_USER_B", "user.b")),

  %% Try as best as possible to cleanup the bitbucket instance, and
  %% don't fail if e.g. some user doesn't exist.
  catch bitbucket:remove_user(UserA),
  catch bitbucket:remove_user(UserB),
  catch bitbucket:remove_group(TeamA),
  catch bitbucket:remove_group(TeamB),
  catch bitbucket:delete_repo(ProjectKey, RepoSlug),
  catch bitbucket:delete_project(ProjectKey).

init_logging() ->
  lager:start(),
  lager:set_loglevel(lager_file_backend, debug),
  lager:set_loglevel(lager_console_backend, none),
  ok.

is_wz_supported() ->
  try
    ok = bitbucket:get_wz_branch_reviewers(<<"TOOLS">>, <<"bec-test">>),
    true
  catch _:_ ->
      lager:error("Workzone plugin is not supported. Corresponding tests will not be run."),
      false
  end.
