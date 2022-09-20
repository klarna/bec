%% Test utilities for setting up a Bitbucket instance for test purposes.
-module(bec_test_utils).

-export([ init_bitbucket/0
        , init_logging/0
        , flush_logging/0
        , is_wz_supported/0

        , bitbucket_server_url/0
        , bitbucket_project_key/0
        , bitbucket_repo_slug/0
        , bitbucket_test_users/0
        , bitbucket_test_groups/0
        , bitbucket_username/0
        , bitbucket_password/0
        , bitbucket_set_credentials/0
        ]).

-include_lib("kernel/include/logger.hrl").

-spec cmd(Fmt :: io:format(), Args :: [any()]) -> string().
cmd(Fmt, Args) ->
  Cmd = lists:flatten(io_lib:format(Fmt, Args)),
  ?LOG_DEBUG("Executing shell command: ~s~n", [Cmd]),
  Output = os:cmd(Cmd),
  ?LOG_DEBUG("Shell command output: ~s~n", [Output]),
  Output.

init_repo() ->
  ProjectKey = bitbucket_project_key(),
  RepoSlug = bitbucket_repo_slug(),
  {ok, #{clone_http := GitUrl}} = bitbucket:get_repo(ProjectKey, RepoSlug),
  Map = uri_string:parse(GitUrl),
  GitUrlWithAuth =
    uri_string:recompose(maps:merge(#{userinfo =>
                                        bitbucket_username() ++ ":" ++
                                                   bitbucket_password()},
                                    Map)),
  LocalRepo = "/tmp/bec-test" ++ integer_to_list(rand:uniform(1 bsl 127)),

  case bitbucket:branch_exists(ProjectKey, RepoSlug, <<"master">>) of
    true -> ok;
    false ->
      Output = cmd("mkdir -p ~s && cd ~s && "
                   "git init . && "
                   "git config user.email $USER@$HOSTNAME && "
                   "git config user.name Anonymous && "
                   "echo >README.md && git add README.md && "
                   "git commit -a -m 'First commit' && "
                   "git remote add origin ~s && git fetch && "
                   "git push origin master && "
                   "rm -rf ~s && echo SUCCESS",
                   [LocalRepo, LocalRepo, GitUrlWithAuth, LocalRepo]),
      case string:find(Output, "SUCCESS", trailing) of
        nomatch ->
          throw({could_not_initialize_git_repo, Output});
        _ ->
          ok = bitbucket:set_default_branch(ProjectKey, RepoSlug, <<"master">>)
      end
  end,
  maybe_create_branch(ProjectKey, RepoSlug, <<"feature">>, <<"master">>).


init_bitbucket() ->
  bitbucket_set_credentials(),

  ProjectKey = bitbucket_project_key(),
  RepoSlug = bitbucket_repo_slug(),

  %% This is just to get a better error message in case the license
  %% has expired.
  ok = bitbucket:validate_license(),

  maybe_create_project(ProjectKey),
  maybe_create_repo(ProjectKey, RepoSlug),

  lists:foreach(fun maybe_create_group/1, bitbucket_test_groups()),
  lists:foreach(fun({User, Group}) ->
                    UserEmail = <<User/binary, "@email.com">>,
                    UserDisplayName = <<User/binary, " (Name)">>,
                    Password = <<"password">>,
                    maybe_create_user_with_group(User,
                                                 UserEmail,
                                                 UserDisplayName,
                                                 Password,
                                                 Group)
                end, lists:zip(bitbucket_test_users(),
                               bitbucket_test_groups())),

  case is_wz_supported() of
    false ->
      io:format("Workzone plugin is not supported. Corresponding tests will not be run.~n");
    true ->
      ok
  end,

  lists:foreach(
    fun(Hook) ->
        io:format("Hook '~s' is not available and will not be tested.~n", [Hook])
    end, bec_proper_gen:unavailable_hooks()),

  init_repo().

maybe_create_project(ProjectKey) ->
  try
    ok = bitbucket:create_project(ProjectKey)
  catch error:E ->
      ?LOG_DEBUG("Ignoring: ~p", [E])
  end.

maybe_create_repo(ProjectKey, RepoSlug) ->
  try
    ok = bitbucket:create_repo(ProjectKey, RepoSlug)
  catch error:E ->
      ?LOG_DEBUG("Ignoring: ~p", [E])
  end.

maybe_create_group(Group) ->
  try
    ok = bitbucket:create_group(Group)
  catch error:E ->
      ?LOG_DEBUG("Ignoring: ~p", [E])
  end.

maybe_create_user_with_group(User, Email, DisplayName, Password, Group) ->
  try
    bitbucket:create_user(User, Email, DisplayName, Password),
    bitbucket:add_user_to_groups(User, Group)
  catch error:E ->
      ?LOG_DEBUG("Ignoring: ~p", [E])
  end.

maybe_create_branch(ProjectKey, RepoSlug, BranchName, StartPoint) ->
  try
    bitbucket:create_branch(ProjectKey, RepoSlug, BranchName, StartPoint)
  catch error:E ->
      ?LOG_DEBUG("Ignoring: ~p", [E])
  end.

init_logging() ->
  ok = logger:set_primary_config(level, all),
  ok = logger:set_handler_config(default, level, critical),
  ok = logger:update_formatter_config(
         default,
         #{ single_line => true
          , legacy_header => false
          }),
  maybe_add_file_handler().

maybe_add_file_handler() ->
  FileHandlerId = file_handler,
  case lists:member(FileHandlerId, logger:get_handler_ids()) of
    true -> ok;
    false ->
      Filename = "log/bec.log",
      io:format("Full BEC output sent to ~s~n", [Filename]),
      ok = logger:add_handler(FileHandlerId, logger_std_h,
                              #{config => #{file => Filename},
                                level => all})
  end.

flush_logging() ->
  logger_std_h:filesync(default),
  logger_std_h:filesync(file_handler).

is_wz_supported() ->
  try
    {ok, _} = bitbucket:get_wz_branch_reviewers(<<"TOOLS">>, <<"bec-test">>),
    true
  catch _:_ ->
      ?LOG_ERROR("Workzone plugin is not supported. Corresponding tests will not be run."),
      false
  end.

-spec bitbucket_server_url() -> binary().
bitbucket_server_url() ->
  %% Match this with the URL set up by the pre-hook, so that we can
  %% run "rebar3 proper" without having to set BITBUCKET_SERVER_URL
  %% explicitly.
  os:getenv("BITBUCKET_SERVER_URL", "http://localhost:7990").

-spec bitbucket_username() -> string().
bitbucket_username() ->
  os:getenv("BITBUCKET_USERNAME", "admin").

-spec bitbucket_password() -> string().
bitbucket_password() ->
  os:getenv("BITBUCKET_PASSWORD", "admin").

-spec bitbucket_project_key() -> binary().
bitbucket_project_key() ->
  list_to_binary(os:getenv("BITBUCKET_PROJECT_KEY", "TOOLS")).

-spec bitbucket_repo_slug() -> binary().
bitbucket_repo_slug() ->
  list_to_binary(os:getenv("BITBUCKET_REPO_SLUG", "bec")).

-spec bitbucket_test_users() -> [binary()].
bitbucket_test_users() ->
  binary:split(list_to_binary(
                 os:getenv("BITBUCKET_TEST_USERS",
                           "user.a,user.b")),
               <<",">>, [global]).

-spec bitbucket_test_groups() -> [binary()].
bitbucket_test_groups() ->
  binary:split(list_to_binary(
                 os:getenv("BITBUCKET_TEST_GROUPS",
                           "group.a,group.b")),
               <<",">>, [global]).

%% Write the credentials received from os:getenv() through the
%% functions above into the application environment so that e.g. code
%% in bitbucket_http can pick it up just as if the user had specified
%% "-c ..." on the command line.
-spec bitbucket_set_credentials() -> ok.
bitbucket_set_credentials() ->
  application:set_env(bec, bitbucket_url,      bitbucket_server_url()),
  application:set_env(bec, bitbucket_username, bitbucket_username()),
  application:set_env(bec, bitbucket_password, bitbucket_password()),
  ok.
