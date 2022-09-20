-module(bitbucket_repo_config).

-include_lib("kernel/include/logger.hrl").

-export([ verify/1
        , verify/2
        ]).

-type opts() :: [{atom(), any()}].

-spec verify(string()) -> boolean().
verify(Path) ->
  verify(Path, []).

-spec verify(string(), opts()) -> boolean().
verify(Path, Options) ->
  case filename:extension(Path) of
    ".yaml" ->
      Config = read(Path),
      do_verify(Options, Config);
    ".yml"  ->
      Config = read(Path),
      do_verify(Options, Config);
    ".ymlt" ->
      Dirname  = filename:dirname(Path),
      Basename = filename:basename(Path, ".ymlt"),
      VarsPath = filename:join([Dirname, Basename ++ ".ymlv"]),
      Vars = read_vars(VarsPath),
      All = [begin
               [Config] = read_template(Path, Var),
               do_verify(Options, Config)
             end || Var <- Vars],
      lists:all(fun(true) -> true; (false) -> false end, All);
    Ext ->
      ?LOG_ERROR("File extension not recognized (~p).~n", [Ext]),
      false
  end.

-spec read(string()) -> map().
read(Path) ->
  [Config] = bec_yml:decode_file(Path),
  Config.

-spec read_vars(string()) -> [[{atom(), any()}]].
read_vars(VarsPath) ->
  case bec_yml:decode_file(VarsPath) of
    [#{<<"repo">> := Values}] ->
      %% Simple template: contains a list of `repo' values only
      [[{repo, binary_to_list(V)}] || V <- Values];
    [Vars] ->
      %% Complex template: contains a list of maps
      lists:map(
        fun (Map) ->
            maps:fold(
              fun (K, V, L) ->
                  [{binary_to_atom(K, unicode), bec_yml:to_mustache(V)} | L]
              end,
              [],
              Map)
        end,
        Vars)
  end.

-spec read_template(string(), [{atom(), any()}]) -> [map()].
read_template(TemplatePath, Variables) ->
  {ok, Template} = file:read_file(TemplatePath),
  Ctx            = dict:from_list(Variables),
  bec_yml:decode(render(binary_to_list(Template), Ctx)).

-dialyzer({no_opaque, render/2}).

-spec render(string(), dict:dict(any(), any())) -> string().
render(String, Ctx) ->
  mustache:render(String, Ctx).

%% Convert a binary to upper-case.
-spec binary_to_upper(Bin :: binary()) -> binary().
binary_to_upper(Bin) ->
  list_to_binary(string:to_upper(binary_to_list(Bin))).

-spec do_verify(opts(), map()) -> boolean().
do_verify(Options, Config) ->
  Project      = maps:get(<<"project">>, Config),
  Repo         = maps:get(<<"repo">>,    Config),
  Enforce      = proplists:get_value(enforce, Options, false),
  %% abort_on_error is only meaningful if enforce == false
  AbortOnError = case Enforce of
                   true  ->
                     false;
                   false ->
                     proplists:get_value(abort_on_error, Options, true)
                 end,
  case {Project, Repo} of
    {undefined, _} -> {error, missing_project};
    {_, undefined} -> {error, missing_repo};
    {_, _}         ->
      NewConfig = remove_global_config(Config),
      %% Project key must be upper-case, or some pluging
      %% (e.g. Workzone) may get confused.
      do_verify(binary_to_upper(Project), Repo, NewConfig, Enforce,
                AbortOnError)
  end.

-spec do_verify(binary(), binary(), map(), boolean(), boolean()) -> boolean().
do_verify(Project, Repo, Config, Enforce, AbortOnError) ->
  do_verify(Project, Repo, maps:to_list(Config), Enforce, AbortOnError, true).

-spec do_verify(binary(), binary(), opts(), boolean(), boolean(), boolean()) ->
        boolean().
do_verify(_Project, _Repo, [], _Enforce, _AbortOnError, SoFar) ->
  SoFar;
do_verify(Project, Repo, [{K, V}|Tail], Enforce, AbortOnError, SoFar) ->
  Result = do_verify(Project, Repo, K, V),
  case {Result, Enforce, AbortOnError} of
    {true, _, _} ->
      %% Verification passed. Nothing to enforce, proceeding
      ok = ?LOG_INFO("[~s/~s] Check OK ~n", [Project, Repo]),
      do_verify(Project, Repo, Tail, Enforce, AbortOnError, SoFar);
    {false, true, false} ->
      %% Verification failed. If enforce == true, abort_on_error is false
      %% by definition. Record the fact that verification failed by
      %% updating the SoFar flag.
      ok = ?LOG_WARNING("[~s/~s] Check FAILED~n", [Project, Repo]),
      ok = do_enforce(Project, Repo, K, V),
      do_verify(Project, Repo, Tail, Enforce, AbortOnError, false);
    {false, false, true} ->
      %% Verification failed. abort_on_error is true, so stopping.
      ok = ?LOG_ERROR("[~s/~s] Check FAILED~n", [Project, Repo]),
      false;
    {false, false, false} ->
      %% Verification failed. abort_on_error is false, so record the
      %% fact using the SoFar flag and keep going.
      ok = ?LOG_WARNING("[~s/~s] Check FAILED~n", [Project, Repo]),
      do_verify(Project, Repo, Tail, Enforce, AbortOnError, false)
  end.

-spec getter(binary()) -> fun().
getter(<<"access-keys">>) ->
  fun bitbucket:get_ssh_keys/2;
getter(<<"branch-restrictions">>) ->
  fun bitbucket:get_branch_restrictions/2;
getter(<<"default-branch">>) ->
  fun bitbucket:get_default_branch/2;
getter(<<"wz-branch-reviewers">>) ->
  fun bitbucket:get_wz_branch_reviewers/2;
getter(<<"wz-pr-restrictions">>) ->
  fun bitbucket:get_wz_pr_restrictions/2;
getter(<<"wz-workflow">>) ->
  fun bitbucket:get_wz_workflow/2;
getter(<<"groups">>) ->
  fun bitbucket:get_groups/2;
getter(<<"hooks">>) ->
  fun bitbucket:get_hooks/2;
getter(<<"public">>) ->
  fun bitbucket:get_public/2;
getter(<<"users">>) ->
  fun bitbucket:get_users/2;
getter(<<"push-after-pr">>) ->
  fun bitbucket:get_push_after_pr/2;
getter(<<"unapprove-pr">>) ->
  fun bitbucket:get_unapprove_pr/2;
getter(<<"pr-restrictions">>) ->
  fun bitbucket:get_pr_restrictions/2;
getter(<<"webhooks">>) ->
  fun bitbucket:get_webhooks/2.

-spec do_verify(binary(), binary(), binary(), any()) -> boolean().
do_verify(ProjectKey, RepoSlug, Key, Expected) ->
  ok  = ?LOG_INFO("[~s/~s] Checking ~p ~n", [ProjectKey, RepoSlug, Key]),
  Get = getter(Key),
  Adapted = adapt(Key, Expected),
  {ok, Actual} = Get(ProjectKey, RepoSlug),
  ok = ?LOG_DEBUG( "[~s/~s] Actual   ==> ~p ~n"
                  , [ProjectKey, RepoSlug, Actual]),
  ok = ?LOG_DEBUG( "[~s/~s] Expected ==> ~p ~n"
                 , [ProjectKey, RepoSlug, Adapted]),
  case equals(Key, Actual, Adapted) of
    true ->
      true;
    false ->
      %% Not showing details here since they may contain secrets.
      %% They were debug-logged above.
      ok = ?LOG_ERROR("[~s/~s] Actual does not match Expected"
                      " (increase verbosity to see details)~n"
                     , [ProjectKey, RepoSlug]),
      false
  end.

-spec equals(binary(), any(), any()) -> boolean().
equals(<<"branch-restrictions">>, Actual, Expected) ->
  Expected == [maps:remove(id, X) || X <- Actual];
equals(<<"access-keys">>, Actual, Expected) ->
  lists:sort(Expected) == lists:sort([maps:remove(id, X) || X <- Actual]);
equals(<<"hooks">>, Actual, Expected) ->
  F = fun(Hook) -> lists:member(Hook, Actual) end,
  lists:all(F, Expected);
equals(<<"webhooks">>, Actual0, Expected) ->
  Generated = [id, createdDate, updatedDate],
  Actual = [lists:foldl(fun maps:remove/2, Map, Generated) || Map <- Actual0],
  lists:sort(Expected) == lists:sort(Actual);
equals(_Key, X, X) ->
  true;
equals(_Key, _X, _Y) ->
  false.

-spec setter(binary()) -> fun().
setter(<<"access-keys">>) ->
  fun bitbucket:set_ssh_keys/3;
setter(<<"branch-restrictions">>) ->
  fun bitbucket:set_branch_restrictions/3;
setter(<<"default-branch">>) ->
  fun bitbucket:set_default_branch/3;
setter(<<"wz-branch-reviewers">>) ->
  fun bitbucket:set_wz_branch_reviewers/3;
setter(<<"wz-pr-restrictions">>) ->
  fun bitbucket:set_wz_pr_restrictions/3;
setter(<<"wz-workflow">>) ->
  fun bitbucket:set_wz_workflow/3;
setter(<<"groups">>) ->
  fun bitbucket:set_groups/3;
setter(<<"hooks">>) ->
  fun bitbucket:set_hooks/3;
setter(<<"public">>) ->
  fun bitbucket:set_public/3;
setter(<<"push-after-pr">>) ->
  fun bitbucket:set_push_after_pr/3;
setter(<<"unapprove-pr-on-source-change">>) ->
  fun bitbucket:set_unapprove_pr/3;
setter(<<"users">>) ->
  fun bitbucket:set_users/3;
setter(<<"pr-restrictions">>) ->
  fun bitbucket:set_pr_restrictions/3;
setter(<<"webhooks">>) ->
  fun bitbucket:set_webhooks/3.

-spec adapt(binary(), any()) -> any().
adapt(<<"users">>, Users) ->
  bec_config_ss:to_permission_users(Users);
adapt(<<"groups">>, Groups) ->
  bec_config_ss:to_permission_groups(Groups);
adapt(<<"branch-restrictions">>, Restrictions) ->
  bec_config_ss:to_branch_restrictions(Restrictions);
adapt(<<"access-keys">>, Keys) ->
  bec_config_ss:to_access_keys(Keys);
adapt(<<"hooks">>, Hooks) ->
  bec_config_ss:to_hooks(Hooks);
adapt(<<"pr-restrictions">>, Restrictions) ->
  bec_config_ss:to_pr_restrictions(Restrictions);
adapt(<<"wz-workflow">>, Workflow) ->
  bec_config_ss:to_wz_workflow(Workflow);
adapt(<<"wz-pr-restrictions">>, Restrictions) ->
  bec_config_ss:to_wz_pr_restrictions(Restrictions);
adapt(<<"wz-branch-reviewers">>, Reviewers) ->
  bec_config_ss:to_wz_branch_reviewers(Reviewers);
adapt(<<"webhooks">>, WebHooks) ->
  lists:sort([bec_webhook_t:from_map(WH) || WH <- WebHooks]);
adapt(_Key, Value) ->
  Value.

-spec do_enforce(binary(), binary(), binary(), any()) -> ok.
do_enforce(ProjectKey, RepoSlug, Key, Expected) ->
  ok = ?LOG_INFO("[~s/~s] Enforcing ~p ~n", [ProjectKey, RepoSlug, Key]),
  Set = setter(Key),
  Adapted = adapt(Key, Expected),
  ok = ?LOG_INFO("[~s/~s] Setting value to ~p ~n",
                 [ProjectKey, RepoSlug, Adapted]),
  ok = Set(ProjectKey, RepoSlug, Adapted).

-spec remove_global_config(map()) -> map().
remove_global_config(Config) ->
  maps:remove(<<"project">>, maps:remove(<<"repo">>, Config)).
