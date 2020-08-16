# BEC
> Keep your BitBucket Settings under version control

[![Build Status][ci-image]][ci-url]
[![License][license-image]][license-url]
[![Developed at Klarna][klarna-image]][klarna-url]

The behaviour of a BitBucket repository can be customized in a
multitude of ways. Configuring default reviewers, branch restrictions,
hooks and merge strategies can be a tedious, repetitive and error
prone procedure if it is performed via the GUI, especially with a
large number of repositories.

Wouldn't it be nice if it was possible to store the BitBucket settings
under version control and maintain them in the same way we maintain
code? This is the idea behind _BEC_, a tool which has been developed
internally at Klarna. The tool allows you to write your BitBucket
repository settings in `yml` format and to check and apply them via a
command line interface.

BEC comes with extensive
documentation and sample repository configuration files. It also supports Mustache
templates, allowing you to apply variations of your configuration to
multiple repositories.

Here's a [short presentation][slides] explaining what _BEC_ is about.

## Quickstart

To build `bec`, you need Erlang/OTP 21+ and rebar3 installed on your machine.

```
git clone git@github.com:klarna-incubator/bec.git
cd bec
rebar3 escriptize
```

You will get an executable escript in:

```
_build/default/bin/
```

Ensure the location of the script is added to your `PATH`:

```
export PATH=/path/to/_build/default/bin:$PATH
```

Then you can run _BEC_:

```
$ bec
Usage: bec [-h] [-c [<config>]] [-r [<repo_config>]]
           [-e [<enforce>]] [-k [<keep>]]
           [-v [<verbosity>]]

  -h, --help         Show this help message
  -c, --config       The BitBucket Config File [default: bitbucket.config]
  -r, --repo_config  The Repo Config to check or configure [default: undefined]
  -e, --enforce      Enforce values when they do not match expectations [default: false]
  -k, --keep         Keep going after the first error (always true when enforce == true) [default: false]
  -v, --verbosity    Verbosity Level [default: 1]

```

## Sample BitBucket Configuration

Set BitBucket url and credentials in `bitbucket.config`:

```
{bitbucket_url, "https://my.bitbucket.server"}.
{bitbucket_username, "first.last"}.
{bitbucket_password, "password"}.
```

## Sample Repo Configuration

You can find a sample configuration file for a custom BitBucket repo
in [sample_repo_configuration.yml](sample_repo_configuration.yml).

### Repo Configuration Templates

It is a common need to apply very similar configuration to multiple
repositories. Repo configuration templates are supported for this use
case.

A repo configuration file is similar to a repo configuration, but has
a `.ymlt` extension, and will be rendered as a
[Mustache](https://mustache.github.io/) template. The parameters for
the template are defined in a `.ymlv` file (as Yaml).

For example `common_repo.ymlt` may look like this:

```
---
project: {{project}}
repo: {{repo}}

pr-restrictions:
  required-all-approvers : true
  required-all-tasks-complete : true
  required-approvers : 2
  {{#has_ci}}required-successful-builds : 1
  {{/has_ci}}
  # Please note that Mustache throws away all white space after
  # section opening and closing tags, this is the reason for not
  # putting both of them on a single line. The closing tag has to
  # be indented as much as the next line of Yaml needs to be.

{{#has_ci}}
hooks:
  - key: "com.nerdwin15.stash-stash-webhook-jenkins:jenkinsPostReceiveHook"
    enabled: true
    settings:
      branchOptions:
      branchOptionsBranches:
      cloneType: custom
      gitRepoUrl: ssh://git@my.bitbucket.server/{{project}}/{{repo}}.git
      ignoreCerts: false
      ignoreCommitters:
      jenkinsBase: https://jenkins.hipster.company.io/
      omitBranchName: false
      omitHashCode: false
      omitTriggerBuildButton: false
{{/has_ci}}
```

The corresponding `common_repo.ymlv` specifies the values for the
`project`, `repo` and `has_ci` variables:

```
---
- project: MYTEAM
  repo: my_repo
  has_ci: true

- project: MYTEAM
  repo: best_cat_pics
  has_ci: false
```

Please note that previous versions of the BitBucket Erlang client only
supported the `repo` parameter in the `.ymlt` file, and the `.ymlv`
file had a simplified syntax:

```
---
repo:
  - my_repo
  - best_cat_pics
```

## Known Limitations

* Basic HTTP Authentication is the only authentication mechanism
  currently supported
* The BitBucket Paged API is not supported, yet

## Migrating from version 1.x to 2.x

Version 2.x of the BitBucket Erlang client requires BitBucket 5.5 or
greater. The version introduces some backward-incompatibile changes to
the YML config. This section summarizes the required changes.

### Access Keys

Version 1.x:

    access-keys:
      - NAME:
          key: >
            ssh-rsa AAA...
          permission: read

Version 2.x:

    access-keys:
       - permission: PERMISSION_TYPE
         text: >
           ssh-rsa AAA...

Where `PERMISSION_TYPE` is either `REPO_READ` or `REPO_WRITE`.

Reason for the change: the key _name_ was an internal concept to the
client and it did not have a counterpart in BitBucket. This created
confusion for some users, since it was thought to be equivalent to the
_id_ which is automatically assigned to the access key by BitBucket.

### Branch Restrictions

Version 1.x:

    branch-restrictions:
      - BRANCH_NAME:
        - RESTRICTION_TYPE:
            exceptions:
              groups:
                - GROUP_NAME
              users:
                - USER_NAME

Version 2.x:

    branch-restrictions:
      - branch-id: BRANCH_NAME
        type: RESTRICTION_TYPE
        matcher-type: MATCHER_TYPE
        exceptions:
          groups:
            - GROUP_NAME
          users:
            - USER_NAME

Where `RESTRICTION_TYPE` is one of `fast-forward-only`, `no-deletes`,
`pull-request-only` or `read-only` and `MATCHER_TYPE` is either
`BRANCH` or `PATTERN`.

`users` and `groups` entries are now mandatory. So, if you want an
empty list of users, use something like `users: []`.

Reason for the change: utilize a (more verbose, but clearer) data
structure which is closer to the one exposed by the BitBucket API and
simplify handling of some parameters (e.g. the _matcher type_), which
were hidden before.

### Default Reviewers

Version 1.x:

    default-reviewers:
      - BRANCH_NAME:
        groupQuota: 1
        paths:
          - FILE_PATH:
              groups:
                - GROUP_NAME
              users:
                - USER_NAME

Version 2.x:

    wz-branch-reviewers:
      - branch-id: BRANCH_NAME
        users: []
        groups: []
        paths:
          - FILE_PATH
              groups:
                - GROUP_NAME
              users:
                - USER_NAME

    wz-pr-restrictions:
      - branch-id: BRANCH_NAME
        approval-quota: 1
        group-quota: 1

Reason for the change: the previous implementation was combining two
independent APIs into one. This made the client code more complicated
and less maintainable. Default reviewers are now built-in in BitBucket
and this change prepares for a newer version of the client, where the
built-in mechanism is used, instead of the one provided by the
_Workzone_ plugin.

### Hooks

Version 1.x:

    hooks:
      - HOOK_KEY:
          K1: V1
          K2: V2

Version 2.x:

    hooks:
      - key: HOOK_KEY
        enabled: true
        settings:
          K1: V1
          K2: V2

Where `K*` and `V*` are plugin-specif key-value pairs.

Reason for the change: the new format allows to keep hook
configuration under version control even for disabled plugins.

#### The _http-get-post-receive_ hook

The _http-get-post-receive_ hook is worth a special mention since, in
the 1.x version of the client, its configuration was going through a
plugin-specific adapter which has now been removed.

Version 1.x:

    hooks:
      - de.aeffle.stash.plugin.stash-http-get-post-receive-hook:http-get-post-receive-hook
          - HOOK_NAME_1:
              branchFilter: ^$
              httpMethod: POST
              tagFilter: .*
              url: https://my/url?token=MY_TOKEN&GIT_BRANCH=${refChange.name}
          - HOOK_NAME_2:
              branchFilter: master
              httpMethod: POST
              tagFilter: ^$
              url: https://my/second/url

Version 2.x:

    hooks:
      - key: de.aeffle.stash.plugin.stash-http-get-post-receive-hook:http-get-post-receive-hook
        enabled: true
        settings:
          locationCount: '2' # The amount of hooks that are configured below
          version: '3' # Plugin's API version
          branchFilter: ^$
          httpMethod: POST
          tagFilter: .*
          url: https://my/url?token=MY_TOKEN&GIT_BRANCH=${refChange.name}
          skipSsl: true
          branchFilter2: master
          httpMethod2: POST
          tagFilter2: ^$
          url2: https://my/second/url
          skipSsl2: false

Reason for the change: I won't argue that the previous way of
configuring this hook was much nicer, but that was just syntactic
sugar introduced by the client, through a plugin-specific hack. That
hack has now been removed from the client, so that there is a 1-to-1
mapping between the configuration provided by the user in the YML file
and the one expected by the plugins and things are a lot easier to
maintain. If you believe (as you should) that the current
configuration for the plugin looks terrible, feel free to contact the
plugin author and complain.

### Pull Request Restrictions

Version 1.x:

    pull-requests:
      requiredApprovers        : 2
      requiredSuccessfulBuilds : 1
      requiredAllTasksComplete : true
      requiredAllApprovers     : false

    merge-strategies:
      default: STRATEGY_TYPE
      enabled:
        - STRATEGY_TYPE
        - STRATEGY_TYPE

Version 2.x:

    pr-restrictions:
      merge-config:
        default-strategy: STRATEGY_TYPE
        enabled-strategies:
          - STRATEGY_TYPE
          - STRATEGY_TYPE
      required-all-approvers : true
      required-all-tasks-complete : true
      required-approvers : 2
      required-successful-builds : 1

Where `STRATEGY_TYPE` is one of `ff`, `ff-only`, `no-ff`, `squash`,
`squash-ff-only`, `rebase-no-ff` or `rebase-ff-only`.

Reason for the change: pull requests restrictions are configured using
a single API. The change makes the client code simpler and more
maintanaible. It also avoids multiple, un-necessary, API calls.

### Workzone Flow

Version 1.x:

    push-after-pr: true
    unapprove-pr-on-source-change: true

Version 2.x:

    wz-workflow:
      push-after-pr: true
      unapprove-pr: true

Reason for the change: these settings are plugin-specific and it makes
sense to nest them appropriately.

## Author

* Roberto Aloi

## How to contribute

See our guide on [contributing](.github/CONTRIBUTING.md).

## Release History

See our [changelog](CHANGELOG.md).

## License

Copyright © 2020 Klarna Bank AB

For license details, see the [LICENSE](LICENSE) file in the root of this project.

<!-- Markdown link & img dfn's -->
[ci-image]: https://img.shields.io/badge/build-passing-brightgreen?style=flat-square
[ci-url]: https://github.com/klarna-incubator/bec
[license-image]: https://img.shields.io/badge/license-Apache%202-blue?style=flat-square
[license-url]: http://www.apache.org/licenses/LICENSE-2.0
[klarna-image]: https://img.shields.io/badge/%20-Developed%20at%20Klarna-black?labelColor=ffb3c7&style=flat-square&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAOCAYAAAAmL5yKAAAAAXNSR0IArs4c6QAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAALQAAAAAQAAAtAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAABCgAwAEAAAAAQAAAA4AAAAA0LMKiwAAAAlwSFlzAABuugAAbroB1t6xFwAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAVBJREFUKBVtkz0vREEUhsdXgo5qJXohkUgQ0fgFNFpR2V5ClP6CQu9PiB6lEL1I7B9A4/treZ47c252s97k2ffMmZkz5869m1JKL/AFbzAHaiRbmsIf4BdaMAZqMFsOXNxXkroKbxCPV5l8yHOJLVipn9/vEreLa7FguSN3S2ynA/ATeQuI8tTY6OOY34DQaQnq9mPCDtxoBwuRxPfAvPMWnARlB12KAi6eLTPruOOP4gcl33O6+Sjgc83DJkRH+h2MgorLzaPy68W48BG2S+xYnmAa1L+nOxEduMH3fgjGFvZeVkANZau68B6CrgJxWosFFpF7iG+h5wKZqwt42qIJtARu/ix+gqsosEq8D35o6R3c7OL4lAnTDljEe9B3Qa2BYzmHemDCt6Diwo6JY7E+A82OnN9HuoBruAQvUQ1nSxP4GVzBDRyBfygf6RW2/gD3NmEv+K/DZgAAAABJRU5ErkJggg==
[klarna-url]: https://github.com/klarna-incubator
[slides]:https://speakerdeck.com/robertoaloi/migrating-to-bitbucket-lessons-learnt
