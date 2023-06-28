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

## Requirements

_BEC_ requires BitBucket Server 5.5 or superior.

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

### Build with Docker

```bash
docker run --rm --name erlangbuilder -v ${PWD}:/bec  -w=/bec  erlang rebar3 escriptize
```

## Sample BitBucket Configuration

BEC supports both Basic Authentication (via username/password) and Token-Based Authentication (preferred).
If both a `token` and a `username/password` pair are provided, the token will take precedence.

Set BitBucket url and credentials in `bitbucket.config`:

```
{bitbucket_url, "https://my.bitbucket.server"}.
{bitbucket_username, "first.last"}.
{bitbucket_password, "password"}.
{bitbucket_token, "someToken"}.
```

Values can also be provided using OS environment variables, for example

```
{bitbucket_password, {env, "BITBUCKET_PASSWORD"}}.
```

Please follow [this](https://confluence.atlassian.com/bitbucketserver072/personal-access-tokens-1005335924.html#Personalaccesstokens-Generatingpersonalaccesstokens) guide if you want to generate the token to authenticate.

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

## Running tests against Bitbucket

The full test suite (including PropEr tests) needs a Bitbucket Server
instance to run.  Running "rebar3 proper" will check if there is one
already running on http://localhost:7990 and start one using a docker
container if no one is found. This functionality requires
[docker-compose](https://docs.docker.com/compose/).

BEC has support for some hooks (see
`bec_proper_gen:supported_hooks/0`), as well as for the Workzone
plugin. These hooks are not available in the default Bitbucket Server
docker image used if you only use "rebar3 proper".

If you have a Bitbucket Server where these plugins/hooks are supported,
you can run the PropEr tests against that server instead. To do this,
set these environment variables:

* `BITBUCKET_SERVER_URL`: The Bitbucket Server you want to use.
* `BITBUCKET_USERNAME`: The username to use. Only http
  username/password authentication is supported for now.
* `BITBUCKET_PASSWORD`: The password to use.

The following environment variables can be used to override which
repo/users/groups will be used for testing. If these do not exist (or
not specified), the test setup will attempt to create them. If
`$BITBUCKET_USERNAME` does not have sufficient privileges to create
repos/users/groups, you can specify pre-created users/groups here.

* `BITBUCKET_PROJECT_KEY`: The Bitbucket project to use.
* `BITBUCKET_REPO_SLUG`: The Bitbucket repository to use.
* `BITBUCKET_TEST_USERS`: A comma-separated list of test users.
* `BITBUCKET_TEST_GROUPS`: A comma-separated list of test groups.

Note: the test users and test groups should be the same length, and
each user should be a member of the corresponding group, i.e.

```
BITBUCKET_TEST_USERS=user.a,user.b
BITBUCKET_TEST_GROUPS=group.a,group.b
```

where `user.a` is a member of `group.a` and `user.b` is a member of
`group.b`.

## Author

Roberto Aloi was the original author of BEC. Lots of people have
contributed to its development since then.

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
