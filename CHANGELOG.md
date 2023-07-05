# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.5.0] - 2023-07-05

### Added

- Allow injecting OS env vars into YAML files via custom !env tag (#70).
- Allow injecting OS env vars into `bitbucket.config` parameters (#69).
- Handle HTTP 429 Too Many Requests errors (#64).
- Execute tests as GitHub workflows (#52, #53, #56).

### Changed

- The project is now tested on OTP versions 23-26, OTP 23 is the minimum supported version (#49, #65).
- Replaced `lager` with OTP's `logger` (#55).
- Minor improvements in what messages are printed/logged (#49).

### Fixed

- Handle Bitbucket not preserving the order of access keys (#59).
- Avoid confusing WorkZone with lower case project names (#59).

### Security

- Do not log any settings at info level to avoid leaking secrets to the terminal (#66).

## [1.3.0] - 2022-08-18

- Hard reset to commit c4d6e0ac88431458fa0996d3e6fa666c4fda0ead due to changes after it being untested and breaking test.
  Discarded changes are kept in separate branch old-master-1.3.0.
- Cherry-picked commit 44cdf56fa3e070857c43b56ceafad978aa9a5e22 that we know is tested and not breaking tests.

## [1.0.0] - 2020-08-16

- Open sourcing of an existing internal project

<!-- Markdown link dfn's -->
[unreleased]: https://github.com/klarna-incubator/bec/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/klarna-incubator/bec/releases/tag/1.0.0
