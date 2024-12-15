# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `type_.variables` function to get a list of the variables used in an expression
- `type_.functions` function to get a list of the functions used in an expression
- Better documentation on the `interpreter.default_context`

### Changed

- The parser now sets an expression ID for each (sub-)expression. These will later be referenced by the type checker.
