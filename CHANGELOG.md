# configuration-tools

## 0.7.1 (2025-03-04)

* Support lastest GHC (9.12)
* Use setters instead of lenses in ..: and %.:
* Allow validation functions to return new values

## 0.7.0 (2022-06-22)

The version bump is due to the update of the dependency on optparse-applicative,
which could imply breaking changes for users.

* Support GHC-9.6.
* Require Cabal >=3.6.
* Drop support for GHC <8.10.
* Require optparse-applicative >=0.18.
* Require prettyprinter package and drop dependency on deprecated ansi-wl-pprint.
* Replace dependency on cryptonite package by crypton.
* Raise some outdated lower dependency bounds.

## 0.6.1 (2021-10-12)

* Support GHC-9.2
* Support aeson >=2.0

## 0.6.0 (2021-02-16)

#### New

* The command line option `--print-config-as` was added, that takes the values
  `full`, `minimal`, and `diff` and print either the full configuration, a
  minimal configuration that contains only changes that are different from the
  default configuration, or it print a YAML document that shows the difference
  between the actual configuration and the default configuration.

* The helper functions `jsonOption` and `jsonReader` for building command line
  parsers have been added.

#### Removed

* The function `fmapL` is removed from `Configuration.Utils.Internal`. Instead
  the function `first` from `Data.Bifunctor` from the `base` package can be
  used.

## 0.5.0 (2020-04-06)

#### Changed

- Support for GHC < 8.4 has been dropped.
- Support for Cabal < 2.2 has been dropped.
- Support for "Remote Configuration" has been turned off by default. It can be
  manually activated via the `remote-configs` flag.

#### Removed

- The previously deprecated `<.>` and `⊙` operators have been removed. Use `<.<`
  instead for either.

#### Deprecated

- The unicode `×` operator will be removed with the next major release. Use `%`
  instead.

## 0.4.2 (2020-01-25)

* GHC 8.8 support.

## 0.4.1 (2019-05-10)

* Added `pLeftSemigroupalUpdate` and `pRightSemigroupalUpdate`.

## 0.4.0 (2018-08-21)

*   Drop support for GHC < 7.10 and base < 4.8
*   Drop support for Cabal < 1.24
*   Drop support for transformers < 0.4
*   Don't run CI tests for Cabal < 2

*   With Cabal 2.0 or later package info modules are placed in per component
    `autogen` directories. All package info modules are named just `PkgInfo`.
    For backward compatibility modules with the old legacy names (`PkgInfo_*`)
    are still generated but marked deprecated. With Cabal 1.24 only the legacy
    behavior is available and a deprecation warning is raised.

## 0.3.1 (2018-03-16)

*   Support GHC-8.4.1 and Cabal-2.2
*   Replaced the use of non-ascii identifiers in the public API

## 0.3.0

*   Remove built in short options `-p`, `-c`, and `-i`
*   Support GHC-8.2 and Cabal-2.0

## 0.2.15

*   Support for http-client >= 0.5

## 0.2.14

*   Support for GHC-8 and Cabal-1.24

## 0.2.13

*   Eliminate most compiler warnings when compiling with GHC-7.10.

*   Bump lower bound on the version of `optparse-applicative` to `0.11.0.2`.
    This avoids issues when building with `transformers-compat`.

*   Reduce compilation time with `text<1.2.0.5` by avoiding usage of `toCaseFold`
    from the `case-insensitive` package.

*   Dropped dependency on the error package.

*   [Issue 43](https://github.com/alephcloud/hs-configuration-tools/issues/43):
    Support detection of the version control system when the package directory
    and thus the cabal file is in sub-directory of the repository.

## 0.2.12

*   Added support for transformers-0.3.0.0. This changes allows usage
    of configuration-tools along with packages that depend on ghc, which
    in turn depends on transformers-0.3.0.0.

## 0.2.11

*   Added support for Cabal-1.18. This is supposed to make the build more
    robust and simplify integration with existing build infrastructure
    and other packages.

## 0.2.10

*   Moved all internal APIs to the `Internal` name space. Exposing them
    in 0.2.9 was considered a bug.

*   Configuration files can be formatted either as JSON or as YAML.
    For remote configuration files the HTTP `Content-Type` header is used to
    determine the format, for local files the file suffix is used.
    The default format is YAML.

*   Set the HTTP `accept` header for JSON and YAML when requesting remote
    configuration files.

## 0.2.9

*   Use tight constraint for all validation functions. Previously the
    constraint where unnecessarily restrictive.

*   Added `updateProperty` function that generalized `%.:` in the same
    way as `setProperty` generalizes `..:`.

*   Added a validation function for configuration file arguments.

*   Allow usage of more than a single `--config-file` option on the
    command line.

*   Support for static configuration file locations. Configuration files
    can be marked as `required` or `optional`.

*   Support for loading of configuration files form HTTP and HTTPS URLs.
    There are new flags for disabling validation of SSL certificates and
    white listing SSL certificates based on their fingerprint.

*   Added tools for updating configurations with a monoid instance.

*   Added two new option parsers for boolean flags.

    *   The `boolOption_` parser uses the syntax `--feature` and
        `--no-feature` to enable and respectively disable a feature.

    *   The `enableDisableFlag` parser uses the syntax `--enable-feature`
        and `--disable-feature` to enable and respectively disable a feature.

*   Refactored the module layout. The API of the existing modules is
    is backward compatible, but a lot of code got moved into submodules.

*   Improved documentation.

*   Improved test suite.

## 0.2.8

*   Added validation functions for Boolean values, numeric values and
    orders.

*   Added operator `!..:` for parsing of configuration values that are
    required to be present in a configuration file, thus preventing
    the default value from being used.

*   More consistent usage of case in metavar values.

*   Drop support for optparse-applicative < 0.10.

## 0.2.7

*   Added `view` function for lenses to `Configuration.Utils.Internal`.

*   Added support for validation of configuration values.

*   Added module `Configuration.Utils.Validation` that provides primitives
    for validating different basic configuration values.

## 0.2.6

*   For git repositories include also light-weight (non-annotated) tags
    in the version description.

*   Added new function `boolReader`, `boolOption`, `fileOption`, and
    `eitherReadP` to `Utils`.

*   Added new function `maybeOption` and improved documentation about
    `Maybe` config values.

*   Included optimisation level into long info.

## 0.2.5

*   `Configuration.Utils.Setup`: export `mkPkgInfoModules` function
    that modifies a given `UserHooks` record to generate an `PkgInfo`
    module during configuration.

## 0.2.4.1

*   Support for optparse-applicative >= 0.10.

## 0.2.4

*   Configuration.Utils.Setup: fixed generation of `PkgInfo` module for
    package configurations with explicit flags.

*   Improved documentation for `Maybe` values.

## 0.2.3

*   Show the help options in the options summary message.

*   Add `-?` as short version of for `--help` in addition to `-h`.

*   Remove `showHelpOnError` and `disambiguate` from option parser preferences.

*   Added file `INSTALL_ON_WINDOWS.md` with installation instructions for
    windows to the package.

## 0.2.2

*   Add Lens `piOptionParserAndDefaultConfiguration` that gives simultaneous
    accesses to `piOptionParser` and `piDefaultConfiguration` which allows
    changing the type parameter of `ProgramInfo a`.

*   Introduce function `setProperty`. It is used as the `..:` operator
    but allows to specify a custom parser for the property value instead
    of the default `parseJSON` from the `FromJSON` instance.

*   Introduce operators `(<*<)`, `(>*>)`, `(<$<)`, `(>$>)` and deprecate
    `(⊙)` and `(<.>)`.

## 0.2.1

*   Fix build with GHC-7.6 by relaxing lower bounds on some dependencies.

## 0.2

First release.
