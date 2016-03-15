0.2.14
======

*   Support for GHC-8 and Cabal-1.24

0.2.13
======

*   Eliminate most compiler warnings when compiling with GHC-7.10.

*   Bump lower bound on the version of `optparse-applicative` to `0.11.0.2`.
    This avoids issues when building with `transformers-compat`.

*   Reduce compilation time with `text<1.2.0.5` by avoiding usage of `toCaseFold`
    from the `case-insensitive` package.

*   Dropped dependency on the error package.

*   [Issue 43](https://github.com/alephcloud/hs-configuration-tools/issues/43):
    Support detection of the version control system when the package directory
    and thus the cabal file is in sub-directory of the repository.

0.2.12
======

*   Added support for transformers-0.3.0.0. This changes allows usage
    of configuration-tools along with packages that depend on ghc, which
    in turn depends on transformers-0.3.0.0.

0.2.11
======

*   Added support for Cabal-1.18. This is supposed to make the build more
    robust and simplify integration with existing build infrastructure
    and other packages.

0.2.10
======

*   Moved all internal APIs to the `Internal` name space. Exposing them
    in 0.2.9 was considered a bug.

*   Configuration files can be formatted either as JSON or as YAML.
    For remote configuration files the HTTP `Content-Type` header is used to
    determine the format, for local files the file suffix is used.
    The default format is YAML.

*   Set the HTTP `accept` header for JSON and YAML when requesting remote
    configuration files.

0.2.9
=====

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

0.2.8
=====

*   Added validation functions for Boolean values, numeric values and
    orders.

*   Added operator `!..:` for parsing of configuration values that are
    required to be present in a configuration file, thus preventing
    the default value from being used.

*   More consistent usage of case in metavar values.

*   Drop support for optparse-applicative < 0.10.

0.2.7
=====

*   Added `view` function for lenses to `Configuration.Utils.Internal`.

*   Added support for validation of configuration values.

*   Added module `Configuration.Utils.Validation` that provides primitives
    for validating different basic configuration values.

0.2.6
=====

*   For git repositories include also light-weight (non-annotated) tags
    in the version description.

*   Added new function `boolReader`, `boolOption`, `fileOption`, and
    `eitherReadP` to `Utils`.

*   Added new function `maybeOption` and improved documentation about
    `Maybe` config values.

*   Included optimisation level into long info.

0.2.5
=====

*   `Configuration.Utils.Setup`: export `mkPkgInfoModules` function
    that modifies a given `UserHooks` record to generate an `PkgInfo`
    module during configuration.

0.2.4.1
=======

*   Support for optparse-applicative >= 0.10.

0.2.4
=====

*   Configuration.Utils.Setup: fixed generation of `PkgInfo` module for
    package configurations with explicit flags.

*   Improved documentation for `Maybe` values.

0.2.3
=====

*   Show the help options in the options summary message.

*   Add `-?` as short version of for `--help` in addition to `-h`.

*   Remove `showHelpOnError` and `disambiguate` from option parser preferences.

*   Added file `INSTALL_ON_WINDOWS.md` with installation instructions for
    windows to the package.

0.2.2
=====

*   Add Lens `piOptionParserAndDefaultConfiguration` that gives simultaneous
    accesses to `piOptionParser` and `piDefaultConfiguration` which allows
    changing the type parameter of `ProgramInfo a`.

*   Introduce function `setProperty`. It is used as the `..:` operator
    but allows to specify a custom parser for the property value instead
    of the default `parseJSON` from the `FromJSON` instance.

*   Introduce operators `(<*<)`, `(>*>)`, `(<$<)`, `(>$>)` and deprecate
    `(⊙)` and `(<.>)`.

0.2.1
=====

*   Fix build with GHC-7.6 by relaxing lower bounds on some dependencies.

0.2
===

First release.

