0.2.4.1
=======

*   Support for optparse-applicative >= 0.10.

0.2.4
=====

*   Configuration.Utils.Setup: fixed generation of 'PkgInfo' module for
    package configurations with explict flags.

*   Improved documentation for 'Maybe' values.

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

