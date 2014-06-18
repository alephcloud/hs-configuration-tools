Overview
========

This package provides a collection of utils on top of the packages
optparse-applicative, aeson, and yaml, for configuring libraries and
applications in a composable way.

The main features are

1.   configuration management through integration of command line option
     parsing and configuration files and
2.   a `Setup.hs` file that generates a `PkgInfo` module that provides
     information about the package and the build.

Configuration Management
========================

The purpose is to make management of configurations easy by providing an
idiomatic style of defining and deploying configurations.

For each data type that is used as a configuration type the following must be
provided:

1.  a default value,

2.  a 'FromJSON' instance that yields a function that takes a value and
    updates that value with the parsed values,

3.  a 'ToJSON' instance, and

4.  an options parser that yields a function that takes a value and updates
    that value with the values provided as command line options.

The package provides operators and functions that make the implmentation of
these entities easy for the common case that the configurations are encoded
mainly as nested records.

In addition to the user defined command line option the following
options are recognized by the application:

`--config-file, -c`
:    Parse the given file as a (partial) configuration file in YAML format.

`print-config, -p`
:    Print the parsed configuration that would otherwise be used by the
     application to standard out in YAML format and exit.

`--help, -h`
:   Print a help message and exit.

The operators assume that lenses for the configuration record types are
provided.

An complete usage example can be found in the file `example/Example.hs` of the
cabal package.

Usage Example
-------------

Remark: there are non-unicode equivalents available in `Configuration.Utils`
for the UTF-8 operators.

We start with some language extensions and imports.

~~~{.haskell}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
( main
) where

import Configuration.Utils
import Data.Monoid.Unicode
import Prelude.Unicode
~~~

Next we define types for the configuration of our application. In this contrived
example these are the types for a simplified version of HTTP URLs. We also
derive lenses for the configuration types.

~~~{.haskell}
data Auth = Auth
    { _user ∷ !String
    , _pwd ∷ !String
    }

$(makeLenses ''Auth)
~~~

We must provide a default value. If there is no reasonable default the
respective value could for instance be wrapped into `Maybe`.

~~~{.haskell}
defaultAuth ∷ Auth
defaultAuth = Auth
    { _user = ""
    , _pwd = ""
    }
~~~

Now we define an [Aeson](https://hackage.haskell.org/package/aeson) `FromJSON`
instance that yields a function that updates a given `Auth` value with the
values from the parsed JSON value. The `⊙` operator is functional composition
lifted for applicative functors and `×` is a version of `$` with a different
precedence that helps to reduce the use of paranthesis in applicative style
code.

~~~{.haskell}
instance FromJSON (Auth → Auth) where
    parseJSON = withObject "Auth" $ \o → pure id
        ⊙ user ..: "user" × o
        ⊙ pwd ..: "pwd" × o
~~~

The `ToJSON` instance is needed to print the configuration (as YAML document)
when the user provides the `--print-config` command line option.

~~~{.haskell}
instance ToJSON Auth where
    toJSON a = object
        [ "user" .= (a ^. user)
        , "pwd" .=  (a ^. pwd)
        ]
~~~

Finally we define a command line option parser using the machinery from
the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
package. Similar to the `FromJSON` instance the parser does not yield a value
directly but instead yields a function that updates a given `Auth` value with
the value from the command line. The `⊕` is a single spaced UTF-8 version of
the infix monoidal concatenation operator `<>`.

~~~{.haskell}
pAuth ∷ MParser Auth
pAuth = pure id
    ⊙ user .:: strOption
        × long "user"
        ⊕ help "user name"
    ⊙ pwd .:: strOption
        × long "pwd"
        ⊕ help "password for user"
~~~

The following definitons for the `HttpURL` are similar to definitions for
the `Auth` type above. In addition it is demonstrated how define nested
configuration types.

~~~{.haskell}
data HttpURL = HttpURL
    { _auth ∷ !Auth
    , _domain ∷ !String
    , _path ∷ !String
    }

$(makeLenses ''HttpURL)

defaultHttpURL ∷ HttpURL
defaultHttpURL = HttpURL
    { _auth = defaultAuth
    , _domain = ""
    , _path = ""
    }

instance FromJSON (HttpURL → HttpURL) where
    parseJSON = withObject "HttpURL" $ \o → pure id
        ⊙ auth %.: "auth" × o
        ⊙ domain ..: "domain" × o
        ⊙ path ..: "path" × o

instance ToJSON HttpURL where
    toJSON a = object
        [ "auth" .= (a ^. auth)
        , "domain" .= (a ^. domain)
        , "path" .= (a ^. path)
        ]

pHttpURL ∷ MParser HttpURL
pHttpURL = pure id
    ⊙ auth %:: pAuth
    ⊙ domain .:: strOption
        × long "domain"
        ⊕ short 'd'
        ⊕ help "HTTP domain"
    ⊙ path .:: strOption
        × long "path"
        ⊕ short 'p'
        ⊕ help "HTTP URL path"
~~~

Once the configuration value and the related functions and instances is defined
it can be used to create a `ProgramInfo` value. The `ProgramInfo` value is than
use with the `runWithConfiguratin` function to wrap a main function that takes
an `HttpURL` argument with a configuration file and command line parsing.

~~~{.haskell}
mainInfo ∷ ProgramInfo HttpURL
mainInfo = programInfo "HTTP URL" pHttpURL defaultHttpURL

main ∷ IO ()
main = runWithConfiguration mainInfo $ \conf → do
    putStrLn
        $ "http://"
        ⊕ conf ^. auth ∘ user
        ⊕ ":"
        ⊕ conf ^. auth ∘ pwd
        ⊕ "@"
        ⊕ conf ^. domain
        ⊕ "/"
        ⊕ conf ^. path
~~~

Package and Build Information
=============================

The module `Configuration.Utils.Setup`{.haskell} contains an example
`Setup.hs`{.haskell} script that hooks into the cabal build process at the end
of the configuration phase and generates for each a module with package
information for each component of the cabal pacakge.

The modules are created in the *autogen* build directory where also the *Path_*
module is created by cabal's simple build setup. This is usually the directory
`./dist/build/autogen`.

For a library component the module is named just `PkgInfo`{.haskell}. For all
other components the module is name `PkgInfo_COMPONENT_NAME`{.haskell} where
`COMPONENT_NAME` is the name of the component with `-` characters replaced by
`_`.

> For instance, if a cabal package contains a library and an executable that
> is called *my-app*, the following modules are created: `PkgInfo`{.haskell}
> and `PkgInfo_my_app`{.haskell}.

In order to use the feature with your own package the code of the module
`Configuration.Utils.Setup`{.haskell} from the file
`./src/Configuration/Utils/Setup.hs`{.shell} must be placed into a file called
`Setup.hs` in the root directory of your package. In addition the value of the
`Build-Type` field in the package description (cabal) file must be set to
`Custom`:

    Build-Type: Custom

You can integrate the information provided by the `PkgInfo` modules with the
command line interface of an application by importing the respective module for
the component and using the `runWithPkgInfoConfiguration`{.haskell} function
from the module `Configuration.Utils`{.haskell} as show in the following
example:

~~~{.haskell}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
( main
) where

import Configuration.Utils
import PkgInfo

instance FromJSON (() → ()) where parseJSON _ = pure id

mainInfo ∷ ProgramInfo ()
mainInfo = programInfo "Hello World" (pure id) ()

main ∷ IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"
~~~

With that the resulting application supports the following additional command
line options:

`--version, -v`
:    Print the version of the application and exit.

`--info, -i`
:   Print a short info message for the application and exit.

`--long-info`
:   Print a detailed info message for the application and exit.

`--license`
:   Print the text of the lincense of the application and exit.

TODO
====

This package is in an early stage of development. I plan to add
more features.

Most of these features are already implemented elsewhere but not yet
integrated into this package.

*   Teach optparse-applicative to not print usage-message for
    info options.

*   Simplify specification of Configuration data types by
    integrating the aeson instances and the option parser.

*   Include help text as comments in YAML serialization of configuration
    values.

*   Provide operators (or at least examples) for more scenarios
    (like required options)

*   Nicer errors messages if parsing fails.

*   Supper JSON encoded configuration files.

*   Integrate with version control and cabal (e.g. options for joing
    versions of all dependencies and build flags).

*   Support mode where JSON/YAML parsing fails when unexpected
    properties are encountered.

*   Loading of configurations from URLs.

