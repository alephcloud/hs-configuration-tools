Overview
========

This package provides a collection of utils on top of the packages
optparse-applicative, aeson, and yaml, for configuring libraries and
applications in a composable way.

The main feature is the integration of command line option parsing and
configuration files.

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

The operators assume that lenses for the configuration record types are
provided.

An complete usage example can be found in the file @example/Example.hs@ of the
cabal package.

Usage Example
=============

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

~~~{.haskell}

-- | Specification of the authentication section of a URL.
--
data Auth = Auth
    { _user ∷ !String
    , _pwd ∷ !String
    }

$(makeLenses ''Auth)
~~~

First we define a default value. If there is no reasonable default the
respective value could for instance be wrapped into `Maybe`.

~~~{.haskell}
defaultAuth ∷ Auth
defaultAuth = Auth
    { _user = ""
    , _pwd = ""
    }
~~~

Next we define an [Aeson](https://hackage.haskell.org/package/aeson) `FromJSON`
instance that yields a function that updates a given `Auth` value with the
values from the parsed JSON value.

~~~{.haskell}
instance FromJSON (Auth → Auth) where
    parseJSON = withObject "Auth" $ \o → pure id
        ⊙ user ..: "user" × o
        ⊙ pwd ..: "pwd" × o
~~~

The 'ToJSON' instance is needed to print the configuration (as YAML document)
when the user provides the `--print-config` command line option.

~~~{.haskell}
instance ToJSON Auth where
    toJSON a = object
        [ "user" .= (a ^. user)
        , "pwd" .=  (a ^. pwd)
        ]
~~~

Finally, we define an command line option parser using the machinery from
the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
package. Similar to the `FromJSON` instance the parser does not yield a value
directly but instead yields a function that updates a given `Auth` value with
the value from the command line.

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

The following definiton for the `HttpURL` are similar to definitions for
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

TODO
====

This package is in an early stage of development. I plan to add
more features.

Most of these features are already implemented elsewhere but not yet
integrated into this package.

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

