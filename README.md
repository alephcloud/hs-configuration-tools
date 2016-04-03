[![Build Status](https://travis-ci.org/alephcloud/hs-configuration-tools.svg?branch=master)](https://travis-ci.org/alephcloud/hs-configuration-tools)
[![Hackage](https://img.shields.io/hackage/v/configuration-tools.svg)](https://hackage.haskell.org/package/configuration-tools)

Overview
========

This package provides a collection of utilities on top of the packages
[optparse-applicative](http://hackage.haskell.org/package/optparse-applicative),
[aeson](http://hackage.haskell.org/package/aeson), and
[yaml](http://hackage.haskell.org/package/yaml) for configuring libraries and
applications in a composable way.

The main features are

1.  compositional configuration management with integration of command line
    option parsing and configuration files, validation of configurations,
    and loading of configurations from remote locations,
2.  a `Setup.hs` file that generates a `PkgInfo` module for each component
    of a package that provide information about the package and the build, and
3.  a set of types for configuration of HTTP services and clients along
    with aeson instances and command line option parsers.

The ultimate goal for this package is a general framework for
compositional configuration management for software components.
Instead of designing such a framework from scratch the approach of this
package is to first explore design and implementation patterns based
on practical examples and by gluing together existing technology.

Therefor at the current state this package mostly provides operators and
coding patterns for writing stylish boilerplate code.

Once we feel that the developed patterns cover a sufficient portion of
real world requirements we plan to rewrite this package such that
the boilerplate is hidden behind a clean and simple DSL.

Installation
============

Assuming that you have a recent version version of `GHC` and `Cabal`
installed in your system this package can be install from [Hackage](http://hackage.haskell.org/)
via

```bash
cabal install configuration-tools
```

If you don't need support for remote configuration files this package
can be build with a much smaller set of dependencies via

```bash
cabal install -f-remote-configs configuration-tools
```

The package can be tested via

```bash
git clone https://github.com/alephcloud/hs-configuration-tools.git
cd hs-configurationt-tools
cabal configure --enable-tests
cabal build
cabal test
```

If you have issues building this package, first ensure that the installed
version of the `Cabal` library matches the version that is used by
your `cabal` binary. You may compare the results of `cabal info Cabal` and
`cabal --version`.

Configuration Management
========================

The goal of this package is to make management of configurations easy by
providing an idiomatic style of defining and deploying configurations.

For each data type that is used as a configuration type the following must be
provided:

1.  a default value,

2.  a `FromJSON` instance that yields a function that takes a value and
    updates that value with the parsed values,

3.  a `ToJSON` instance, and

4.  an options parser that yields a function that takes a value and updates
    that value with the values provided as command line options.

Optionally, a function for validating the configuration value may be
provided.

The package provides operators and functions that make the implementation of
these requisites easy for the common case that the configuration is encoded
mainly through nested records.

In addition to the user defined command line options the following
options are recognized by the application:

*   `--config-file, -c`
    parses the given file as a --possibly partial-- configuration in YAML or
    JSON format. The file location can be provided either as a local file
    system path or as a remote HTTP or HTTPS URL. In addition a list of static
    configuration file locations can be defined in the code.

    If this option is provided more than a single time the configuration
    files are loaded in the order as the respective options appear
    on the command line, where settings that are loaded later have
    precedence over earlier settings. Files from static locations are
    loaded before files that are specified on the command line.

*   `print-config, -p`
    configures the application and prints the configuration in YAML format
    to standard out and exits. The printed configuration is exactly the
    configuration that otherwise would be used to run the application.

*   `--help, -h`
    prints a help message and exits.

As long as the package wasn't build with `-f-remote-configs` the following
two options are available. They affect how configuration files
are loaded from remote URLs.

*   `--config-https-insecure=true|false`
    Bypass certificate validation for all HTTPS
    connections to all services.

*   `--config-https-allow-cert=HOSTNAME:PORT:FINGERPRINT`
    Unconditionally trust the certificate for connecting
    to the service.

The operators provided in this package assume that
[lenses](http://hackage.haskell.org/package/lens) are provided for field of the
configuration record types.

An complete usage example can be found in the file
[example/Example.hs](https://github.com/alephcloud/hs-configuration-tools/blob/master/examples/Example.hs)
of the cabal package.

Usage Example
-------------

Remark: there are unicode equivalents for some operators available in
`Configuration.Utils` that lead to better aligned and more readable code.

We start with language extensions and imports.

~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
( main
) where

import Configuration.Utils
import Data.Monoid
~~~

Next we define the types that are used for the configuration of our application.
In this contrived example these types define a simplified version of HTTP URLs.

~~~{.haskell}
data Auth = Auth
    { _user :: !String
    , _pwd :: !String
    }
~~~

We have to define lenses for the configuration types. Here we do it explicitly.
Alternatively one could have used TemplateHaskell along with `makeLenses` from
the module `Control.Lens` from the [lens](http://hackage.haskell.org/package/lens)
package.

~~~{.haskell}
user :: Functor f => (String -> f String) -> Auth -> f Auth
user f s = (\u → s { _user = u }) <$> f (_user s)

pwd :: Functor f => (String -> f String) -> Auth -> f Auth
pwd f s = (\p -> s { _pwd = p }) <$> f (_pwd s)
~~~

(Note, that the module `Configuration.Utils` defines its own type synonyms for
lenses. If you import `Control.Lens` you should hide `Lens` and `Lens'` from
either module.)

We must provide a default value. If there is no reasonable default the
respective value could, for instance, be wrapped into `Maybe`. Here we
use the monoid identity value of the type.

~~~{.haskell}
defaultAuth :: Auth
defaultAuth = Auth
    { _user = ""
    , _pwd = ""
    }
~~~

Now we define an [aeson](https://hackage.haskell.org/package/aeson) `FromJSON`
instance that yields a function that updates a given `Auth` value with the
values from the parsed JSON value. The `<*<` operator is functional composition
lifted for applicative functors and `%` is a version of `$` with a different
precedence that helps to reduce the use of parenthesis in applicative style
code.

~~~{.haskell}
instance FromJSON (Auth -> Auth) where
    parseJSON = withObject "Auth" $ \o -> id
        <$< user ..: "user" % o
        <*< pwd ..: "pwd" % o
~~~

The `ToJSON` instance is needed to print the configuration (as YAML document)
when the user provides the `--print-config` command line option.

~~~{.haskell}
instance ToJSON Auth where
    toJSON a = object
        [ "user" .= _user a
        , "pwd" .= _pwd a
        ]
~~~

Finally we define a command line option parser using the machinery from
the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
package. Similar to the `FromJSON` instance the parser does not yield a value
directly but instead yields a function that updates a given `Auth` value with
the value from the command line.

~~~{.haskell}
pAuth :: MParser Auth
pAuth = id
    <$< user .:: strOption
        % long "user"
        <> help "user name"
    <*< pwd .:: strOption
        % long "pwd"
        <> help "password for user"
~~~

You may consult the documentation of the
[optparse-applicative](http://hackage.haskell.org/package/optparse-applicative)
package for further information on how to define command line options.

The following definitions for the `HttpURL` are similar to definitions for
the `Auth` type above. In addition it is demonstrated how to deal with nested
configuration types. Mainly the usage of `..:` is replaced by `%.:` and
`.::` is replaced by `%::`.

~~~{.haskell}
data HttpURL = HttpURL
    { _auth :: !Auth
    , _domain :: !String
    , _path :: !String
    }

auth :: Functor f => (Auth -> f Auth) -> HttpURL -> f HttpURL
auth f s = (\u → s { _auth = u }) <$> f (_auth s)

domain :: Functor f => (String -> f String) -> HttpURL -> f HttpURL
domain f s = (\u → s { _domain = u }) <$> f (_domain s)

path :: Functor f => (String -> f String) -> HttpURL -> f HttpURL
path f s = (\u → s { _path = u }) <$> f (_path s)

defaultHttpURL :: HttpURL
defaultHttpURL = HttpURL
    { _auth = defaultAuth
    , _domain = ""
    , _path = ""
    }

instance FromJSON (HttpURL -> HttpURL) where
    parseJSON = withObject "HttpURL" $ \o -> id
        <$< auth %.: "auth" % o
        <*< domain ..: "domain" % o
        <*< path ..: "path" % o

instance ToJSON HttpURL where
    toJSON a = object
        [ "auth" .= _auth a
        , "domain" .= _domain a
        , "path" .= _path a
        ]

pHttpURL :: MParser HttpURL
pHttpURL = id
    <$< auth %:: pAuth
    <*< domain .:: strOption
        % long "domain"
        <> short 'd'
        <> help "HTTP domain"
    <*< path .:: strOption
        % long "path"
        <> help "HTTP URL path"
~~~

Now that everything is set up the configuration can be used to create a
`ProgramInfo` value. The `ProgramInfo` value is than use with the
`runWithConfiguratin` function to wrap a main function that takes an `HttpURL`
argument with configuration file and command line parsing.

~~~{.haskell}
mainInfo :: ProgramInfo HttpURL
mainInfo = programInfo "HTTP URL" pHttpURL defaultHttpURL

main :: IO ()
main = runWithConfiguration mainInfo $ \conf -> do
    putStrLn
        $ "http://"
        <> (_user . _auth) conf
        <> ":"
        <> (_pwd . _auth) conf
        <> "@"
        <> _domain conf
        <> "/"
        <> _path conf
~~~

Using Sum Types as Configuration Types
======================================

Sum types can not be used as configuration types in the same way as product types.
The reason is that the nondeterminism in the choice of a term for the type is
not restricted to the chosen constructor arguments but in addition there
is non-determinism in the choice of the constructor, too.

An update function for a product type can be defined point-wise as a mapping from
constructor parameters to values. An update for a sum type must take the
constructor context into account. Moreover, when applied to a given default
value the function may not be applicable at all if the default value uses a
different constructor context than what the update assumes.

For the future we plan to provide a general solution for configurations of sum
types which would be based on the possibility to define default values for more
than a single constructor. For now one must restrict configurations of sum types
to yield constant values instead of point-wise (partial) updates. In practice
this means that for a type `a` one has to provide an `FromJSON` instance for `a`
and use the `..:` operator. Similarly for the option parser one has to define a
parser that yields an `a` and use it with the `.::` operator.

The module `Configuration.Utils.Maybe` provides tools for dealing with
`Maybe` values.

Package and Build Information
=============================

The module `Configuration.Utils.Setup` an example `Setup.hs` script that hooks
into the cabal build process at the end of the configuration phase and generates
a module with package information for each component of the cabal package.

The modules are created in the *autogen* build directory where also the *Path_*
module is created by cabal's simple build setup. This is usually the directory
`./dist/build/autogen`.

For a library component the module is named just `PkgInfo`. For all
other components the module is named `PkgInfo_COMPONENT_NAME` where
`COMPONENT_NAME` is the name of the component with `-` characters replaced by
`_`.

For instance, if a cabal package contains a library and an executable that
is called *my-app*, the following modules are created: `PkgInfo`
and `PkgInfo_my_app`.

Usage as Setup Script
---------------------

There are two ways how this module can be used:

1.  Copy the code of this module into a file called `Setup.hs` in the root
    directory of your package.

2.  If the *configuration-tools* package is already installed in the system
    where the build is done, following code can be used as `Setup.hs` script:

    ~~~{.haskell}
    module Main (main) where

    import Configuration.Utils.Setup
    ~~~

With both methods the field `Build-Type` in the package description (cabal) file
must be set to `Custom`:

    Build-Type: Custom

Integration With `Configuration.Utils`
--------------------------------------

You can integrate the information provided by the `PkgInfo` modules with the
command line interface of an application by importing the respective module for
the component and using the `runWithPkgInfoConfiguration` function from the
module `Configuration.Utils` as show in the following example:

~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
( main
) where

import Configuration.Utils
import PkgInfo

instance FromJSON (() -> ()) where parseJSON _ = pure id

mainInfo :: ProgramInfo ()
mainInfo = programInfo "Hello World" (pure id) ()

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"
~~~

With that the resulting application supports the following additional command
line options:

*   `--version, -v`
    prints the version of the application and exits.

*   `--info, -i`
    prints a short info message for the application and exits.

*   `--long-info`
    print a detailed info message for the application and exits.
    Beside component name, package name, version, revision, and copyright
    the message also contain information about the compiler that
    was used for the build, the build architecture, build flags,
    the author, the license type, and a list of all direct and
    indirect dependencies along with their licenses and copyrights.

*   `--license`
    prints the text of the license of the application and exits.

Here is the example output of `--long-info` for the example
`examples/Trivial.hs` from this package:

~~~{.shell}
trivial-0.2.6 (package configuration-tools-0.2.6 revision 2cc860c)
Copyright (c) 2014 AlephCloud, Inc.

Author: Lars Kuhtz <lars@alephcloud.com>
License: MIT
Homepage: https://github.com/alephcloud/hs-configuration-tools
Build with: ghc-7.8.3 (x86_64-osx)
Build flags:
Optimisation: normal

Dependencies:
    Cabal-1.20.0.2 [BSD3, 2003-2006, Isaac Jones 2005-2011, Duncan Coutts]
    MonadRandom-0.3 [OtherLicense]
    aeson-0.8.0.0 [BSD3, (c) 2011-2014 Bryan O'Sullivan (c) 2011 MailRank, Inc.]
    ansi-terminal-0.6.1.1 [BSD3]
    ansi-wl-pprint-0.6.7.1 [BSD3]
    array-0.5.0.0 [BSD3]
    attoparsec-0.12.1.0 [BSD3]
    base-4.7.0.1 [BSD3]
    base-unicode-symbols-0.2.2.4 [BSD3, 2009–2011 Roel van Dijk <vandijk.roel@gmail.com>]
    bifunctors-4.1.1.1 [BSD3, Copyright (C) 2008-2013 Edward A. Kmett]
    rts-1.0 [BSD3]
    bytestring-0.10.4.0 [BSD3, Copyright (c) Don Stewart 2005-2009, (c) Duncan Coutts 2006-2013, (c) David Roundy 2003-2005, (c) Jasper Van der Jeugt 2010, (c) Simon Meier 2010-2013.]
    case-insensitive-1.2.0.0 [BSD3, 2011 Bas van Dijk]
    comonad-4.2.2 [BSD3, Copyright (C) 2008-2014 Edward A. Kmett, Copyright (C) 2004-2008 Dave Menendez]
    conduit-1.2.0.2 [MIT]
    containers-0.5.5.1 [BSD3]
    contravariant-1.2 [BSD3, Copyright (C) 2007-2014 Edward A. Kmett]
    deepseq-1.3.0.2 [BSD3]
    directory-1.2.1.0 [BSD3]
    distributive-0.4.4 [BSD3, Copyright (C) 2011-2014 Edward A. Kmett]
    dlist-0.7.1 [BSD3, 2006-2009 Don Stewart, 2013 Sean Leather]
    either-4.3.1 [BSD3, Copyright (C) 2008-2014 Edward A. Kmett]
    errors-1.4.7 [BSD3, 2012, 2013 Gabriel Gonzalez]
    exceptions-0.6.1 [BSD3, Copyright (C) 2013-2014 Edward A. Kmett Copyright (C) 2012 Google Inc.]
    filepath-1.3.0.2 [BSD3]
    free-4.9 [BSD3, Copyright (C) 2008-2013 Edward A. Kmett]
    ghc-prim-0.3.1.0 [BSD3]
    hashable-1.2.2.0 [BSD3]
    integer-gmp-0.5.1.0 [BSD3]
    lifted-base-0.2.3.0 [BSD3, (c) 2011-2012 Bas van Dijk, Anders Kaseorg]
    mmorph-1.0.4 [BSD3, 2013 Gabriel Gonzalez]
    monad-control-0.3.3.0 [BSD3, (c) 2011 Bas van Dijk, Anders Kaseorg]
    mtl-2.2.1 [BSD3]
    nats-0.2 [BSD3, Copyright (C) 2011-2014 Edward A. Kmett]
    old-locale-1.0.0.6 [BSD3]
    optparse-applicative-0.11.0.1 [BSD3, (c) 2012-2014 Paolo Capriotti <paolo@capriotti.io>]
    prelude-extras-0.4 [BSD3, Copyright (C) 2011-2014 Edward A. Kmett]
    pretty-1.1.1.1 [BSD3]
    primitive-0.5.3.0 [BSD3, (c) Roman Leshchinskiy 2009-2012]
    process-1.2.0.0 [BSD3]
    profunctors-4.2.0.1 [BSD3, Copyright (C) 2011-2014 Edward A. Kmett]
    random-1.0.1.1 [BSD3]
    resourcet-1.1.2.3 [BSD3]
    safe-0.3.6 [BSD3, Neil Mitchell 2007-2014]
    scientific-0.3.3.0 [BSD3]
    semigroupoids-4.2 [BSD3, Copyright (C) 2011-2013 Edward A. Kmett]
    semigroups-0.15.3 [BSD3, Copyright (C) 2011-2014 Edward A. Kmett]
    stm-2.4.3 [BSD3]
    syb-0.4.2 [BSD3]
    tagged-0.7.2 [BSD3, 2009-2013 Edward A. Kmett]
    template-haskell-2.9.0.0 [BSD3]
    text-1.1.1.3 [BSD3, 2009-2011 Bryan O'Sullivan, 2008-2009 Tom Harper]
    time-1.4.2 [BSD3]
    transformers-0.4.1.0 [BSD3]
    transformers-base-0.4.3 [BSD3, 2011 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>, Bas van Dijk <v.dijk.bas@gmail.com>]
    transformers-compat-0.3.3.4 [BSD3, Copyright (C) 2012 Edward A. Kmett]
    unix-2.7.0.1 [BSD3]
    unordered-containers-0.2.5.0 [BSD3, 2010-2014 Johan Tibell 2010 Edward Z. Yang]
    vector-0.10.11.0 [BSD3, (c) Roman Leshchinskiy 2008-2012]
    void-0.6.1 [BSD3, Copyright (C) 2008-2013 Edward A. Kmett]
    yaml-0.8.9.1 [BSD3]
~~~

Configuration Types for HTTP Services and Clients
=================================================

The module `Configuration.Utils.Http` contains some types for configuring HTTP
services and clients. Currently these types only provide the most basic
configuration settings. This will probably be extended in the future. Feel free
to submit patches for missing settings.

TODO
====

This package is in an early stage of development and more features
are planned.

*   Simplify specification of Configuration data types by
    integrating the aeson instances and the option parser.

*   Come up with a story for sum types. We may use the following approach: The
    definition of the default should include alternate values for each
    constructor. Effectively, this means to map the sum type onto a product type
    by interpreting the summands as factors. For mapping back from the product
    type to the original sum type one has to provide a choice of the
    constructor. Intuitively, a sum type can be represented as a tree where the
    leafs partition the type into classes of value with the same constructors.
    By providing a default value for each such class partial configurations that
    are defined through point-wise updates can always be applied in a meaningful
    way.

*   Include help text as comments in YAML serialization of configuration
    values.

*   Provide operators (or at least examples) for more scenarios
    (like required options)

*   Nicer error messages if parsing fails.

*   Support JSON encoded configuration files.

*   Support mode where JSON/YAML parsing fails when unexpected
    properties are encountered.

*   Include default values in help message.

*   Use 'helpDoc' to highlight "meta-options", like options that enable
    optional configuration values through usage of the `maybeOption`
    function.

*   Add functionality optparse-applicative that allows to group options.

