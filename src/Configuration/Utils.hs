-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


-- | This module provides a collection of utils on top of the packages
-- optparse-applicative, aeson, and yaml, for configuring libraries and
-- applications in a composable way.
--
-- The main feature is the integration of command line option parsing and
-- configuration files.
--
-- The purpose is to make management of configurations easy by providing an
-- idiomatic style of defining and deploying configurations.
--
-- For each data type that is used as a configuration type the following must be
-- provided:
--
-- 1. a default value,
--
-- 2. a 'FromJSON' instance that yields a function that takes a value and
--    updates that value with the parsed values,
--
-- 3. a 'ToJSON' instance, and
--
-- 4. an options parser that yields a function that takes a value and updates
--    that value with the values provided as command line options.
--
-- The module provides operators and functions that make the implmentation of
-- these entities easy for the common case that the configurations are encoded
-- mainly as nested records.
--
-- The operators assume that lenses for the configuration record types are
-- provided.
--
-- An complete usage example can be found in the file @example/Example.hs@
-- of the cabal package.
--
-- TODO (most of these features are already implemented but not yet
-- integrated into this package)
--
-- * Simplify specification of Configuration data types by
--   integrating the aeson instances and the option parser.
--
-- * Include help text as comments in YAML serialization of configuration
--   values.
--
-- * Provide operators (or at least examples) for more scenarios
--   (like required options)
--
-- * Nicer errors messages if parsing fails.
--
-- * Supper JSON encoded configuration files.
--
-- * Integrate with version control and cabal (e.g. options for joing
--   versions of all dependencies and build flags).
--
-- * Support mode where JSON/YAML parsing fails when unexpected
--   properties are encountered.
--
-- * Loading of configurations from URLs.
--
module Configuration.Utils
(
-- * Program Configuration
  ProgramInfo
, programInfo
, piDescription
, piHelpHeader
, piHelpFooter
, piOptionParser
, piDefaultConfiguration

-- * Running an Configured Application
, runWithConfiguration

-- * Applicative Option Parsing with Default Values
, MParser
, (.::)
, (%::)

-- * Parsing of Configuration Files with Default Values
, (..:)
, (%.:)

-- * Misc Utils
, (×)
, (<.>)
, (⊙)
, dropAndUncaml

-- * Reexports
, module Data.Aeson
, module Options.Applicative
, makeLenses
, (^.)
) where

import Control.Error (fmapL)
import Control.Lens hiding ((.=), (<.>), argument)

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import Options.Applicative hiding (Parser, Success, (&))
import qualified Options.Applicative as O

import Prelude.Unicode

import System.IO.Unsafe (unsafePerformIO)

-- | This operator is an alternative for '($)' with a higher precedence which
-- makes it suitable for usage within Applicative style funtors without the need
-- to add parenthesis.
--
-- The hex value of the UTF-8 character × is 0x00d7.
--
-- In VIM type: @Ctrl-V u 00d7@
--
-- You may also define a key binding by adding something like the following line
-- to your vim configuration file:
--
-- > iabbrev <buffer> >< ×
--
(×) ∷ (α → β) → α → β
(×) = ($)
infixr 5 ×
{-# INLINE (×) #-}

-- | Functional composition for applicative functors.
--
-- This is a rather popular operator. Due to conflicts (for instance with the
-- lens package) it may have to be imported qualified.
--
(<.>) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(<.>) a b = pure (.) <*> a <*> b
infixr 4 <.>
{-# INLINE (<.>) #-}

-- | For people who likes nicely aligned code and does not mind messing with
-- editor key-maps: here a version of '(<.>)' that uses a unicode symbol
--
-- The hex value of the UTF-8 character ⊙ is 0x2299.
--
-- A convenient VIM key-map is:
--
-- > iabbrev <buffer> ../ ⊙
--
(⊙) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(⊙) = (<.>)
infixr 4 ⊙
{-# INLINE (⊙) #-}

-- -------------------------------------------------------------------------- --
-- Applicative Option Parsing with Default Values

-- | An operator for applying a setter to an option parser that yields a value.
--
-- Example usage:
--
-- @
-- data Auth = Auth
--     { _user ∷ !String
--     , _pwd ∷ !String
--     }
--
-- $(makeLenses ''Auth)
--
-- pAuth ∷ MParser Auth
-- pAuth = pure id
--    ⊙ user .:: strOption
--        × long "user"
--        ⊕ short 'u'
--        ⊕ help "user name"
--    ⊙ pwd .:: strOption
--        × long "pwd"
--        ⊕ short 'p'
--        ⊕ help "password for user"
-- @
--
(.::) ∷ (Alternative φ, Applicative φ) ⇒ Setter' α β → φ β → φ (α → α)
(.::) a opt = set a <$> opt <|> pure id
infixr 5 .::
{-# INLINE (.::) #-}

-- | An operator for applying a setter to an option parser that yields
-- a modification function.
--
-- Example usage:
--
-- @
-- data HttpURL = HttpURL
--     { _auth ∷ !Auth
--     , _domain ∷ !String
--     }
--
-- $(makeLenses ''HttpURL)
--
-- pHttpURL ∷ MParser HttpURL
-- pHttpURL = pure id
--     ⊙ auth %:: pAuth
--     ⊙ domain .:: strOption
--         × long "domain"
--         ⊕ short 'd'
--         ⊕ help "HTTP domain"
-- @
--
(%::) ∷ (Alternative φ, Applicative φ) ⇒ Setter' α β → φ (β → β) → φ (α → α)
(%::) a opt = over a <$> opt <|> pure id
infixr 5 %::
{-# INLINE (%::) #-}

-- | Type of option parsers that yield a modification function.
--
type MParser α = O.Parser (α → α)

-- -------------------------------------------------------------------------- --
-- Parsing of Configuration Files with Default Values

dropAndUncaml ∷ Int → String → String
dropAndUncaml i l
    | length l < i + 1 = l
    | otherwise = let (h:t) = drop i l
        in toLower h : concatMap (\x → if isUpper x then "-" ⊕ [toLower x] else [x]) t

-- | A variant of the aeson operator '(.:)' that creates a parser
-- that updates a setter with the parsed value.
--
-- @
-- data Auth = Auth
--     { _user ∷ !String
--     , _pwd ∷ !String
--     }
--
-- $(makeLenses ''Auth)
--
-- instance FromJSON (Auth → Auth) where
--     parseJSON = withObject "Auth" $ \o → pure id
--         ⊙ user ..: "user" × o
--         ⊙ pwd ..: "pwd" × o
-- @
--
(..:) ∷ FromJSON β ⇒ Setter' α β → T.Text → Object → Parser (α → α)
(..:) s k o = case H.lookup k o of
    Nothing → pure id
    Just v → set s <$> parseJSON v
infix 6 ..:
{-# INLINE (..:) #-}

-- | A variant of the aeson operator '(.:)' that creates a parser
-- that modifies a setter with a parsed function.
--
-- @
-- data HttpURL = HttpURL
--     { _auth ∷ !Auth
--     , _domain ∷ !String
--     }
--
-- $(makeLenses ''HttpURL)
--
-- instance FromJSON (HttpURL → HttpURL) where
--     parseJSON = withObject "HttpURL" $ \o → pure id
--         ⊙ auth %.: "auth" × o
--         ⊙ domain ..: "domain" × o
-- @
--
(%.:) ∷ FromJSON (β → β) ⇒ Setter' α β → T.Text → Object → Parser (α → α)
(%.:) s k o = case H.lookup k o of
    Nothing → pure id
    Just v → over s <$> parseJSON v
infix 6 %.:
{-# INLINE (%.:) #-}

-- -------------------------------------------------------------------------- --
-- Main Configuration

data ProgramInfo α = ProgramInfo
    { _piDescription ∷ !String
      -- ^ Program Description
    , _piHelpHeader ∷ !(Maybe String)
      -- ^ Help header
    , _piHelpFooter ∷ !(Maybe String)
      -- ^ Help footer
    , _piOptionParser ∷ !(MParser α)
      -- ^ options parser for configuration (TODO consider using a typeclass for this)
    , _piDefaultConfiguration ∷ !α
      -- ^ default configuration
    }

$(makeLenses ''ProgramInfo)

programInfo ∷ String → MParser α → α → ProgramInfo α
programInfo desc parser defaultConfig = ProgramInfo
    { _piDescription = desc
    , _piHelpHeader = Nothing
    , _piHelpFooter = Nothing
    , _piOptionParser = parser
    , _piDefaultConfiguration = defaultConfig
    }

data AppConfiguration α = AppConfiguration
    { _printConfig ∷ !Bool
    , _mainConfig ∷ !α
    }

$(makeLenses ''AppConfiguration)

pAppConfiguration ∷ (FromJSON (α → α)) ⇒ α → O.Parser (AppConfiguration α)
pAppConfiguration d = AppConfiguration
    <$> O.switch
        × O.long "print-config"
        ⊕ O.short 'p'
        ⊕ O.help "Print the parsed configuration to standard out and exit"
        ⊕ O.showDefault
    <*> O.nullOption
        × O.long "config-file"
        ⊕ O.short 'c'
        ⊕ O.metavar "FILE"
        ⊕ O.help "Configuration file for backend services in YAML fromat"
        ⊕ O.eitherReader (\file → fileReader file <*> pure d)
        ⊕ O.value d
  where
    fileReader file = fmapL (\e → "failed to parse configuration file " ⊕ file ⊕ ": " ⊕ show e)
        $ unsafePerformIO (Yaml.decodeFileEither file)

mainOptions
    ∷ ∀ α . FromJSON (α → α)
    ⇒ ProgramInfo α
    → O.ParserInfo (AppConfiguration α)
mainOptions ProgramInfo{..} = O.info optionParser
    $ O.fullDesc
    ⊕ O.progDesc _piDescription
    ⊕ maybe mempty O.header _piHelpHeader
    ⊕ maybe mempty O.footer _piHelpFooter
  where
    optionParser = O.helper
        <*> (over mainConfig <$> _piOptionParser)
        <*> pAppConfiguration _piDefaultConfiguration

-- | Run an IO action with a configuration that is obtained by updating the
-- given default configuration the values defined via command line arguments.
--
-- In addition to the options defined by the given options parser the following
-- options are recognized:
--
-- [@--config-file, -c@]
--     Parse the given file path as a (partial) configuration in YAML
--     format.
--
-- [@--print-config, -p@]
--     Print the final parsed configuration to standard out and exit.
--
-- [@--help, -h@]
--     Print a help message and exit.
--
runWithConfiguration
    ∷ (FromJSON (α → α), ToJSON α)
    ⇒ ProgramInfo α
    → (α → IO ())
    → IO ()
runWithConfiguration appInfo mainFunction = do
    conf ← O.customExecParser parserPrefs mainOpts
    if conf ^. printConfig
        then B8.putStrLn ∘ Yaml.encode $ conf ^. mainConfig
        else mainFunction $ conf ^. mainConfig
  where
    mainOpts = mainOptions appInfo
    parserPrefs = O.prefs
        $ O.disambiguate
        ⊕ O.showHelpOnError

