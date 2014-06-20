-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


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
, PkgInfo
, runWithPkgInfoConfiguration

-- * Applicative Option Parsing with Default Values
, MParser
, (.::)
, (%::)

-- * Parsing of Configuration Files with Default Values
, (..:)
, (%.:)

-- * Misc Utils
, (%)
, (×)
, (<.>)
, (⊙)
, dropAndUncaml
, Lens'

-- * Reexports
, module Data.Aeson
, module Options.Applicative
) where

import Configuration.Utils.Internal

import Control.Error (fmapL)

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import Options.Applicative hiding (Parser, Success, (&))
import qualified Options.Applicative as O

import Prelude.Unicode

import System.IO.Unsafe (unsafePerformIO)

-- -------------------------------------------------------------------------- --
-- Useful Operators

-- | This operator is an alternative for '$' with a higher precedence which
-- makes it suitable for usage within Applicative style funtors without the need
-- to add parenthesis.
--
(%) ∷ (α → β) → α → β
(%) = ($)
infixr 5 %
{-# INLINE (%) #-}

-- | This operator is a UTF-8 version of '(%)' which is an alternative for '($)'
-- with a higher precedence which makes it suitable for usage within Applicative
-- style funtors without the need to add parenthesis.
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

-- | For people who like nicely aligned code and do not mind messing with
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
-- user ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- user f s = (\u → s { _user = u }) <$> f (_user s)
--
-- pwd ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- pwd f s = (\p → s { _pwd = p }) <$> f (_pwd s)
--
-- -- or with lenses and TemplateHaskell just:
-- -- $(makeLenses ''Auth)
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
(.::) ∷ (Alternative φ, Applicative φ) ⇒ Lens' α β → φ β → φ (α → α)
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
-- auth ∷ Functor φ ⇒ (Auth → φ Auth) → HttpURL → φ HttpURL
-- auth f s = (\u → s { _auth = u }) <$> f (_auth s)
--
-- domain ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- domain f s = (\u → s { _domain = u }) <$> f (_domain s)
--
-- path ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- path f s = (\u → s { _path = u }) <$> f (_path s)
--
-- -- or with lenses and TemplateHaskell just:
-- -- $(makeLenses ''HttpURL)
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
(%::) ∷ (Alternative φ, Applicative φ) ⇒ Lens' α β → φ (β → β) → φ (α → α)
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
-- user ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- user f s = (\u → s { _user = u }) <$> f (_user s)
--
-- pwd ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- pwd f s = (\p → s { _pwd = p }) <$> f (_pwd s)
--
-- -- or with lenses and TemplateHaskell just:
-- -- $(makeLenses ''Auth)
--
-- instance FromJSON (Auth → Auth) where
--     parseJSON = withObject "Auth" $ \o → pure id
--         ⊙ user ..: "user" × o
--         ⊙ pwd ..: "pwd" × o
-- @
--
(..:) ∷ FromJSON β ⇒ Lens' α β → T.Text → Object → Parser (α → α)
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
-- auth ∷ Functor φ ⇒ (Auth → φ Auth) → HttpURL → φ HttpURL
-- auth f s = (\u → s { _auth = u }) <$> f (_auth s)
--
-- domain ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- domain f s = (\u → s { _domain = u }) <$> f (_domain s)
--
-- path ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- path f s = (\u → s { _path = u }) <$> f (_path s)
--
-- -- or with lenses and TemplateHaskell just:
-- -- $(makeLenses ''HttpURL)
--
-- instance FromJSON (HttpURL → HttpURL) where
--     parseJSON = withObject "HttpURL" $ \o → pure id
--         ⊙ auth %.: "auth" × o
--         ⊙ domain ..: "domain" × o
-- @
--
(%.:) ∷ FromJSON (β → β) ⇒ Lens' α β → T.Text → Object → Parser (α → α)
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

piDescription ∷ Lens' (ProgramInfo α) String
piDescription = lens _piDescription $ \s a → s { _piDescription = a}

piHelpHeader ∷ Lens' (ProgramInfo α) (Maybe String)
piHelpHeader = lens _piHelpHeader $ \s a → s { _piHelpHeader = a}

piHelpFooter ∷ Lens' (ProgramInfo α) (Maybe String)
piHelpFooter = lens _piHelpFooter $ \s a → s { _piHelpFooter = a}

piOptionParser ∷ Lens' (ProgramInfo α) (MParser α)
piOptionParser = lens _piOptionParser $ \s a → s { _piOptionParser = a}

piDefaultConfiguration ∷ Lens' (ProgramInfo α) α
piDefaultConfiguration = lens _piDefaultConfiguration $ \s a → s { _piDefaultConfiguration = a}

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

printConfig ∷ Lens' (AppConfiguration α) Bool
printConfig = lens _printConfig $ \s a → s { _printConfig = a }

mainConfig ∷ Lens' (AppConfiguration α) α
mainConfig = lens _mainConfig $ \s a → s { _mainConfig = a }

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
    → (∀ β . Maybe (MParser β))
    → O.ParserInfo (AppConfiguration α)
mainOptions ProgramInfo{..} pkgInfoParser = O.info optionParser
    $ O.progDesc _piDescription
    ⊕ O.fullDesc
    ⊕ maybe mempty O.header _piHelpHeader
    ⊕ maybe mempty O.footer _piHelpFooter
  where
    optionParser = fromMaybe (pure id) pkgInfoParser
        <*> O.helper
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
    if _printConfig conf
        then B8.putStrLn ∘ Yaml.encode ∘ _mainConfig $ conf
        else mainFunction ∘ _mainConfig $ conf
  where
    mainOpts = mainOptions appInfo Nothing
    parserPrefs = O.prefs
        $ O.disambiguate
        ⊕ O.showHelpOnError

-- -------------------------------------------------------------------------- --
-- Main Configuration with Package Info

pPkgInfo ∷ PkgInfo → MParser α
pPkgInfo (sinfo, detailedInfo, version, license) =
    infoO <*> detailedInfoO <*> versionO <*> licenseO
  where
    infoO = infoOption sinfo
        $ O.long "info"
        ⊕ O.short 'i'
        ⊕ O.help "Print program info message and exit"
        ⊕ O.value id
    detailedInfoO = infoOption detailedInfo
        $ O.long "long-info"
        ⊕ O.help "Print detailed program info message and exit"
        ⊕ O.value id
    versionO = infoOption version
        $ O.long "version"
        ⊕ O.short 'v'
        ⊕ O.help "Print version string and exit"
        ⊕ O.value id
    licenseO = infoOption license
        $ O.long "license"
        ⊕ O.help "Print license of the program and exit"
        ⊕ O.value id

-- | @(info message, detailed info message, version string, license text)@
--
type PkgInfo =
    ( String
      -- info message
    , String
      -- detailed info message
    , String
      -- version string
    , String
      -- license text
    )

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
-- [@--version, -v@]
--     Print the version of the application and exit.
--
-- [@--info, -i@]
--     Print a short info message for the application and exit.
--
-- [@--long-inf@]
--     Print a detailed info message for the application and exit.
--
-- [@--license@]
--     Print the text of the lincense of the application and exit.
--
runWithPkgInfoConfiguration
    ∷ (FromJSON (α → α), ToJSON α)
    ⇒ ProgramInfo α
    → PkgInfo
    → (α → IO ())
    → IO ()
runWithPkgInfoConfiguration appInfo pkgInfo mainFunction = do
    conf ← O.customExecParser parserPrefs mainOpts
    if _printConfig conf
        then B8.putStrLn ∘ Yaml.encode ∘ _mainConfig $ conf
        else mainFunction ∘ _mainConfig $ conf
  where
    mainOpts = mainOptions appInfo (Just $ pPkgInfo pkgInfo)
    parserPrefs = O.prefs
        $ O.disambiguate
        ⊕ O.showHelpOnError

-- -------------------------------------------------------------------------- --
-- Configuration of Optional ('Maybe') Values

-- | Optional configuration values are supposed to be encoded by wrapping
-- the respective type with 'Maybe'.
--
-- The semantics are as follows:
--
-- * If the parsed configuration value is 'Null' the result is 'Nothing'.
-- * If the parsed configuration value is not 'Null' then the result is
--   an update function that
--
--     * updates the given default value if the given default value is @Just x@
--       or
--     * is a constant function that returns the value that is parsed
--       from the configuration using the 'FromJSON' instance for the
--       configuration type.
--
-- Note, that this instance requires an 'FromJSON' instance for
-- the option configuration type itself as well as a 'FromJSON' instance
-- for an update function of the configuration type. The former can
-- be defined by means of the latter as follows:
--
-- @
-- instance FromJSON MyType where
--     parseJSON v = parseJSON v <*> pure defaultMyType
-- @
--
-- This instance will cause the usage of 'defaultMyType' as default
-- value if the default value that is given to the configuration
-- parser is 'Nothing' and the parsed configuration is not 'Null'.
--
instance (FromJSON (a -> a), FromJSON a) => FromJSON (Maybe a -> Maybe a) where

    -- | If the configuration explicitly requires 'Null' the result
    -- is 'Nothing'.
    --
    parseJSON Null = pure (const Nothing)

    -- | If the default value is @(Just x)@ and the configuration
    -- provides and update function @f@ then result is @Just f@.
    --
    -- If the default value is 'Nothing' and the configuration
    -- is parsed using a parser for a constant value (and not
    -- an update function).
    --
    parseJSON v = f <$> parseJSON v <*> parseJSON v
      where
        f g _ Nothing = Just g
        f _ g (Just x) = Just (g x)

