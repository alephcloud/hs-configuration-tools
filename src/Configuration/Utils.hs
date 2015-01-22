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
{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK show-extensions #-}

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
-- In addition to the above optionally a validation function may be provided
-- that (recursively) validates a configuration value and returns either
-- an error or a --possibly empty-- list-like structure of warnings.
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

-- * Program Configurations with Validation of Configuration Values
, ConfigValidation
, programInfoValidate

-- ** Low-level Config Validation
, ProgramInfoValidate
, piValidateConfiguration
, ConfigValidationFunction
, piOptionParserAndDefaultConfiguration

-- * Running an Configured Application
, runWithConfiguration
, PkgInfo
, runWithPkgInfoConfiguration

-- * Applicative Option Parsing with Default Values
, MParser
, (.::)
, (%::)
, boolReader
, boolOption
, fileOption
, eitherReadP
, module Options.Applicative

-- * Parsing of Configuration Files with Default Values
, setProperty
, (..:)
, (%.:)
, module Data.Aeson

-- * Command Line Option Parsing
-- * Misc Utils
, (%)
, (×)
, (<*<)
, (>*>)
, (<$<)
, (>$>)
, (<.>)
, (⊙)
, dropAndUncaml
, Lens'
, Lens

-- * Configuration of Optional Values

-- * Simple Maybe Values
-- $simplemaybe

-- **  Record Maybe Values
-- $recordmaybe
, maybeOption
) where

import Configuration.Utils.Internal

import Control.Error (fmapL)
import Control.Monad.Except hiding (mapM_)
import Control.Monad.Writer hiding (mapM_)

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml

#if MIN_VERSION_optparse_applicative(0,10,0)
import Options.Applicative hiding (Parser, Success)
#else
import Options.Applicative hiding (Parser, Success, (&))
#endif

import qualified Options.Applicative as O

import Prelude hiding (concatMap, mapM_, any)
import Prelude.Unicode

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import qualified Text.ParserCombinators.ReadP as P

-- -------------------------------------------------------------------------- --
-- Useful Operators

-- | This operator is an alternative for '$' with a higher precedence. It is
-- suitable for usage within applicative style code without the need to add
-- parenthesis.
--
(%) ∷ (α → β) → α → β
(%) = ($)
infixr 5 %
{-# INLINE (%) #-}

-- | This operator is a UTF-8 version of '%' which is an alternative for '$'
-- with a higher precedence. It is suitable for usage within applicative style
-- code without the need to add parenthesis.
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
(<*<) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(<*<) a b = pure (.) <*> a <*> b
infixr 4 <*<
{-# INLINE (<*<) #-}

-- | Functional composition for applicative functors with its arguments
-- flipped.
--
(>*>) ∷ Applicative φ ⇒ φ (α → β) → φ (β → γ) → φ (α → γ)
(>*>) = flip (<*<)
infixr 4 >*>
{-# INLINE (>*>) #-}

-- | Applicative functional composition between a pure function
-- and an applicative function.
--
(<$<) ∷ Functor φ ⇒ (β → γ) → φ (α → β) → φ (α → γ)
(<$<) a b = (a .) <$> b
infixr 4 <$<
{-# INLINE (<$<) #-}

-- | Applicative functional composition between a pure function
-- and an applicative function with its arguments flipped.
--
(>$>) ∷ Functor φ ⇒ φ (α → β) → (β → γ) → φ (α → γ)
(>$>) = flip (<$<)
infixr 4 >$>
{-# INLINE (>$>) #-}

-- | Functional composition for applicative functors.
--
-- This is a rather popular operator. Due to conflicts (for instance with the
-- lens package) it may have to be imported qualified.
--
(<.>) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(<.>) = (<*<)
infixr 4 <.>
{-# INLINE (<.>) #-}
{-# DEPRECATED (<.>) "use '<*<' instead" #-}

-- | For people who like nicely aligned code and do not mind messing with
-- editor key-maps: here a version of '<.>' that uses a unicode symbol
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
{-# DEPRECATED (⊙) "use '<*<' instead" #-}

-- -------------------------------------------------------------------------- --
-- Applicative Option Parsing with Default Values

-- | An operator for applying a setter to an option parser that yields a value.
--
-- Example usage:
--
-- > data Auth = Auth
-- >     { _user ∷ !String
-- >     , _pwd ∷ !String
-- >     }
-- >
-- > user ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- > user f s = (\u → s { _user = u }) <$> f (_user s)
-- >
-- > pwd ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- > pwd f s = (\p → s { _pwd = p }) <$> f (_pwd s)
-- >
-- > -- or with lenses and TemplateHaskell just:
-- > -- $(makeLenses ''Auth)
-- >
-- > pAuth ∷ MParser Auth
-- > pAuth = id
-- >    <$< user .:: strOption
-- >        × long "user"
-- >        ⊕ short 'u'
-- >        ⊕ help "user name"
-- >    <*< pwd .:: strOption
-- >        × long "pwd"
-- >        ⊕ help "password for user"
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
-- > data HttpURL = HttpURL
-- >     { _auth ∷ !Auth
-- >     , _domain ∷ !String
-- >     }
-- >
-- > auth ∷ Functor φ ⇒ (Auth → φ Auth) → HttpURL → φ HttpURL
-- > auth f s = (\u → s { _auth = u }) <$> f (_auth s)
-- >
-- > domain ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- > domain f s = (\u → s { _domain = u }) <$> f (_domain s)
-- >
-- > path ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- > path f s = (\u → s { _path = u }) <$> f (_path s)
-- >
-- > -- or with lenses and TemplateHaskell just:
-- > -- $(makeLenses ''HttpURL)
-- >
-- > pHttpURL ∷ MParser HttpURL
-- > pHttpURL = id
-- >     <$< auth %:: pAuth
-- >     <*< domain .:: strOption
-- >         × long "domain"
-- >         ⊕ short 'd'
-- >         ⊕ help "HTTP domain"
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

-- | A JSON 'Value' parser for a property of a given
-- 'Object' that updates a setter with the parsed value.
--
-- > data Auth = Auth
-- >     { _userId ∷ !Int
-- >     , _pwd ∷ !String
-- >     }
-- >
-- > userId ∷ Functor φ ⇒ (Int → φ Int) → Auth → φ Auth
-- > userId f s = (\u → s { _userId = u }) <$> f (_userId s)
-- >
-- > pwd ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- > pwd f s = (\p → s { _pwd = p }) <$> f (_pwd s)
-- >
-- > -- or with lenses and TemplateHaskell just:
-- > -- $(makeLenses ''Auth)
-- >
-- > instance FromJSON (Auth → Auth) where
-- >     parseJSON = withObject "Auth" $ \o → id
-- >         <$< setProperty user "user" p o
-- >         <*< setProperty pwd "pwd" parseJSON o
-- >       where
-- >         p = withText "user" $ \case
-- >             "alice" → pure (0 ∷ Int)
-- >             "bob" → pure 1
-- >             e → fail $ "unrecognized user " ⊕ e
--
setProperty
    ∷ Lens' α β -- ^ a lens into the target that is updated by the parser
    → T.Text -- ^ the JSON property name
    → (Value → Parser β) -- ^ the JSON 'Value' parser that is used to parse the value of the property
    → Object -- ^ the parsed JSON 'Value' 'Object'
    → Parser (α → α)
setProperty s k p o = case H.lookup k o of
    Nothing → pure id
    Just v → set s <$> p v

-- | A variant of the 'setProperty' that uses the default 'parseJSON' method from the
-- 'FromJSON' instance to parse the value of the property. Its usage pattern mimics the
-- usage pattern of the '.:' operator from the aeson library.
--
-- > data Auth = Auth
-- >     { _user ∷ !String
-- >     , _pwd ∷ !String
-- >     }
-- >
-- > user ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- > user f s = (\u → s { _user = u }) <$> f (_user s)
-- >
-- > pwd ∷ Functor φ ⇒ (String → φ String) → Auth → φ Auth
-- > pwd f s = (\p → s { _pwd = p }) <$> f (_pwd s)
-- >
-- > -- or with lenses and TemplateHaskell just:
-- > -- $(makeLenses ''Auth)
-- >
-- > instance FromJSON (Auth → Auth) where
-- >     parseJSON = withObject "Auth" $ \o → id
-- >         <$< user ..: "user" × o
-- >         <*< pwd ..: "pwd" × o
--
(..:) ∷ FromJSON β ⇒ Lens' α β → T.Text → Object → Parser (α → α)
(..:) s k = setProperty s k parseJSON
infix 6 ..:
{-# INLINE (..:) #-}

-- | A variant of the aeson operator '.:' that creates a parser
-- that modifies a setter with a parsed function.
--
-- > data HttpURL = HttpURL
-- >     { _auth ∷ !Auth
-- >     , _domain ∷ !String
-- >     }
-- >
-- > auth ∷ Functor φ ⇒ (Auth → φ Auth) → HttpURL → φ HttpURL
-- > auth f s = (\u → s { _auth = u }) <$> f (_auth s)
-- >
-- > domain ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- > domain f s = (\u → s { _domain = u }) <$> f (_domain s)
-- >
-- > path ∷ Functor φ ⇒ (String → φ String) → HttpURL → φ HttpURL
-- > path f s = (\u → s { _path = u }) <$> f (_path s)
-- >
-- > -- or with lenses and TemplateHaskell just:
-- > -- $(makeLenses ''HttpURL)
-- >
-- > instance FromJSON (HttpURL → HttpURL) where
-- >     parseJSON = withObject "HttpURL" $ \o → id
-- >         <$< auth %.: "auth" × o
-- >         <*< domain ..: "domain" × o
--
(%.:) ∷ FromJSON (β → β) ⇒ Lens' α β → T.Text → Object → Parser (α → α)
(%.:) s k o = case H.lookup k o of
    Nothing → pure id
    Just v → over s <$> parseJSON v
infix 6 %.:
{-# INLINE (%.:) #-}

-- -------------------------------------------------------------------------- --
-- Command Line Option Parsing

boolReader
    ∷ (Eq a, Show a, CI.FoldCase a, IsString a, IsString e, Monoid e)
    ⇒ a
    → Either e Bool
boolReader x = case CI.mk x of
    "true" → Right True
    "false" → Right False
    _ → Left $ "failed to read Boolean value " <> fromString (show x)
        <> ". Expected either \"true\" or \"false\""

-- | The 'boolOption' is an alternative to 'O.switch'.
--
-- Using 'O.switch' with command line parsers that overwrite settings
-- from a configuration file is problematic: the absence of the 'switch'
-- is interpreted as setting the respective configuration value to 'False'.
-- So there is no way to specify on the command line that the value from
-- the configuration file shall be used. Some command line UIs use two
-- different options for those values, for instance @--enable-feature@ and
-- @--disable-feature@. This option instead expects a Boolean value. Beside
-- that it behaves like any other option.
--
boolOption
    ∷ O.Mod O.OptionFields Bool
    → O.Parser Bool
#if MIN_VERSION_optparse_applicative(0,10,0)
boolOption mods = O.option (O.eitherReader boolReader)
    % O.metavar "TRUE|FALSE"
    <> O.completeWith ["true", "false", "TRUE", "FALSE", "True", "False"]
    <> mods
#else
boolOption mods = O.nullOption
    % metavar "TRUE|FALSE"
    <> O.completeWith ["true", "false", "TRUE", "FALSE", "True", "False"]
    <> O.eitherReader boolReader
    <> mods
#endif

fileOption
    ∷ O.Mod O.OptionFields String
    → O.Parser FilePath
fileOption mods = O.strOption
    % O.metavar "FILE"
    <> O.action "file"
    <> mods

eitherReadP
    ∷ T.Text
    → P.ReadP a
    → T.Text
    → Either T.Text a
eitherReadP label p s =
    case [ x | (x,"") ← P.readP_to_S p (T.unpack s) ] of
        [x] → Right x
        []  → Left $ "eitherReadP: no parse for " <> label <> " of " <> s
        _  → Left $ "eitherReadP: ambigous parse for " <> label <> " of " <> s

-- -------------------------------------------------------------------------- --
-- Main Configuration

-- | A validation function. The type in the 'MonadWriter' is excpected to
-- be a 'Foldable' structure for collecting warnings.
--
type ConfigValidation α λ = (MonadIO μ, Functor μ, Applicative μ, MonadError T.Text μ, MonadWriter (λ T.Text) μ) ⇒ α → μ ()

-- | A newtype wrapper around a validation function. The only purpose of
-- this type is to avoid @ImpredicativeTypes@ when storing the function
-- in the 'ProgramInfoValidate' record.
--
newtype ConfigValidationFunction α λ = ConfigValidationFunction
    { runConfigValidation ∷ ConfigValidation α λ
    }

type ProgramInfo α = ProgramInfoValidate α []

data ProgramInfoValidate α λ = ProgramInfo
    { _piDescription ∷ !String
      -- ^ Program Description
    , _piHelpHeader ∷ !(Maybe String)
      -- ^ Help header
    , _piHelpFooter ∷ !(Maybe String)
      -- ^ Help footer
    , _piOptionParser ∷ !(MParser α)
      -- ^ options parser for configuration
    , _piDefaultConfiguration ∷ !α
      -- ^ default configuration
    , _piValidateConfiguration ∷ !(ConfigValidationFunction α λ)
      -- ^ a validation function. The 'Right' result is interpreted as a 'Foldable'
      -- structure of warnings.
    }

-- | Program Description
--
piDescription ∷ Lens' (ProgramInfoValidate α λ) String
piDescription = lens _piDescription $ \s a → s { _piDescription = a }
{-# INLINE piDescription #-}

-- | Help header
--
piHelpHeader ∷ Lens' (ProgramInfoValidate α λ) (Maybe String)
piHelpHeader = lens _piHelpHeader $ \s a → s { _piHelpHeader = a }
{-# INLINE piHelpHeader #-}

-- | Help footer
--
piHelpFooter ∷ Lens' (ProgramInfoValidate α λ) (Maybe String)
piHelpFooter = lens _piHelpFooter $ \s a → s { _piHelpFooter = a }
{-# INLINE piHelpFooter #-}

-- | Options parser for configuration
--
piOptionParser ∷ Lens' (ProgramInfoValidate α λ) (MParser α)
piOptionParser = lens _piOptionParser $ \s a → s { _piOptionParser = a }
{-# INLINE piOptionParser #-}

-- | Default configuration
--
piDefaultConfiguration ∷ Lens' (ProgramInfoValidate α λ) α
piDefaultConfiguration = lens _piDefaultConfiguration $ \s a → s { _piDefaultConfiguration = a }
{-# INLINE piDefaultConfiguration #-}

-- | Validation Function
--
-- The 'Right' result is interpreted as a 'Foldable' structure of warnings.
--
piValidateConfiguration ∷ Lens' (ProgramInfoValidate α λ) (ConfigValidationFunction α λ)
piValidateConfiguration = lens _piValidateConfiguration $ \s a → s { _piValidateConfiguration = a }
{-# INLINE piValidateConfiguration #-}

-- | 'Lens' for simultaneous query and update of 'piOptionParser' and
-- 'piDefaultConfiguration'. This supports to change the type of 'ProgramInfo'
-- with 'over' and 'set'.
--
piOptionParserAndDefaultConfiguration
    ∷ Lens
        (ProgramInfoValidate α λ)
        (ProgramInfoValidate β γ)
        (MParser α, α, ConfigValidationFunction α λ)
        (MParser β, β, ConfigValidationFunction β γ)
piOptionParserAndDefaultConfiguration = lens g $ \s (a,b,c) → ProgramInfo
    { _piDescription = _piDescription s
    , _piHelpHeader = _piHelpHeader s
    , _piHelpFooter = _piHelpFooter s
    , _piOptionParser = a
    , _piDefaultConfiguration = b
    , _piValidateConfiguration = c
    }
  where
    g s = (_piOptionParser s, _piDefaultConfiguration s, _piValidateConfiguration s)
{-# INLINE piOptionParserAndDefaultConfiguration #-}

-- | Smart constructor for 'ProgramInfo'.
--
-- 'piHelpHeader' and 'piHelpFooter' are set to 'Nothing'.
-- The function 'piValidateConfiguration' is set to @const (return [])@
--
programInfo
    ∷ String
        -- ^ program description
    → MParser α
        -- ^ parser for updating the default configuration
    → α
        -- ^ default configuration
    → ProgramInfo α
programInfo desc parser defaultConfig =
    programInfoValidate desc parser defaultConfig $ const (return ())

-- | Smart constructor for 'ProgramInfo'.
--
-- 'piHelpHeader' and 'piHelpFooter' are set to 'Nothing'.
--
programInfoValidate
    ∷ String
    → MParser α
    → α
    → ConfigValidation α λ
    → ProgramInfoValidate α λ
programInfoValidate desc parser defaultConfig valFunc = ProgramInfo
    { _piDescription = desc
    , _piHelpHeader = Nothing
    , _piHelpFooter = Nothing
    , _piOptionParser = parser
    , _piDefaultConfiguration = defaultConfig
    , _piValidateConfiguration = ConfigValidationFunction valFunc
    }

data AppConfiguration α = AppConfiguration
    { _printConfig ∷ !Bool
    , _mainConfig ∷ !α
    }

-- | A flag that indicates that the application should
-- output the effective configuration and exit.
--
printConfig ∷ Lens' (AppConfiguration α) Bool
printConfig = lens _printConfig $ \s a → s { _printConfig = a }

-- | The configuration value that is given to the
-- application.
--
mainConfig ∷ Lens' (AppConfiguration α) α
mainConfig = lens _mainConfig $ \s a → s { _mainConfig = a }

pAppConfiguration ∷ (FromJSON (α → α)) ⇒ α → O.Parser (AppConfiguration α)
pAppConfiguration d = AppConfiguration
    <$> O.switch
        × O.long "print-config"
        ⊕ O.short 'p'
        ⊕ O.help "Print the parsed configuration to standard out and exit"
        ⊕ O.showDefault
#if MIN_VERSION_optparse_applicative(0,10,0)
    <*> O.option (O.eitherReader $ \file → fileReader file <*> pure d)
        × O.long "config-file"
        ⊕ O.short 'c'
        ⊕ O.metavar "FILE"
        ⊕ O.help "Configuration file in YAML format"
        ⊕ O.value d
#else
    <*> O.nullOption
        × O.long "config-file"
        ⊕ O.short 'c'
        ⊕ O.metavar "FILE"
        ⊕ O.help "Configuration file in YAML format"
        ⊕ O.eitherReader (\file → fileReader file <*> pure d)
        ⊕ O.value d
#endif
  where
    fileReader file = fmapL (\e → "failed to parse configuration file " ⊕ file ⊕ ": " ⊕ show e)
        $ unsafePerformIO (Yaml.decodeFileEither file)

mainOptions
    ∷ ∀ α λ . FromJSON (α → α)
    ⇒ ProgramInfoValidate α λ
    → (∀ β . Maybe (MParser β))
    → O.ParserInfo (AppConfiguration α)
mainOptions ProgramInfo{..} pkgInfoParser = O.info optionParser
    $ O.progDesc _piDescription
    ⊕ O.fullDesc
    ⊕ maybe mempty O.header _piHelpHeader
    ⊕ maybe mempty O.footer _piHelpFooter
  where
    optionParser = fromMaybe (pure id) pkgInfoParser
        <*> nonHiddenHelper
        <*> (over mainConfig <$> _piOptionParser)
        <*> pAppConfiguration _piDefaultConfiguration

    -- the 'O.helper' option from optparse-applicative is hidden be default
    -- which seems a bit weired. This option doesn't hide the access to help.
    nonHiddenHelper = abortOption ShowHelpText
        × long "help"
        ⊕ short 'h'
        ⊕ short '?'
        ⊕ help "Show this help text"

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
    ∷ (FromJSON (α → α), ToJSON α, Foldable λ, Monoid (λ T.Text))
    ⇒ ProgramInfoValidate α λ
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → (α → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runWithConfiguration appInfo mainFunction = do
    conf ← O.customExecParser parserPrefs mainOpts
    validateConfig appInfo $ _mainConfig conf
    if _printConfig conf
        then B8.putStrLn ∘ Yaml.encode ∘ _mainConfig $ conf
        else mainFunction ∘ _mainConfig $ conf
  where
    mainOpts = mainOptions appInfo Nothing
    parserPrefs = O.prefs O.disambiguate

-- | Validates a configuration value. Throws an user error
-- if there is an error. If there are warnings they are
-- printed to 'stderr'.
--
validateConfig
    ∷ (Foldable λ, Monoid (λ T.Text))
    ⇒ ProgramInfoValidate α λ
    → α
    → IO ()
validateConfig appInfo conf = do
    warnings ← execWriterT . exceptT (error . T.unpack) return $ do
        runConfigValidation (view piValidateConfiguration appInfo) conf
    when (any (const True) warnings) $ do
        T.hPutStrLn stderr "WARNINGS:"
        mapM_ (\w → T.hPutStrLn stderr $ "warning: " ⊕ w) warnings

exceptT
    ∷ Monad μ
    ⇒ (ε → μ β)
    → (α → μ β)
    → ExceptT ε μ α
    → μ β
exceptT a b = runExceptT >=> either a b

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

-- | Information about the cabal package. The format is:
--
-- @(info message, detailed info message, version string, license text)@
--
-- See the documentation of "Configuration.Utils.Setup" for a way
-- how to generate this information automatically from the package
-- description during the build process.
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
-- [@--long-info@]
--     Print a detailed info message for the application and exit.
--
-- [@--license@]
--     Print the text of the lincense of the application and exit.
--
runWithPkgInfoConfiguration
    ∷ (FromJSON (α → α), ToJSON α, Foldable λ, Monoid (λ T.Text))
    ⇒ ProgramInfoValidate α λ
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → PkgInfo
        -- 'PkgInfo' value that contain information about the package.
        --
        -- See the documentation of "Configuration.Utils.Setup" for a way
        -- how to generate this information automatically from the package
        -- description during the build process.
    → (α → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runWithPkgInfoConfiguration appInfo pkgInfo mainFunction = do
    conf ← O.customExecParser parserPrefs mainOpts
    validateConfig appInfo $ _mainConfig conf
    if _printConfig conf
        then B8.putStrLn ∘ Yaml.encode ∘ _mainConfig $ conf
        else mainFunction ∘ _mainConfig $ conf
  where
    mainOpts = mainOptions appInfo (Just $ pPkgInfo pkgInfo)
    parserPrefs = O.prefs O.disambiguate

-- -------------------------------------------------------------------------- --
-- Configuration of Optional Values

-- $simplemaybe
-- Optional configuration values are supposed to be encoded by wrapping
-- the respective type with 'Maybe'.
--
-- For simple values the standard 'FromJSON' instance from the aeson
-- package can be used along with the '..:' operator.
--
-- > data LogConfig = LogConfig
-- >    { _logLevel ∷ !Int
-- >    , _logFile ∷ !(Maybe String)
-- >    }
-- >
-- > $(makeLenses ''LogConfig)
-- >
-- > defaultLogConfig ∷ LogConfig
-- > defaultLogConfig = LogConfig
-- >     { _logLevel = 1
-- >     , _logFile = Nothing
-- >     }
-- >
-- > instance FromJSON (LogConfig → LogConfig) where
-- >     parseJSON = withObject "LogConfig" $ \o → id
-- >         <$< logLevel ..: "LogLevel" % o
-- >         <*< logFile ..: "LogConfig" % o
-- >
-- > instance ToJSON LogConfig where
-- >     toJSON config = object
-- >         [ "LogLevel" .= _logLevel config
-- >         , "LogConfig" .= _logFile config
-- >         ]
-- >
--
-- When defining command line option parsers with '.::' and '%::' all
-- options are optional. When an option is not present on the command
-- line the default value is used. For 'Maybe' values it is therefore
-- enough to wrap the parsed value into 'Just'.
--
-- > pLogConfig ∷ MParser LogConfig
-- > pLogConfig = id
-- > #if MIN_VERSION_optparse-applicative(0,10,0)
-- >     <$< logLevel .:: option auto
-- > #else
-- >     <$< logLevel .:: option
-- > #endif
-- >         % long "log-level"
-- >         % metavar "INTEGER"
-- >         % help "log level"
-- >     <*< logFile .:: fmap Just % strOption
-- >         % long "log-file"
-- >         % metavar "FILENAME"
-- >         % help "log file name"
--

-- $recordmaybe
--
-- For product-type (record) 'Maybe' values the following orphan 'FromJSON'
-- instance is provided:
--
-- > instance (FromJSON (a → a), FromJSON a) ⇒ FromJSON (Maybe a → Maybe a)
-- >     parseJSON Null = pure (const Nothing)
-- >     parseJSON v = f <$> parseJSON v <*> parseJSON v
-- >       where
-- >         f g _ Nothing = Just g
-- >         f _ g (Just x) = Just (g x)
--
-- (Using an orphan instance is generally problematic but convenient in
-- this case. It's unlikely that an instance for this type is needed elsewhere.
-- If this is an issue for you, please let me know. In that case we can define a
-- new type for optional configuration values.)
--
-- The semantics are as follows:
--
-- * If the parsed configuration value is 'Null' the result is 'Nothing'.
-- * If the parsed configuration value is not 'Null' then the result is
--   an update function that
--
--     * updates the given default value if this value is @Just x@
--       or
--     * is a constant function that returns the value that is parsed
--       from the configuration using the 'FromJSON' instance for the
--       configuration type.
--
-- Note, that this instance requires an 'FromJSON' instance for the
-- configuration type itself as well as a 'FromJSON' instance for the update
-- function of the configuration type. The former can be defined by means of the
-- latter as follows:
--
-- > instance FromJSON MyType where
-- >     parseJSON v = parseJSON v <*> pure defaultMyType
--
-- This instance will cause the usage of 'defaultMyType' as default value if the
-- default value that is given to the configuration parser is 'Nothing' and the
-- parsed configuration is not 'Null'.
--
instance (FromJSON (a → a), FromJSON a) ⇒ FromJSON (Maybe a → Maybe a) where

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

-- | Commandline parser for record 'Maybe' values
--
-- == Example:
--
-- > data Setting = Setting
-- >     { _setA ∷ !Int
-- >     , _setB ∷ !String
-- >     }
-- >     deriving (Show, Read, Eq, Ord, Typeable)
-- >
-- > $(makeLenses ''Setting)
-- >
-- > defaultSetting ∷ Setting
-- > defaultSetting = Setting
-- >     { _setA = 0
-- >     , _setB = 1
-- >     }
-- >
-- > instance ToJSON Setting where
-- >     toJSON setting = object
-- >        [ "a" .= _setA setting
-- >        , "b" .= _setB setting
-- >        ]
-- >
-- > instance FromJSON (Setting → Setting) where
-- >     parseJSON = withObject "Setting" $ \o → id
-- >         <$< setA ..: "a" % o
-- >         <*< setB ..: "b" % o
-- >
-- > instance FromJSON Setting where
-- >    parseJSON v = parseJSON v <*> pure defaultSetting
-- >
-- > pSetting ∷ MParser Setting
-- > pSetting = id
-- >     <$< setA .:: option auto
-- >         % short 'a'
-- >         <> metavar "INT"
-- >         <> help "set a"
-- >     <*< setB .:: option auto
-- >         % short 'b'
-- >         <> metavar "INT"
-- >         <> help "set b"
-- >
-- > -- | Use 'Setting' as 'Maybe' in a configuration:
-- > --
-- > data Config = Config
-- >     { _maybeSetting ∷ !(Maybe Setting)
-- >     }
-- >     deriving (Show, Read, Eq, Ord, Typeable)
-- >
-- > $(makeLenses ''Config)
-- >
-- > defaultConfig ∷ Config
-- > defaultConfig = Config
-- >     { _maybeSetting = defaultSetting
-- >     }
-- >
-- > instance ToJSON Config where
-- >     toJSON config = object
-- >         [ "setting" .= maybeSetting
-- >         ]
-- >
-- > instance FromJSON (Config → Config) where
-- >     parseJSON = withObject "Config" $ \o → id
-- >         <$< maybeSetting %.: "setting" % o
-- >
-- > pConfig ∷ MParser Config
-- > pConfig = id
-- >     <$< maybeSetting %:: (maybeOption defaultSetting
-- >         <$> pEnableSetting
-- >         <*> pSetting)
-- >   where
-- >     pEnableSetting = boolOption
-- >         % long "setting-enable"
-- >         <> value False
-- >         <> help "Enable configuration flags for setting"
--
maybeOption
    ∷ a
        -- ^ default value that is used if base configuration is 'Nothing'
    → Bool
        -- ^ whether to enable this parser or not (usually is a boolean option parser)
    → (a → a)
        -- ^ update function (usually given as applicative 'MParser a')
    → Maybe a
        -- ^ the base value that is updated (usually the result of parsing the configuraton file)
    → Maybe a
maybeOption _ False _ Nothing = Nothing -- not enabled
maybeOption defA True update Nothing = Just $ update defA -- disabled in config file but enabled by command line
maybeOption _ _ update (Just val) = Just $ update val -- enabled by config file and possibly by command line

