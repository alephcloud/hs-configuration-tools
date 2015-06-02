{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module: Configuration.Utils
-- Description: Utilities for Configuring Programs
-- Copyright: Copyright © 2014-2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides a collection of utilities on top of the packages
-- optparse-applicative, aeson, and yaml, for configuring libraries and
-- applications in a composable way.
--
-- The main feature is the integration of command line option parsing and
-- configuration files.
--
-- The purpose is to make management of configurations easy by providing an
-- idiomatic style of defining and deploying configurations in a modular
-- and composable way.
--
-- = Usage
--
-- The module provides operators and functions that make the implementation of
-- these entities easy for the common case that the configurations are encoded
-- mainly as nested records.
--
-- For each data type that is used as as component in a configuration type
-- the following must be provided:
--
-- 1. a /default value/,
--
-- 2. a /'FromJSON' instance/ that yields a function that takes a value and
--    updates that value with the parsed values,
--
-- 3. a /'ToJSON' instance/, and
--
-- 4. a /command line options parser/ that yields a function that takes a value
--    and updates that value with the values provided as command line options.
--
-- In addition to the above optionally a /validation function/ may be provided
-- that (recursively) validates a configuration value and returns either
-- an error or a (possibly empty) list-like structure of warnings.
--
-- The modules
--
-- * "Configuration.Utils.CommandLine",
-- * "Configuration.Utils.ConfigFile", and
-- * "Configuration.Utils.Operators"
--
-- contain tools and examples for defining above prerequisites for using a
-- type in a configuration type.
--
-- The provided functions and operators assume that lenses for the
-- configuration record types are provided.
--
-- The module "Configuration.Utils.Monoid" provides tools for the case that
-- a /simple type/ is a container with a monoid instance, such as @List@ or
-- @HashMap@.
--
-- The module "Configuration.Utils.Maybe" explains the usage of optional
-- 'Maybe' values in configuration types.
--
-- = Usage Example
--
-- Beside the examples that are provided in the haddock documentation there is
-- a complete usage example in the file
-- <https://github.com/alephcloud/hs-configuration-tools/blob/master/examples/Example.hs example/Example.hs>
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
, piConfigurationFiles

-- * Program Configuration with Validation of Configuration Values
, ConfigValidation
, programInfoValidate

-- * Running a Configured Application
, runWithConfiguration
, PkgInfo
, runWithPkgInfoConfiguration
, parseConfiguration

-- * Command Line Option Parsing with Default Values
, module Configuration.Utils.CommandLine

-- * Parsing of Configuration Files with Default Values
, module Configuration.Utils.ConfigFile

-- * Miscellaneous Utilities
, module Configuration.Utils.Operators
, Lens'
, Lens

-- * Configuration of Optional Values
, module Configuration.Utils.Maybe

-- * Configuration of Monoids
, module Configuration.Utils.Monoid

-- * Low-level Configuration Validation
, ProgramInfoValidate
, piValidateConfiguration
, ConfigValidationFunction(..)
, piOptionParserAndDefaultConfiguration
) where

import Configuration.Utils.CommandLine
import Configuration.Utils.ConfigFile
import qualified Configuration.Utils.Internal.ConfigFileReader as CF
import Configuration.Utils.Internal
import Configuration.Utils.Maybe
import Configuration.Utils.Monoid
import Configuration.Utils.Operators
import Configuration.Utils.Validation

import Control.Monad.Except hiding (mapM_)
import Control.Monad.Writer hiding (mapM_)

import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Data.Maybe
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml

import qualified Options.Applicative.Types as O

import qualified Options.Applicative as O

import Prelude hiding (concatMap, mapM_, any)
import Prelude.Unicode

import System.IO

import qualified Text.PrettyPrint.ANSI.Leijen as P

#ifdef REMOTE_CONFIGS
import Control.Monad.Trans.Control
#endif

-- -------------------------------------------------------------------------- --
-- Main Configuration

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
    , _piConfigurationFiles ∷ ![ConfigFile]
      -- ^ a list of configuration files that are loaded in order
      -- before any command line argument is evaluated.
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

-- | Configuration files that are loaded in order before any command line
-- argument is evaluated.
--
piConfigurationFiles ∷ Lens' (ProgramInfoValidate α λ) [ConfigFile]
piConfigurationFiles = lens _piConfigurationFiles $ \s a → s { _piConfigurationFiles = a }
{-# INLINE piConfigurationFiles #-}

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
    , _piConfigurationFiles = _piConfigurationFiles s
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
    , _piConfigurationFiles = []
    }

-- -------------------------------------------------------------------------- --
-- AppConfiguration

-- | An /internal/ data type that is used during configuration parsing to
-- represent the overall application configuration which includes
--
-- 1. the /user/ configuration, which is actual configuration value that
--    is given to the application and
--
-- 2. the /meta/ configuration, which are all settings that determine how the
--    actual /user/ configuration is loaded and parsed.
--
-- NOTE that /meta/ configuration settings can only be provided via command
-- line options but not through configuration files.
--
data AppConfiguration α = AppConfiguration
    { _printConfig ∷ !Bool
    , _configFilesConfig ∷ !ConfigFilesConfig
    , _configFiles ∷ ![ConfigFile]
    , _mainConfig ∷ !α
    }

-- | A flag that indicates that the application should output the effective
-- configuration and exit.
--
printConfig ∷ Lens' (AppConfiguration α) Bool
printConfig = lens _printConfig $ \s a → s { _printConfig = a }

-- | The 'ConfigFilesConfig' collects all parameters that determine how
-- configuration files are loaded and parsed.
--
configFilesConfig ∷ Lens' (AppConfiguration α) ConfigFilesConfig
configFilesConfig = lens _configFilesConfig $ \s a → s { _configFilesConfig = a }

-- | A list of configuration file locations. Configuration file locations are
-- set either statically in the code or are provided dynamically on the command
-- line via @--config-file@ options.
--
configFiles ∷ Lens' (AppConfiguration α) [ConfigFile]
configFiles = lens _configFiles $ \s a → s { _configFiles = a }

-- | The /user/ configuration. During parsing this is represented as an update
-- function that yields a configuration value when applied to a default
-- value.
--
mainConfig ∷ Lens (AppConfiguration α) (AppConfiguration β) α β
mainConfig = lens _mainConfig $ \s a → s { _mainConfig = a }

-- | This function parsers /all/ command line options:
--
-- 1. 'ConfigFilesConfig' options that determine how configuration
--    files are loaded.
--
-- 2. 'ConfigFiles' options are all @--config-file@ options.
--
-- 3. Other /meta/ options, such as @--print-config@.
--
-- 4. Options for the actual user /configuration/. The user configuration
--    is represented as an update function that yields a configuration
--    value when applied to an default value.
--
pAppConfiguration
    ∷ O.Parser (α → α)
    → O.Parser (AppConfiguration (α → α))
pAppConfiguration mainParser = AppConfiguration
    <$> pPrintConfig
    <*> (pConfigFilesConfig <*> pure defaultConfigFilesConfig)
    <*> many pConfigFile
    <*> mainParser
  where
    pPrintConfig = O.switch
        × O.long "print-config"
        ⊕ O.short 'p'
        ⊕ O.help "Print the parsed configuration to standard out and exit"
        ⊕ O.showDefault

    pConfigFile = ConfigFileRequired ∘ T.pack <$> O.strOption
        × O.long "config-file"
        ⊕ O.short 'c'
        ⊕ O.metavar "FILE"
        ⊕ O.help "Configuration file in YAML or JSON format. If more than a single config file option is present files are loaded in the order in which they appear on the command line."

-- -------------------------------------------------------------------------- --
-- Main Configuration without Package Info

-- | Run an IO action with a configuration that is obtained by updating the
-- given default configuration the values defined via command line arguments.
--
-- In addition to the options defined by the given options parser the following
-- options are recognized:
--
-- [@--config-file, -c@]
--     Parse the given file path as a (partial) configuration in YAML or JSON
--     format.
--
-- [@--print-config, -p@]
--     Print the final parsed configuration to standard out and exit.
--
-- [@--help, -h@]
--     Print a help message and exit.
--
-- As long as the package wasn't build with @-f-remote-configs@ the following
-- two options are available. They affect how configuration files
-- are loaded from remote URLs.
--
-- [@--config-https-insecure=true|false@]
--     Bypass certificate validation for all HTTPS
--     connections to all services.
--
-- [@--config-https-allow-cert=HOSTNAME:PORT:FINGERPRINT@]
--     Unconditionally trust the certificate for connecting
--     to the service.
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
runWithConfiguration appInfo = runInternal appInfo Nothing

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
--     Parse the given file path as a (partial) configuration in YAML or JSON
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
--     Print the text of the license of the application and exit.
--
-- As long as the package wasn't build with @-f-remote-configs@ the following
-- two options are available. They affect how configuration files
-- are loaded from remote URLs.
--
-- [@--config-https-insecure=true|false@]
--     Bypass certificate validation for all HTTPS
--     connections to all services.
--
-- [@--config-https-allow-cert=HOSTNAME:PORT:FINGERPRINT@]
--     Unconditionally trust the certificate for connecting
--     to the service.
--
runWithPkgInfoConfiguration
    ∷ (FromJSON (α → α), ToJSON α, Foldable λ, Monoid (λ T.Text))
    ⇒ ProgramInfoValidate α λ
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → PkgInfo
        -- 'PkgInfo' value that contains information about the package.
        --
        -- See the documentation of "Configuration.Utils.Setup" for a way
        -- how to generate this information automatically from the package
        -- description during the build process.
    → (α → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runWithPkgInfoConfiguration appInfo pkgInfo =
    runInternal appInfo (Just $ pPkgInfo pkgInfo)

-- -------------------------------------------------------------------------- --
-- Internal main function

mainOptions
    ∷ ∀ α λ . FromJSON (α → α)
    ⇒ ProgramInfoValidate α λ
        -- ^ Program Info value which may include a validation function

    → (∀ β . Maybe (MParser β))
        -- ^ Maybe a package info parser. This parser is run only for its
        -- side effects. It is supposed to /intercept/ the parsing process
        -- and execute any implied action (showing help messages).

    → O.ParserInfo (AppConfiguration (α → α))
mainOptions ProgramInfo{..} pkgInfoParser = O.info optionParser
    $ O.progDesc _piDescription
    ⊕ O.fullDesc
    ⊕ maybe mempty O.header _piHelpHeader
    ⊕ O.footerDoc (Just $ defaultFooter ⊕ maybe mempty P.text _piHelpFooter)
  where
    optionParser =
        -- these are identity parsers that are only applied for their side effects
        fromMaybe (pure id) pkgInfoParser <*> nonHiddenHelper
        -- this parser produces the results
        <*> pAppConfiguration _piOptionParser

    -- the 'O.helper' option from optparse-applicative is hidden by default
    -- which seems a bit weired. This option doesn't hide the access to help.
    nonHiddenHelper = abortOption ShowHelpText
        × long "help"
        ⊕ short 'h'
        ⊕ short '?'
        ⊕ help "Show this help message"

    defaultFooter = P.vsep
        [ par "Configurations are loaded in order from the following sources:"
        , P.indent 2 ∘ P.vsep $ zipWith ($) (catMaybes [staticFiles, cmdFiles, cmdOptions]) [1..]
        , ""
        , P.fillSep
            [ par "Configuration file locations can be either local file system paths"
            , par "or remote HTTP or HTTPS URLs. Remote URLs must start with"
            , par "either \"http://\" or \"https://\"."
            ]
        , ""
        , P.fillSep
            [ par "Configuration settings that are loaded later overwrite settings"
            , par "that were loaded before."
            ]
        , ""
        ]

    staticFiles
        | null _piConfigurationFiles = Nothing
        | otherwise = Just $ \n → P.hang 3 $ P.vsep
            [ P.int n ⊕ "." P.</> par "Configuration files at the following locations:"
            , P.vsep $ map (\f → "* " ⊕ printConfigFile f) _piConfigurationFiles
            ]
    cmdFiles = Just $ \n → P.hang 3 $ P.fillSep
        [ P.int n ⊕ "." P.</> par "Configuration files from locations provided through"
        , par "--config-file options in the order as they appear."
        ]
    cmdOptions = Just $ \n → P.hang 3
        $ P.int n ⊕ "." P.</> par "Command line options."

    printConfigFile f = P.text (T.unpack $ getConfigFile f) P.<+> case f of
        ConfigFileRequired _ → P.text "(required)"
        ConfigFileOptional _ → P.text "(optional)"

    par ∷ String → P.Doc
    par = P.fillSep ∘ map P.string ∘ words

-- | Internal main function
--
runInternal
    ∷ (FromJSON (α → α), ToJSON α, Foldable λ, Monoid (λ T.Text))
    ⇒ ProgramInfoValidate α λ
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → (∀ β . Maybe (MParser β))
        -- 'PkgInfo' value that contains information about the package.
        --
        -- See the documentation of "Configuration.Utils.Setup" for a way
        -- how to generate this information automatically from the package
        -- description during the build process.
    → (α → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runInternal appInfo maybePkgInfo mainFunction = do

    -- Parse command line arguments and add static config files to resulting app config
    cliAppConf ← configFiles `over` (⊕) (_piConfigurationFiles appInfo) <$>
        O.customExecParser parserPrefs (mainOptions appInfo maybePkgInfo)

    -- Load and parse all configuration files
    appConf ← cliAppConf & mainConfig `id` \a → a <$> errorT × CF.parseConfigFiles
        (_configFilesConfig cliAppConf)
        (_piDefaultConfiguration appInfo)
        (_configFiles cliAppConf)

    -- Validate final configuration
    validateConfig appInfo $ _mainConfig appConf

    if _printConfig appConf
        then B8.putStrLn ∘ Yaml.encode ∘ _mainConfig $ appConf
        else mainFunction ∘ _mainConfig $ appConf
  where
    parserPrefs = O.prefs O.disambiguate

-- | Parse the command line arguments.
--
-- Any warnings from the configuration function are discarded.
-- The options @--print-config@ and @--help@ are just ignored.
--
parseConfiguration
    ∷
        ( Applicative m
        , MonadIO m
#ifdef REMOTE_CONFIGS
        , MonadBaseControl IO m
#endif
        , MonadError T.Text m
        , FromJSON (α → α)
        , ToJSON α
        , Foldable λ
        , Monoid (λ T.Text)
        )
    ⇒ T.Text
        -- ^ program name (used in error messages)
    → ProgramInfoValidate α λ
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → [String]
        -- ^ command line arguments
    → m α
parseConfiguration appName appInfo args = do

    -- Parse command line arguments (add static config files to resulting app config)
    cliAppConf ← case O.execParserPure parserPrefs (mainOptions appInfo Nothing) args of
        O.Success a → return $ a & configFiles `over` (⊕) (_piConfigurationFiles appInfo)
        O.Failure e → throwError ∘ T.pack ∘ fst $ renderFailure e (T.unpack appName)
        O.CompletionInvoked _ → throwError "command line parser returned completion result"

    -- Load and parse all configuration files
    appConf ← cliAppConf & mainConfig `id` \a → a <$> CF.parseConfigFiles
        (_configFilesConfig cliAppConf)
        (_piDefaultConfiguration appInfo)
        (_configFiles cliAppConf)

    -- Validate final configuration
    void ∘ validate appInfo $ _mainConfig appConf

    return $ _mainConfig appConf
  where
    parserPrefs = O.prefs O.disambiguate
    validate i conf = runWriterT $
        runConfigValidation (view piValidateConfiguration i) conf

-- -------------------------------------------------------------------------- --
-- Validation

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
    warnings ← execWriterT ∘ exceptT (error ∘ T.unpack) return $
        runConfigValidation (view piValidateConfiguration appInfo) conf
    when (any (const True) warnings) $ do
        T.hPutStrLn stderr "WARNINGS:"
        mapM_ (\w → T.hPutStrLn stderr $ "warning: " ⊕ w) warnings

