{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Configuration.Utils.Internal
import Configuration.Utils.Internal.JsonTools
import qualified Configuration.Utils.Internal.ConfigFileReader as CF
import Configuration.Utils.Maybe
import Configuration.Utils.Monoid
import Configuration.Utils.Operators
import Configuration.Utils.Validation

import Control.Monad (void, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Writer (runWriterT)
import Control.Monad.IO.Class (MonadIO)

import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import Data.Maybe
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml

import qualified Options.Applicative.Types as O

import qualified Options.Applicative as O

import Prelude hiding (any, concatMap, mapM_)
import Prelude.Unicode hiding ((×))

import System.IO

import qualified Prettyprinter as P

#ifdef REMOTE_CONFIGS
import Control.Monad.Trans.Control
#endif

-- -------------------------------------------------------------------------- --
-- Main Configuration

-- | A newtype wrapper around a validation function. The only purpose of
-- this type is to avoid @ImpredicativeTypes@ when storing the function
-- in the 'ProgramInfoValidate' record.
--
newtype ConfigValidationFunction a f r = ConfigValidationFunction
    { runConfigValidation ∷ ConfigValidation' a f r
    }

type ProgramInfo a = ProgramInfoValidate a []

data ProgramInfoValidate' a f r = ProgramInfo
    { _piDescription ∷ !String
      -- ^ Program Description
    , _piHelpHeader ∷ !(Maybe String)
      -- ^ Help header
    , _piHelpFooter ∷ !(Maybe String)
      -- ^ Help footer
    , _piOptionParser ∷ !(MParser a)
      -- ^ options parser for configuration
    , _piDefaultConfiguration ∷ !a
      -- ^ default configuration
    , _piValidateConfiguration ∷ !(ConfigValidationFunction a f r)
      -- ^ a validation function. The 'Right' result is interpreted as a 'Foldable'
      -- structure of warnings.
    , _piConfigurationFiles ∷ ![ConfigFile]
      -- ^ a list of configuration files that are loaded in order
      -- before any command line argument is evaluated.
    }

type ProgramInfoValidate a f = ProgramInfoValidate' a f a

-- | Program Description
--
piDescription ∷ Lens' (ProgramInfoValidate' a f r) String
piDescription = lens _piDescription $ \s a → s { _piDescription = a }
{-# INLINE piDescription #-}

-- | Help header
--
piHelpHeader ∷ Lens' (ProgramInfoValidate' a f r) (Maybe String)
piHelpHeader = lens _piHelpHeader $ \s a → s { _piHelpHeader = a }
{-# INLINE piHelpHeader #-}

-- | Help footer
--
piHelpFooter ∷ Lens' (ProgramInfoValidate' a f r) (Maybe String)
piHelpFooter = lens _piHelpFooter $ \s a → s { _piHelpFooter = a }
{-# INLINE piHelpFooter #-}

-- | Options parser for configuration
--
piOptionParser ∷ Lens' (ProgramInfoValidate' a f r) (MParser a)
piOptionParser = lens _piOptionParser $ \s a → s { _piOptionParser = a }
{-# INLINE piOptionParser #-}

-- | Default configuration
--
piDefaultConfiguration ∷ Lens' (ProgramInfoValidate' a f r) a
piDefaultConfiguration = lens _piDefaultConfiguration $ \s a → s { _piDefaultConfiguration = a }
{-# INLINE piDefaultConfiguration #-}

-- | Validation Function
--
-- The 'Right' result is interpreted as a 'Foldable' structure of warnings.
--
piValidateConfiguration ∷ Lens' (ProgramInfoValidate' a f r) (ConfigValidationFunction a f r)
piValidateConfiguration = lens _piValidateConfiguration $ \s a → s { _piValidateConfiguration = a }
{-# INLINE piValidateConfiguration #-}

-- | Configuration files that are loaded in order before any command line
-- argument is evaluated.
--
piConfigurationFiles ∷ Lens' (ProgramInfoValidate a f) [ConfigFile]
piConfigurationFiles = lens _piConfigurationFiles $ \s a → s { _piConfigurationFiles = a }
{-# INLINE piConfigurationFiles #-}

-- | 'Lens' for simultaneous query and update of 'piOptionParser' and
-- 'piDefaultConfiguration'. This supports to change the type of 'ProgramInfo'
-- with 'over' and 'set'.
--
piOptionParserAndDefaultConfiguration
    ∷ Lens
        (ProgramInfoValidate' a b r)
        (ProgramInfoValidate' c d r')
        (MParser a, a, ConfigValidationFunction a b r)
        (MParser c, c, ConfigValidationFunction c d r')
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
    → MParser a
        -- ^ parser for updating the default configuration
    → a
        -- ^ default configuration
    → ProgramInfo a
programInfo desc parser defaultConfig =
    programInfoValidate desc parser defaultConfig $ const (return ())

-- | Smart constructor for 'ProgramInfo'.
--
-- 'piHelpHeader' and 'piHelpFooter' are set to 'Nothing'.
--
programInfoValidate'
    ∷ String
    → MParser a
    → a
    → ConfigValidation' a f r
    → ProgramInfoValidate' a f r
programInfoValidate' desc parser defaultConfig valFunc = ProgramInfo
    { _piDescription = desc
    , _piHelpHeader = Nothing
    , _piHelpFooter = Nothing
    , _piOptionParser = parser
    , _piDefaultConfiguration = defaultConfig
    , _piValidateConfiguration = ConfigValidationFunction valFunc
    , _piConfigurationFiles = []
    }

-- | Smart constructor for 'ProgramInfo'.
--
-- 'piHelpHeader' and 'piHelpFooter' are set to 'Nothing'.
--
programInfoValidate
    ∷ String
    → MParser a
    → a
    → ConfigValidation a f
    → ProgramInfoValidate a f
programInfoValidate desc parser defaultConfig valFunc =
    programInfoValidate' desc parser defaultConfig $ \c -> valFunc c >> return c

-- -------------------------------------------------------------------------- --
-- AppConfiguration

data PrintConfigMode = Full | Minimal | Diff

printConfigModeToText ∷ PrintConfigMode → T.Text
printConfigModeToText Full = "full"
printConfigModeToText Minimal = "minimal"
printConfigModeToText Diff = "diff"

printConfigModeFromText ∷ T.Text → Either String PrintConfigMode
printConfigModeFromText t = case CI.mk t of
    "full" → Right Full
    "minimal" → Right Minimal
    "diff" → Right Diff
    x → Left $ "unknow print configuration mode: " <> sshow x

instance ToJSON PrintConfigMode where
    toJSON = toJSON ∘ printConfigModeToText
    {-# INLINE toJSON #-}

instance FromJSON PrintConfigMode where
    parseJSON = withText "PrintConfigMode"
        $ either fail return ∘ printConfigModeFromText
    {-# INLINE parseJSON #-}

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
data AppConfiguration a = AppConfiguration
    { _printConfig ∷ !(Maybe PrintConfigMode)
    , _configFilesConfig ∷ !ConfigFilesConfig
    , _configFiles ∷ ![ConfigFile]
    , _mainConfig ∷ !a
    }
    deriving Functor

-- | A list of configuration file locations. Configuration file locations are
-- set either statically in the code or are provided dynamically on the command
-- line via @--config-file@ options.
--
configFiles ∷ Lens' (AppConfiguration a) [ConfigFile]
configFiles = lens _configFiles $ \s a → s { _configFiles = a }

-- | The /user/ configuration. During parsing this is represented as an update
-- function that yields a configuration value when applied to a default
-- value.
--
mainConfig ∷ Lens (AppConfiguration a) (AppConfiguration b) a b
mainConfig = lens _mainConfig $ \s a → s { _mainConfig = a }

-- | This function parsers /all/ command line options:
--
-- 1. 'ConfigFilesConfig' options that determine how configuration
--    files are loaded.
--
-- 2. 'ConfigFiles' options are all @--config-file@ options.
--
-- 3. Other /meta/ options, such as @--print-config@ and @--printconfig-as@.
--
-- 4. Options for the actual user /configuration/. The user configuration
--    is represented as an update function that yields a configuration
--    value when applied to an default value.
--
pAppConfiguration
    ∷ O.Parser (a → a)
    → O.Parser (AppConfiguration (a → a))
pAppConfiguration mainParser = AppConfiguration
    <$> pPrintConfig
    <*> (pConfigFilesConfig <*> pure defaultConfigFilesConfig)
    <*> many pConfigFile
    <*> mainParser
  where
    pConfigFile = ConfigFileRequired ∘ T.pack <$> O.strOption
        % O.long "config-file"
        ⊕ O.metavar "FILE"
        ⊕ O.help "Configuration file in YAML or JSON format. If more than a single config file option is present files are loaded in the order in which they appear on the command line."

    pPrintConfig
        = Just <$> pPrintConfigOption
        <|> Just <$> pPrintConfigFlag
        <|> pure Nothing

    pPrintConfigFlag = O.flag' Full
        % O.long "print-config"
        ⊕ O.help "Print the parsed configuration to standard out and exit. This is an alias for --print-config-as=full"

    pPrintConfigOption = O.option (eitherReader $ printConfigModeFromText . T.pack)
        % O.long "print-config-as"
        ⊕ O.help "Print the parsed configuration to standard out and exit"
        ⊕ O.completeWith ["full", "minimal", "diff", "Full", "Minimal", "Diff"]
        ⊕ O.metavar "full|minimal|diff"

-- -------------------------------------------------------------------------- --
-- Main Configuration without Package Info

-- | Run an IO action with a configuration that is obtained by updating the
-- given default configuration the values defined via command line arguments.
--
-- In addition to the options defined by the given options parser the following
-- options are recognized:
--
-- [@--config-file@]
--     Parse the given file path as a (partial) configuration in YAML or JSON
--     format.
--
-- [@--print-config@]
--     Print the final parsed configuration to standard out and exit.
--
-- [@--print-config-as (full|minimal|diff)@]
--     Configures the application and prints the configuration in YAML format to
--     standard out and exits. The printed configuration is exactly the
--     configuration that otherwise would be used to run the application.
--
--     Arguments:
--
--     *   @full@: print the complete configuration. Same as @--print-config@.
--     *   @minimal@: print a minimal configuration that contains only those
--         settings that are different from the default setting.
--     *   @diff@: print a YAML document that shows the difference between the
--         default configuration and the actual configuration.
--
-- [@--help, -h, -?@]
--     Print a help message and exit.
--
-- If the package is build with @-f+remote-configs@ the following two options
-- are available. They affect how configuration files are loaded from remote
-- URLs.
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
    ∷ (FromJSON (a → a), ToJSON a, Foldable f, Monoid (f T.Text))
    ⇒ ProgramInfoValidate' a f r
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → (r → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runWithConfiguration appInfo = runInternal appInfo Nothing

-- -------------------------------------------------------------------------- --
-- Main Configuration with Package Info

pPkgInfo ∷ PkgInfo → MParser a
pPkgInfo (sinfo, detailedInfo, version, license) =
    infoO <*> detailedInfoO <*> versionO <*> licenseO
  where
    infoO = infoOption sinfo
        $ O.long "info"
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
-- See the documentation of "Configuration.Utils.Setup" for a way how to
-- generate this information automatically from the package description during
-- the build process.
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
-- [@--print-config-as (full|minimal|diff)@]
--     Configures the application and prints the configuration in YAML format to
--     standard out and exits. The printed configuration is exactly the
--     configuration that otherwise would be used to run the application.
--
--     Arguments:
--
--     *   @full@: print the complete configuration. Same as @--print-config@.
--     *   @minimal@: print a minimal configuration that contains only those
--         settings that are different from the default setting.
--     *   @diff@: print a YAML document that shows the difference between the
--         default configuration and the actual configuration.
--
-- [@--help, -h, -?@]
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
-- If the package is build with @-f+remote-configs@ the following two options
-- are available. They affect how configuration files are loaded from remote
-- URLs.
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
    ∷ (FromJSON (a → a), ToJSON a, Foldable f, Monoid (f T.Text))
    ⇒ ProgramInfoValidate a f
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → PkgInfo
        -- 'PkgInfo' value that contains information about the package.
        --
        -- See the documentation of "Configuration.Utils.Setup" for a way
        -- how to generate this information automatically from the package
        -- description during the build process.
    → (a → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runWithPkgInfoConfiguration appInfo pkgInfo =
    runInternal appInfo (Just $ pPkgInfo pkgInfo)

-- -------------------------------------------------------------------------- --
-- Internal main function

mainOptions
    ∷ ∀ a f r . FromJSON (a → a)
    ⇒ ProgramInfoValidate' a f r
        -- ^ Program Info value which may include a validation function

    → (∀ b . Maybe (MParser b))
        -- ^ Maybe a package info parser. This parser is run only for its
        -- side effects. It is supposed to /intercept/ the parsing process
        -- and execute any implied action (showing help messages).

    → O.ParserInfo (AppConfiguration (a → a))
mainOptions ProgramInfo{..} pkgInfoParser = O.info optionParser
    $ O.progDesc _piDescription
    ⊕ O.fullDesc
    ⊕ maybe mempty O.header _piHelpHeader
    ⊕ O.footerDoc (Just $ defaultFooter ⊕ maybe mempty P.pretty _piHelpFooter)
  where
    optionParser =
        -- these are identity parsers that are only applied for their side effects
        fromMaybe (pure id) pkgInfoParser <*> nonHiddenHelper
        -- this parser produces the results
        <*> pAppConfiguration _piOptionParser

    -- the 'O.helper' option from optparse-applicative is hidden by default
    -- which seems a bit weired. This option doesn't hide the access to help.
    nonHiddenHelper = abortOption (ShowHelpText Nothing)
        % long "help"
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

    a </> b = a <> P.softline <> b

    staticFiles
        | null _piConfigurationFiles = Nothing
        | otherwise = Just $ \n → P.hang 3 $ P.vsep
            [ P.pretty @Int n ⊕ "." </> par "Configuration files at the following locations:"
            , P.vsep $ map (\f → "* " ⊕ printConfigFile f) _piConfigurationFiles
            ]
    cmdFiles = Just $ \n → P.hang 3 $ P.fillSep
        [ P.pretty n ⊕ "." </> par "Configuration files from locations provided through"
        , par "--config-file options in the order as they appear."
        ]
    cmdOptions = Just $ \n → P.hang 3
        $ P.pretty n ⊕ "." </> par "Command line options."

    printConfigFile f = P.pretty (getConfigFile f) P.<+> case f of
        ConfigFileRequired _ → "(required)"
        ConfigFileOptional _ → "(optional)"

    par = P.fillSep ∘ map P.pretty ∘ words

-- | Internal main function
--
runInternal
    ∷ (FromJSON (a → a), ToJSON a, Foldable f, Monoid (f T.Text))
    ⇒ ProgramInfoValidate' a f r
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → (∀ b . Maybe (MParser b))
        -- 'PkgInfo' value that contains information about the package.
        --
        -- See the documentation of "Configuration.Utils.Setup" for a way
        -- how to generate this information automatically from the package
        -- description during the build process.
    → (r → IO ())
        -- ^ computation that is given the configuration that is parsed from
        -- the command line.
    → IO ()
runInternal appInfo maybePkgInfo mainFunction = do

    -- Parse command line arguments and add static config files to resulting app config
    cliAppConf ← configFiles `over` (⊕) (_piConfigurationFiles appInfo) <$>
        O.customExecParser parserPrefs (mainOptions appInfo maybePkgInfo)

    -- Load and parse all configuration files
    appConf ← cliAppConf & mainConfig `id` \a → a <$> errorT % CF.parseConfigFiles
        (_configFilesConfig cliAppConf)
        (_piDefaultConfiguration appInfo)
        (_configFiles cliAppConf)

    -- Validate final configuration
    validatedConf ← validateConfig appInfo $ _mainConfig appConf

    case _printConfig appConf of
        Nothing → mainFunction ∘ _mainConfig $ validatedConf <$ appConf
        Just Full → B8.putStrLn ∘ Yaml.encode ∘ _mainConfig $ appConf
        Just Minimal → B8.putStrLn
            ∘ Yaml.encode
            ∘ resolve resolveOnlyRight
            ∘ diff (toJSON $ _piDefaultConfiguration appInfo)
            ∘ toJSON
            ∘ _mainConfig
            $ appConf
        Just Diff → B8.putStrLn
            ∘ Yaml.encode
            ∘ diff (toJSON $ _piDefaultConfiguration appInfo)
            ∘ toJSON
            ∘ _mainConfig
            $ appConf
  where
    parserPrefs = O.prefs mempty


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
        , FromJSON (a → a)
        , ToJSON a
        , Foldable f
        , Monoid (f T.Text)
        )
    ⇒ T.Text
        -- ^ program name (used in error messages)
    → ProgramInfoValidate' a f r
        -- ^ program info value; use 'programInfo' to construct a value of this
        -- type
    → [String]
        -- ^ command line arguments
    → m a
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
    ∷ (Foldable f, Monoid (f T.Text))
    ⇒ ProgramInfoValidate' a f r
    → a
    → IO r
validateConfig appInfo conf = do
    (r, warnings) ← runWriterT ∘ exceptT (error ∘ T.unpack) return $
        runConfigValidation (view piValidateConfiguration appInfo) conf
    when (any (const True) warnings) $ do
        T.hPutStrLn stderr "WARNINGS:"
        mapM_ (\w → T.hPutStrLn stderr $ "warning: " ⊕ w) warnings
    return r
