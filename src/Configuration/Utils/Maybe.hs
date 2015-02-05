{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Configuration.Utils.Maybe
-- Description: Configuration of Optional Values
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides tools for defining Maybe configuration types.
--
module Configuration.Utils.Maybe
(
-- * Simple Maybe Values
-- $simplemaybe

-- * Record Maybe Values
-- $recordmaybe
  maybeOption

) where

import Control.Applicative
import Data.Aeson

-- -------------------------------------------------------------------------- --
-- Simple Maybe Value

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
-- For 'Maybe' types that wrap product (record) types the following orphan 'FromJSON'
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

-- | Command line parser for record 'Maybe' values
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
        -- ^ the base value that is updated (usually the result of parsing the configuration file)
    → Maybe a
maybeOption _ False _ Nothing = Nothing -- not enabled
maybeOption defA True update Nothing = Just $ update defA -- disabled in config file but enabled by command line
maybeOption _ _ update (Just val) = Just $ update val -- enabled by config file and possibly by command line

