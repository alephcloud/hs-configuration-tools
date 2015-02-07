{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.ConfigFile
-- Description: Parsing of Configuration Files with Default Values
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides tools for defining configuration file
-- parsers via instances of 'FromJSON'.
--
-- Unlike /normal/ 'FromJSON' instances the parsers for configuration
-- files are expected to yield an update function that takes
-- a value and updates the value with the settings from the configuration
-- file.
--
-- Assuming that
--
-- * all configuration types are nested Haskell records or
--   simple types and
--
-- * that there are lenses for all record fields
--
-- usually the operators '..:' and '%.:' are all that is needed from this module.
--
-- The module "Configuration.Utils.Monoid" provides tools for the case that
-- a /simple type/ is a container with a monoid instance, such as @List@ or
-- @HashMap@.
--
-- The module "Configuration.Utils.Maybe" explains the usage of optional
-- 'Maybe' values in configuration types.
--
module Configuration.Utils.ConfigFile
(
-- * Parsing of Configuration Files with Default Values
  setProperty
, (..:)
, (!..:)
, updateProperty
, (%.:)

-- * Configuration File Parsing Policy
, ConfigFile(..)
, ConfigFilesConfig(..)
#if REMOTE_CONFIGS
, cfcHttpsPolicy
#endif
, defaultConfigFilesConfig
, pConfigFilesConfig

-- * Miscellaneous Utilities
, dropAndUncaml
, module Data.Aeson

-- * Internal Tools for Parsing Configuration Files
, parseConfigFiles
) where

import Configuration.Utils.CommandLine
import Configuration.Utils.Internal
import Configuration.Utils.Validation

import Control.Monad.Except hiding (mapM_)

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char
import Data.Foldable
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Yaml as Yaml

import Prelude hiding (concatMap, mapM_, any)

#ifdef REMOTE_CONFIGS
import Configuration.Utils.HttpsCertPolicy
import Configuration.Utils.Operators

import Control.Exception.Enclosed
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import qualified Data.Text.IO as T

import qualified Network.HTTP.Client as HTTP

import Prelude.Unicode

import System.IO
#endif

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

-- | A JSON parser for a function that modifies a property
-- of a given 'Object' and updates a setter with the parsed
-- function.
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
updateProperty
    ∷ Lens' α β
    → T.Text
    → (Value → Parser (β → β))
    → Object
    → Parser (α → α)
updateProperty s k p o = case H.lookup k o of
    Nothing → pure id
    Just v → over s <$> p v
{-# INLINE updateProperty #-}

-- | A variant of 'updateProperty' that used the 'FromJSON' instance
-- for the update function. It mimics the aeson operator '.:'.
-- It creates a parser that modifies a setter with a parsed function.
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
(%.:) s k = updateProperty s k parseJSON
infix 6 %.:
{-# INLINE (%.:) #-}

-- | This operator requires that a value is explicitly provided in a
-- configuration file, thus preventing the default value from being used.
-- Otherwise this operator does the same as '(..:)'.
--
(!..:)
    ∷ FromJSON β
    ⇒ Lens' α β
    → T.Text
    → Object
    → Parser (α → α)
(!..:) l property o = set l <$> (o .: property)
{-# INLINE (!..:) #-}

-- -------------------------------------------------------------------------- --
-- Config File Parsing Policy

data ConfigFile
    = ConfigFileRequired { getConfigFile ∷ !T.Text }
    | ConfigFileOptional { getConfigFile ∷ !T.Text }
    deriving (Show, Read, Eq, Ord, Typeable)

-- | An /internal/ type for the meta configuration that specifies how the
-- configuration files are loaded and parsed.
--
#if REMOTE_CONFIGS
data ConfigFilesConfig = ConfigFilesConfig
    { _cfcHttpsPolicy ∷ !HttpsCertPolicy
    }
    deriving (Show, Eq, Typeable)

cfcHttpsPolicy ∷ Lens' ConfigFilesConfig HttpsCertPolicy
cfcHttpsPolicy = lens _cfcHttpsPolicy $ \a b → a { _cfcHttpsPolicy = b }

defaultConfigFilesConfig ∷ ConfigFilesConfig
defaultConfigFilesConfig = ConfigFilesConfig
    { _cfcHttpsPolicy = defaultHttpsCertPolicy
    }

pConfigFilesConfig ∷ MParser ConfigFilesConfig
pConfigFilesConfig = id
    <$< cfcHttpsPolicy %:: pHttpsCertPolicy "config-"

#else

data ConfigFilesConfig = ConfigFilesConfig {}

defaultConfigFilesConfig ∷ ConfigFilesConfig
defaultConfigFilesConfig = ConfigFilesConfig {}

pConfigFilesConfig ∷ MParser ConfigFilesConfig
pConfigFilesConfig = pure id
#endif

-- -------------------------------------------------------------------------- --
-- Tools for parsing configuration files

#ifdef REMOTE_CONFIGS
type ConfigFileParser μ =
    ( Functor μ
    , Applicative μ
    , MonadIO μ
    , MonadBaseControl IO μ
    , MonadError T.Text μ
    )
#else
type ConfigFileParser μ =
    ( Functor μ
    , Applicative μ
    , MonadIO μ
    , MonadError T.Text μ
    )
#endif

parseConfigFiles
    ∷ (ConfigFileParser μ, FromJSON (α → α))
    ⇒ ConfigFilesConfig
    → α
        -- ^ default configuration value
    → [ConfigFile]
        -- ^ list of configuration file paths
    → μ α
parseConfigFiles conf = foldM $ \val file →
    readConfigFile conf file <*> pure val

readConfigFile
    ∷ (ConfigFileParser μ, FromJSON (α → α))
    ⇒ ConfigFilesConfig
    → ConfigFile
        -- ^ file path
    → μ (α → α)
readConfigFile _conf file =
#ifdef REMOTE_CONFIGS
    if isRemote file then loadRemote _conf file else loadLocal file
#else
    loadLocal file
#endif

loadLocal
    ∷ (Functor μ, MonadIO μ, MonadError T.Text μ, FromJSON (α → α))
    ⇒ ConfigFile
        -- ^ file path
    → μ (α → α)
loadLocal path = do
    validateFilePath "config-file" (T.unpack file)
    exists ← (True <$ validateFile "config-file" (T.unpack file)) `catchError` \e → case path of
        ConfigFileOptional _ → return False
        ConfigFileRequired _ → throwError $ "failed to read config file: " ⊕ e
    if exists
      then
        liftIO (Yaml.decodeFileEither (T.unpack file)) >>= \case
            Left e → throwError $ "failed to parse configuration file " ⊕ file ⊕ ": " ⊕ sshow e
            Right r → return r
      else
        return id
  where
    file = getConfigFile path

#ifdef REMOTE_CONFIGS
isRemote
    ∷ ConfigFile
    → Bool
isRemote path = L.any (`T.isPrefixOf` getConfigFile path) ["http://", "https://"]

loadRemote
    ∷ (ConfigFileParser μ, FromJSON (α → α))
    ⇒ ConfigFilesConfig
    → ConfigFile
        -- ^ URL
    → μ (α → α)
loadRemote conf path = do
    validateHttpOrHttpsUrl "config-file" (T.unpack url)
    dat ← (Just <$> doHttp) `catchAnyDeep` \e →
        case path of
            ConfigFileOptional _ → do
                liftIO ∘ T.hPutStrLn stderr $ "WARNING: failed to download remote configuration file " ⊕ url ⊕ ": " ⊕ sshow e
                return Nothing
            ConfigFileRequired _ → throwError $ "failed to download remote configuration file " ⊕ url ⊕ ": " ⊕ sshow e

    case dat of
        Nothing → return id
        Just d → case Yaml.decodeEither' d of
            Left e → throwError $ "failed to parse remote configuration " ⊕ url ⊕ ": " ⊕ sshow e
            Right r → return r
  where
    url = getConfigFile path
    policy = _cfcHttpsPolicy conf
    doHttp = LB.toStrict ∘ HTTP.responseBody <$> liftIO × simpleHttpWithValidationPolicy url policy
#endif

-- -------------------------------------------------------------------------- --
-- Miscellaneous Utilities

dropAndUncaml ∷ Int → String → String
dropAndUncaml i l
    | length l < i + 1 = l
    | otherwise = let (h:t) = drop i l
        in toLower h : concatMap (\x → if isUpper x then "-" ⊕ [toLower x] else [x]) t

