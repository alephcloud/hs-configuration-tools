{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.Internal.ConfigFileReader
-- Description: Internal Tools for Parsing Configuration Files
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
module Configuration.Utils.Internal.ConfigFileReader
(
  parseConfigFiles
, readConfigFile
, ConfigFileFormat(..)

-- * Local Config Files
, loadLocal

#ifdef REMOTE_CONFIGS
-- * Remote Config Files
, isRemote
, loadRemote
, yamlMimeType
, jsonMimeType
, contentType
, requestHeaders
#endif
) where

import Configuration.Utils.ConfigFile
import Configuration.Utils.Internal
import Configuration.Utils.Validation

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Except hiding (mapM_)

import Data.Monoid.Unicode
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Yaml as Yaml

import GHC.Generics

import Prelude hiding (concatMap, mapM_, any)

#ifdef REMOTE_CONFIGS
import Configuration.Utils.Internal.HttpsCertPolicy

import Control.Exception.Enclosed
import Control.Monad.Trans.Control

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.String
import qualified Data.Text.IO as T

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP

import Prelude.Unicode

import System.IO
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

data ConfigFileFormat
    = Yaml
    | Json
    | Other
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

instance NFData ConfigFileFormat

#ifdef REMOTE_CONFIGS
isRemote
    ∷ ConfigFile
    → Bool
isRemote path = L.any (`T.isPrefixOf` getConfigFile path) ["http://", "https://"]

yamlMimeType ∷ IsString s ⇒ [s]
yamlMimeType = map fromString ["application/x-yaml", "text/yaml"]

-- | Defined in  RFC 4627
--
jsonMimeType ∷ IsString s ⇒ [s]
jsonMimeType = map fromString ["application/json"]

contentType
    ∷ B8.ByteString
        -- ^ value of an HTTP @Content-Type@ header
    → ConfigFileFormat
contentType headerValue
    | CI.foldCase "yaml" `B8.isInfixOf` CI.foldCase headerValue = Yaml
    | CI.foldCase "json" `B8.isInfixOf` CI.foldCase headerValue = Json
    | otherwise = Other

loadRemote
    ∷ (ConfigFileParser μ, FromJSON (α → α))
    ⇒ ConfigFilesConfig
    → ConfigFile
        -- ^ URL
    → μ (α → α)
loadRemote conf path = do
    validateHttpOrHttpsUrl "config-file" (T.unpack url)
    result ← (Just <$> doHttp) `catchAnyDeep` \e →
        case path of
            ConfigFileOptional _ → do
                liftIO ∘ T.hPutStrLn stderr $ "WARNING: failed to download remote configuration file " ⊕ url ⊕ ": " ⊕ sshow e
                return Nothing
            ConfigFileRequired _ → throwError $ "failed to download remote configuration file " ⊕ url ⊕ ": " ⊕ sshow e

    case result of
        Nothing → return id
        Just (format, d) → case (parser format) d of
            Left e → throwError $ "failed to parse remote configuration " ⊕ url ⊕ ": " ⊕ e
            Right r → return r
  where
    parser Json = fmapL T.pack ∘ eitherDecodeStrict'
    parser _ = fmapL sshow ∘ Yaml.decodeEither'

    url = getConfigFile path
    policy = _cfcHttpsPolicy conf
    doHttp = liftIO $ do
        request ← (HTTP.parseUrl $ T.unpack url)
            <&> over requestHeaders ((:) acceptHeader)
        resp ← httpWithValidationPolicy request policy
        let format = maybe Other contentType ∘ L.lookup HTTP.hContentType $ HTTP.responseHeaders resp
        return (format, LB.toStrict (HTTP.responseBody resp))

    acceptHeader = (HTTP.hAccept, B8.intercalate "," (yamlMimeType ⊕ jsonMimeType))

requestHeaders ∷ Lens' HTTP.Request HTTP.RequestHeaders
requestHeaders = lens HTTP.requestHeaders $ \s a → s { HTTP.requestHeaders = a }

#endif

