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
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class

import Data.Bifunctor
import qualified Data.ByteString.Char8 as B8
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import GHC.Generics

import Prelude hiding (any, concatMap, mapM_)
import Prelude.Unicode

#ifdef REMOTE_CONFIGS
import Configuration.Utils.Internal.HttpsCertPolicy

import Control.Exception.Enclosed
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.String
import qualified Data.Text.IO as T

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP

import System.IO
#endif

-- -------------------------------------------------------------------------- --
-- Tools for parsing configuration files

#ifdef REMOTE_CONFIGS
type ConfigFileParser m =
    ( Functor m
    , Applicative m
    , MonadIO m
    , MonadBaseControl IO m
    , MonadError T.Text m
    )
#else
type ConfigFileParser m =
    ( Functor m
    , Applicative m
    , MonadIO m
    , MonadError T.Text m
    )
#endif

parseConfigFiles
    ∷ (ConfigFileParser m, FromJSON (a → a))
    ⇒ ConfigFilesConfig
    → a
        -- ^ default configuration value
    → [ConfigFile]
        -- ^ list of configuration file paths
    → m a
parseConfigFiles conf = foldM $ \val file →
    readConfigFile conf file <*> pure val

readConfigFile
    ∷ (ConfigFileParser m, FromJSON (a → a))
    ⇒ ConfigFilesConfig
    → ConfigFile
        -- ^ file path
    → m (a → a)
readConfigFile _conf file =
#ifdef REMOTE_CONFIGS
    if isRemote file then loadRemote _conf file else loadLocal file
#else
    loadLocal file
#endif

fileType ∷ T.Text → ConfigFileFormat
fileType f
    | ".yaml" `T.isSuffixOf` T.toLower f = Yaml
    | ".yml" `T.isSuffixOf` T.toLower f = Yaml
    | ".json" `T.isSuffixOf` T.toLower f = Json
    | ".js" `T.isSuffixOf` T.toLower f = Json
    | otherwise = Other

loadLocal
    ∷ (Functor m, MonadIO m, MonadError T.Text m, FromJSON (a → a))
    ⇒ ConfigFile
        -- ^ file path
    → m (a → a)
loadLocal path = do
    validateFilePath "config-file" (T.unpack file)
    exists ← (True <$ validateFile "config-file" (T.unpack file)) `catchError` \e → case path of
        ConfigFileOptional _ → return False
        ConfigFileRequired _ → throwError $ "failed to read config file: " ⊕ e
    if exists
      then
        liftIO (parser (fileType file) file) >>= \case
            Left e → throwError $ "failed to parse configuration file " ⊕ file ⊕ ": " ⊕ sshow e
            Right r → return r
      else
        return id
  where
    file = getConfigFile path

    parser Json f = first T.pack ∘ eitherDecodeStrict' <$> B8.readFile (T.unpack f)
    parser _ f = first sshow <$> Yaml.decodeFileEither (T.unpack f)

data ConfigFileFormat
    = Yaml
    | Json
    | Other
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

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
    ∷ (ConfigFileParser m, FromJSON (a → a))
    ⇒ ConfigFilesConfig
    → ConfigFile
        -- ^ URL
    → m (a → a)
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
    parser Json = first T.pack ∘ eitherDecodeStrict'
    parser _ = first sshow ∘ Yaml.decodeEither'

    url = getConfigFile path
    policy = _cfcHttpsPolicy conf
    doHttp = liftIO $ do
        request ← (HTTP.parseUrlThrow $ T.unpack url)
            <&> over requestHeaders ((:) acceptHeader)
        resp ← httpWithValidationPolicy request policy
        let format = maybe Other contentType ∘ L.lookup HTTP.hContentType $ HTTP.responseHeaders resp
        return (format, LB.toStrict (HTTP.responseBody resp))

    acceptHeader = (HTTP.hAccept, B8.intercalate "," (yamlMimeType ⊕ jsonMimeType))

requestHeaders ∷ Lens' HTTP.Request HTTP.RequestHeaders
requestHeaders = lens HTTP.requestHeaders $ \s a → s { HTTP.requestHeaders = a }

#endif
