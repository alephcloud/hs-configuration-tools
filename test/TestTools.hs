{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: TestTools
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- TODO
--
module TestTools
(
-- * Very Simple Debugging
  enableDebug
, debug

-- * Configuration Assertions for Testing
, ConfAssertion(..)
, trueLens
, trueAssertion

-- * Test Execution
, check
, runTest

-- * Test Config Files
, withConfigFile
, withConfigFileText
#ifdef REMOTE_CONFIGS
, ConfigType(..)
, serverUrl
, withConfigFileServer
#endif
) where

import Configuration.Utils
import Configuration.Utils.Internal

import Control.Exception
import Control.Monad

import qualified Data.ByteString.Char8 as B8
import Data.IORef
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml

import Distribution.Simple.Utils (withTempFile)

import Prelude.Unicode

import System.Environment
import System.IO

#ifdef REMOTE_CONFIGS
import Control.Concurrent
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as WARP
import qualified Network.HTTP.Types as HTTP
#endif

-- -------------------------------------------------------------------------- --
-- Very Simple Debugging

enableDebug ∷ Bool
enableDebug = False

debug
    ∷ Monad m
    ⇒ m ()
    → m ()
debug a
    | enableDebug = a
    | otherwise = return ()

-- -------------------------------------------------------------------------- --
-- Configuration Assertions for Testing

-- | Specify a assertion about the parsed configuration
--
-- The parameters are
--
-- 1. list of command line arguments,
-- 2. lens for the configuration value
-- 3. the expected value
--
data ConfAssertion β = ∀ α . Eq α ⇒ ConfAssertion [String] (Lens' β α) α

trueLens ∷ Lens' β ()
trueLens = lens (const ()) const

trueAssertion ∷ [String] → ConfAssertion β
trueAssertion args = ConfAssertion args trueLens ()

-- -------------------------------------------------------------------------- --
-- Test execution

-- Check the given list of assertions for the given configuration value
--
check
    ∷ α
    → [ConfAssertion α]
    → IO Bool
check conf assertions =
    foldM (\a (b,n) → (&& a) <$> go b n) True $ zip assertions [0 ∷ Int ..]
  where
    go (ConfAssertion _ l v) n =
        if view l conf ≡ v
          then do
            debug ∘ T.putStrLn $ "DEBUG: assertion " ⊕ sshow n ⊕ " succeeded"
            return True
          else do
            debug ∘ T.putStrLn $ "DEBUG: assertion " ⊕ sshow n ⊕ " failed"
            return False

-- | Run a test with an expected outcome ('True' or 'False')
-- for a given that of assertions.
--
runTest
    ∷ (FromJSON (α → α), ToJSON α)
    ⇒ PkgInfo
    → ProgramInfoValidate α []
    → T.Text
        -- ^ label for the test case
    → Bool
        -- ^ expected outcome
    → [ConfAssertion α]
        -- ^ test assertions
    → IO Bool
runTest pkgInfo mInfo label succeed assertions = do

    debug ∘ T.putStrLn $ "\nDEBUG: ======> " ⊕ label

    debug ∘ T.putStrLn $ "DEBUG: runWithPkgInfoConfiguration"
    a ← run $ runWithPkgInfoConfiguration mInfo pkgInfo

    debug ∘ T.putStrLn $ "DEBUG: runWithConfiguration"
    b ← run $ runWithConfiguration mInfo

    if a ≡ b && succeed ≡ (a && b)
      then
        return True
      else do
        T.putStrLn $ "WARNING: test " ⊕ label ⊕ " failed"
        return False
  where
    run f = do
        ref ← newIORef False
        handle (handler ref) $ withArgs args ∘ f $ \conf →
            writeIORef ref =<< check conf assertions
        readIORef ref

    args = concatMap (\(ConfAssertion x _ _) → x) assertions

    handler ref (e ∷ SomeException) = do
        writeIORef ref False
        debug ∘ T.putStrLn $ "DEBUG: caugth exception: " ⊕ sshow e

-- -------------------------------------------------------------------------- --
-- Test Config Files
--

withConfigFile
    ∷ ToJSON γ
    ⇒ γ
    → (T.Text → IO α)
    → IO α
withConfigFile config inner =
    withTempFile "." "tmp_TestExample.yml" $ \tmpPath tmpHandle → do
        B8.hPutStrLn tmpHandle ∘ Yaml.encode $ config
        hClose tmpHandle
        inner $ T.pack tmpPath

withConfigFileText
    ∷ T.Text
    → (T.Text → IO α)
    → IO α
withConfigFileText configText inner =
    withTempFile "." "tmp_TestExample.yml" $ \tmpPath tmpHandle → do
        T.hPutStrLn tmpHandle configText
        hClose tmpHandle
        inner $ T.pack tmpPath


#ifdef REMOTE_CONFIGS
data ConfigType = ∀ γ . ToJSON γ ⇒ ConfigType γ

instance ToJSON ConfigType where
    toJSON (ConfigType a) = toJSON a

withConfigFileServer
    ∷ [(T.Text, ConfigType)]
    → [(T.Text, T.Text)]
    → IO α
    → IO α
withConfigFileServer configs configTexts inner = do
    void ∘ forkIO $ server serverPort configs configTexts
    inner

serverPort ∷ Int
serverPort = 8283

serverUrl ∷ T.Text
serverUrl = "http://127.0.0.1:" ⊕ sshow serverPort

server
    ∷ Int
    → [(T.Text, ConfigType)]
    → [(T.Text, T.Text)]
    → IO ()
server port configs configTexts = WARP.run port $ \req respond → do
    let maybeBody = LB.fromStrict <$> do
            p ← listToMaybe $ WAI.pathInfo req
            do
                Yaml.encode <$> lookup p configs
                <|>
                (T.encodeUtf8 <$> lookup p configTexts)

    respond $ case maybeBody of
        Just body → WAI.responseLBS HTTP.status200 [] body
        Nothing → WAI.responseLBS HTTP.status404 [] "resource not found"
#endif
