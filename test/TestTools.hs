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
-- Description: Tools for testing program configurations
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- Tools for testing program configurations
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

-- * Test Configuration Files
, withConfigFile
, withConfigFileText
#ifdef REMOTE_CONFIGS
, ConfigType(..)
, serverUrl
, serverTlsUrl
, withConfigFileServer
#endif
) where

import Configuration.Utils
import Configuration.Utils.Internal
import Configuration.Utils.Internal.ConfigFileReader

import Control.Exception
import Control.Monad

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml

import Distribution.Simple.Utils (withTempFile)

#if MIN_VERSION_base(4,13,0)
import Prelude.Unicode hiding ((×))
#else
import Prelude.Unicode
#endif

import System.Environment
import System.IO

#ifdef REMOTE_CONFIGS
import Control.Concurrent
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as WARP
import qualified Network.Wai.Handler.WarpTLS as WARP
import qualified Network.HTTP.Types as HTTP
import Network.Socket (close)
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
data ConfAssertion b = ∀ a . Eq a ⇒ ConfAssertion [String] (Lens' b a) a

trueLens ∷ Lens' b ()
trueLens = lens (const ()) const

trueAssertion ∷ [String] → ConfAssertion b
trueAssertion args = ConfAssertion args trueLens ()

-- -------------------------------------------------------------------------- --
-- Test execution

-- Check the given list of assertions for the given configuration value
--
check
    ∷ a
    → [ConfAssertion a]
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
    ∷ (FromJSON (a → a), ToJSON a)
    ⇒ PkgInfo
    → ProgramInfoValidate a []
    → T.Text
        -- ^ label for the test case
    → Bool
        -- ^ expected outcome
    → [ConfAssertion a]
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
        debug ∘ T.putStrLn $ "DEBUG: caught exception: " ⊕ sshow e

-- -------------------------------------------------------------------------- --
-- Test Config Files
--

withConfigFile
    ∷ ToJSON b
    ⇒ ConfigFileFormat
    → b
    → (T.Text → IO a)
    → IO a
withConfigFile format config inner =
    withTempFile "." ("tmp_TestExample." ⊕ suffix format) $ \tmpPath tmpHandle → do
        B8.hPutStrLn tmpHandle ∘ formatter format $ config
        hClose tmpHandle
        inner $ T.pack tmpPath
  where
    suffix Json = "json"
    suffix _ = "yaml"
    formatter Json = LB.toStrict ∘ encode
    formatter _ = Yaml.encode

withConfigFileText
    ∷ T.Text
    → (T.Text → IO a)
    → IO a
withConfigFileText configText inner =
    withTempFile "." "tmp_TestExample.txt" $ \tmpPath tmpHandle → do
        T.hPutStrLn tmpHandle configText
        hClose tmpHandle
        inner $ T.pack tmpPath


#ifdef REMOTE_CONFIGS
data ConfigType = ∀ a . ToJSON a ⇒ ConfigType a

instance ToJSON ConfigType where
    toJSON (ConfigType a) = toJSON a

withConfigFileServer
    ∷ [(T.Text, ConfigType)]
    → [(T.Text, T.Text)]
    → Maybe ConfigFileFormat
    → (Int → Int → IO a)
    → IO a
withConfigFileServer configs configTexts maybeFormat inner =
    WARP.testWithApplication (return app) $ \httpPort →
        bracket WARP.openFreePort (close ∘ snd) $ \(httpsPort, sock) → do
            s ← forkIO $ WARP.runTLSSocket tlsSettings (warpSettings httpsPort) sock app
            inner httpPort httpsPort `finally` killThread s
  where
    app req respond = do

        let format = fromMaybe Other $ maybeFormat
                <|> (contentType <$> L.lookup HTTP.hAccept % WAI.requestHeaders req)

            maybeBody = LB.fromStrict <$> do
                p ← listToMaybe $ WAI.pathInfo req
                do
                    formatter format <$> lookup p configs
                    <|>
                    (T.encodeUtf8 <$> lookup p configTexts)

        respond $ case maybeBody of
            Just body → WAI.responseLBS HTTP.status200 [] body
            Nothing → WAI.responseLBS HTTP.status404 [contentTypeHeader format] "resource not found"

    formatter Json = LB.toStrict ∘ encode
    formatter _ = Yaml.encode

    contentTypeHeader Json = (HTTP.hContentType, head jsonMimeType)
    contentTypeHeader _ = (HTTP.hContentType, head yamlMimeType)

serverUrl ∷ Int → T.Text
serverUrl serverPort = "http://127.0.0.1:" ⊕ sshow serverPort

serverTlsUrl ∷ Int → T.Text
serverTlsUrl serverTlsPort = "https://127.0.0.1:" ⊕ sshow serverTlsPort

tlsSettings ∷ WARP.TLSSettings
tlsSettings = WARP.tlsSettingsMemory serverCert serverKey

warpSettings ∷ Int → WARP.Settings
warpSettings port = WARP.setPort port WARP.defaultSettings

serverCert ∷ B8.ByteString
serverCert = B8.unlines
    [ "-----BEGIN CERTIFICATE-----"
    , "MIIF5TCCA82gAwIBAgIJAPT8MspOLMHrMA0GCSqGSIb3DQEBCwUAMFQxCzAJBgNV"
    , "BAYTAlVTMRMwEQYDVQQIEwpTb21lLVN0YXRlMRwwGgYDVQQKExNjb25maWd1cmF0"
    , "aW9uLXRvb2xzMRIwEAYDVQQDEwlsb2NhbGhvc3QwIBcNMTUwMjAzMjMzNjIyWhgP"
    , "MjExNTAxMTAyMzM2MjJaMFQxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpTb21lLVN0"
    , "YXRlMRwwGgYDVQQKExNjb25maWd1cmF0aW9uLXRvb2xzMRIwEAYDVQQDEwlsb2Nh"
    , "bGhvc3QwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQCtXCeoaw1m+JaX"
    , "qJnxKdOelNJxuZxPFoNN2tNIxY+63H6yH9XkhDw1bTPsTv2YX0ZdNxGHprh2a5jP"
    , "Z5gUh2EUsPnSNhnVhAGef3Y2gfAxeT1k81Ap4IKhBq9Drmlg7uJOPkqBkUhMi675"
    , "pVtxb2oSOrH4wkJD9n47dGSl2ziuUejfhzc0oZcVEs/w90KFkfNvTYXzSJfjU+WJ"
    , "KX2h0VI3m33lBbreyGktoccImF6+gHNKC0m+L74MyfLzu3TDlg7a+YwEOhCu2Tbf"
    , "kdPcSvSW0xEo9yn4epcGL+bRLXsT8DYQSE0q7sJf0I6y+nespoPCpfWnWinKbs7n"
    , "xNDl7wfD4spYcV19lgAicv7l1W+ItB3A/8KOSD+a7bc5LY7svKwBPV5ZQ4jRkvoa"
    , "Efcztv7i89/CjurCm3TX2oespTqCUOKYlc61NQQ9l//2yoBPY2IvBKUOPpDHSyn+"
    , "ZuMKdNu0mzdTHDjsbNW/es2n8Uk1wG+bQ60ZWCkixJZ/SCECBJpt1qkkgh9iclh+"
    , "abTBXBSU+N8DmyO9UACV9LlsBkKxjfK4F+mlodgP5C9R6nPlpYB7W9cuLvIWQnuR"
    , "DSnW6fS1T0g77mXMFeB8bjnADq7dM3aQXisvkX/XGT1KfsJdTqoau3dFMYArFpx9"
    , "0LqEh6pmt2rkNfFNPHnSx/hzVKr0ZwIDAQABo4G3MIG0MB0GA1UdDgQWBBRPlQCM"
    , "ToNUODspmRtqhXHcidXpRDCBhAYDVR0jBH0we4AUT5UAjE6DVDg7KZkbaoVx3InV"
    , "6UShWKRWMFQxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpTb21lLVN0YXRlMRwwGgYD"
    , "VQQKExNjb25maWd1cmF0aW9uLXRvb2xzMRIwEAYDVQQDEwlsb2NhbGhvc3SCCQD0"
    , "/DLKTizB6zAMBgNVHRMEBTADAQH/MA0GCSqGSIb3DQEBCwUAA4ICAQAJfpa2Ju9j"
    , "Om7M5U9cQUZaYKqKe2N6+fx4y7C6J9oHAVjXzhevPm307I7MyVWpzh8+AShEnzyY"
    , "R83M8hL61cMOCjU3YOl6exz1jUYHXKhj0chl18z6wDM9o8NkoG4iFDbEipAtciKm"
    , "UbU+vm2d9z6mC+VN6xPVa9S72/+dRvyIgkPl3hQOZ5wKYic/7/EXM0MldQ7gb6KO"
    , "UuYDlr5aEHvF1J8Fnju5RzVofCPbC2obiwJN3RnYYFJv5pybQdnYfBg4z8OzgiQf"
    , "V/OVndzqyHWLg21MxPExM/PHFyuzeAh6e3lu6F1XhwoA4H8UzN1Gei2B8HfNQ54O"
    , "xyzeBu+kdPPzC0xQFm/s80CG+OPhtTd2ka6N2/YwgMSL6QOTQ1J7zpIwfsIgCaNH"
    , "Fkjj0gJk82+URMwjMhyh6m49qwhanL/9yodmascr27o10ZmAq6570C+zqaUoVk8y"
    , "m/PMNTHMLsSddjkelAKSjVU9+PQDRgnZTPiNhpEswgAbF8UjNkyxBRjBUrBIEGWd"
    , "k+PNgjOH3HPT3nWXYyTNTjAJQ7D5RVwFiROMdHdZdFjaxjRQpRkMvR/rpEdsf89H"
    , "75OZcCJzjg03soMUQ+ySp8Ax2Z6PSC1Cbvu+P+aOB0lyNpMYFuL0LVq5iWb6GmcB"
    , "5wxh9JKKsOVBhPDpOQlEAyRqtdXGbOwHNg=="
    , "-----END CERTIFICATE-----"
    ]

serverKey ∷ B8.ByteString
serverKey = B8.unlines
    [ "-----BEGIN RSA PRIVATE KEY-----"
    , "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"
    , "-----END RSA PRIVATE KEY-----"
    ]
#endif
