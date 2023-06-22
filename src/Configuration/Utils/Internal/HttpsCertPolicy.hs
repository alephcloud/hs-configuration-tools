{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.Internal.HttpsCertPolicy
-- Description: HTTPS certificate validation policy
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides means for defining and using HTTPS
-- certificate validation polices for HTTPS requests.
--
module Configuration.Utils.Internal.HttpsCertPolicy
(
-- * HTTPS Certificate Validation Policy
  HttpsCertPolicy(..)
, certPolicyInsecure
, certPolicyHostFingerprints
, defaultHttpsCertPolicy
, pHttpsCertPolicy

-- * HTTP Requests With Certificate Validation Policy
, simpleHttpWithValidationPolicy
, httpWithValidationPolicy
, VerboseTlsException(..)
) where

import Configuration.Utils.CommandLine
import Configuration.Utils.Internal
import Configuration.Utils.Monoid
import Configuration.Utils.Operators
import Configuration.Utils.Validation

import Control.Exception (Exception, Handler(..), catches, throwIO)
import Control.Monad.State

import Data.Bifunctor
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import qualified Data.X509 as TLS
import qualified Data.X509.Validation as TLS

import qualified Options.Applicative as O

import Prelude hiding (any, concatMap, mapM_)
import Prelude.Unicode hiding ((×))

import qualified Network.Connection as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.TLS as TLS hiding (HashSHA256)
import qualified Network.TLS.Extra as TLS

import qualified System.X509 as TLS

import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- HTTPS Certificate Validation Policy

data HttpsCertPolicy = HttpsCertPolicy
    { _certPolicyInsecure ∷ !Bool
        -- ^ disable certificate validation
    , _certPolicyHostFingerprints ∷ !(HM.HashMap TLS.ServiceID TLS.Fingerprint)
        -- ^ a whitelist for services with trusted certificates
    }
    deriving (Show, Eq, Typeable)

certPolicyInsecure ∷ Lens' HttpsCertPolicy Bool
certPolicyInsecure = lens _certPolicyInsecure $ \s a → s { _certPolicyInsecure = a }

certPolicyHostFingerprints ∷ Lens' HttpsCertPolicy (HM.HashMap TLS.ServiceID TLS.Fingerprint)
certPolicyHostFingerprints = lens _certPolicyHostFingerprints $ \s a → s { _certPolicyHostFingerprints = a }

defaultHttpsCertPolicy ∷ HttpsCertPolicy
defaultHttpsCertPolicy = HttpsCertPolicy
    { _certPolicyInsecure = False
    , _certPolicyHostFingerprints = mempty
    }

pHttpsCertPolicy
    ∷ T.Text
        -- ^ prefix for the command line options
    → MParser HttpsCertPolicy
pHttpsCertPolicy prefix = id
    <$< certPolicyInsecure .:: boolOption_
        % O.long (T.unpack prefix ⊕ "https-insecure")
        ⊕ O.help "Bypass certificate validation for all HTTPS connections to all services. ONLY USE THIS WHEN YOU UNDERSTAND WHAT YOU DO."
    <*< certPolicyHostFingerprints %:: pLeftMonoidalUpdate % pRule
  where
    pRule = O.option (O.eitherReader readFingerprint)
        % O.long (T.unpack prefix ⊕ "https-allow-cert")
        ⊕ O.help "Unconditionally trust the certificate for connecting to the service. ONLY USE THIS WHEN YOU ARE SURE THAT THE CERTIFICATE CAN BE TRUSTED."
        ⊕ O.metavar "HOSTNAME:PORT:FINGERPRINT"
    readFingerprint = evalStateT $ do
        hostname ∷ String ← next
        x $ validateNonEmpty "hostname" hostname
        port ∷ Int ← lift ∘ readEither =<< next
        x $ validatePort "port" port
        fingerprint ← lift ∘ B64.decode ∘ B8.pack =<< next
        x $ validateNonEmpty "fingerprint" fingerprint -- FIXME we should evaluate the length
        return $ HM.singleton (hostname, sshow port) (TLS.Fingerprint fingerprint)

    next = state $ second (drop 1) ∘ break (≡ ':')

    x = lift ∘ first T.unpack

-- -------------------------------------------------------------------------- --
-- HTTP Requests With Certificate Validation Policy


-- | Make an HTTP request with a given certificate validation policy.
--
-- NOTE that the HTTP request is strictly loaded into memory.
--
-- NOTE that this implementation opens a new TCP connection for each single
-- request. HTTPS certificates validation results are not cached between different
-- requests.
--
simpleHttpWithValidationPolicy
    ∷ T.Text
        -- ^ HTTP or HTTPS URL
    → HttpsCertPolicy
    → IO (HTTP.Response LB.ByteString)
simpleHttpWithValidationPolicy url policy = do
    request ← HTTP.parseUrlThrow $ T.unpack url
    httpWithValidationPolicy request policy

httpWithValidationPolicy
    ∷ HTTP.Request
    → HttpsCertPolicy
    → IO (HTTP.Response LB.ByteString)
httpWithValidationPolicy request policy = do
    certVar ← newIORef Nothing
    settings ← getSettings policy certVar
    mgr ← HTTP.newManager settings
    HTTP.httpLbs request mgr `catches`
        [ Handler $ \(e ∷ TLS.TLSException) → do
            cert ← readIORef certVar
            handleTlsException request cert e
        ]

-- -------------------------------------------------------------------------- --
-- Verbose TLS exceptions

-- | The Haskell @tls@ library provides only limited means for providing
-- user friendly error messages. In particular we'd like to provide the
-- user with fingerprints of the reject certificate for self-signed
-- certificates. Also we want to provide the user with some guidance what
-- a particular failure may indicate with respect to security of the
-- connection.
--
-- Here we employ a /hack/ for better error handling: Based on the assumption
-- that we initialize a new connection 'Manager' and also a new certificate
-- cache for each request, we write the certificate that is received
-- from the server in the TLS handshake to an 'IORef'. If the handshakes
-- fails later on because the certificate is rejected we can recover the
-- rejected certificate from the 'IORef'.
--
-- What we really want are exceptions that can be consumed programatically.
-- In particular exceptions should include rejected certificates.
--
newtype VerboseTlsException = VerboseTlsException T.Text
    deriving (Eq, Ord, Typeable)

instance Show VerboseTlsException where
    show (VerboseTlsException msg) = "TLS exception: " ⊕ T.unpack msg

instance Exception VerboseTlsException

handleTlsException
    ∷ HTTP.Request
    → Maybe (TLS.SignedExact TLS.Certificate)
    → TLS.TLSException
    → IO a
handleTlsException request cert e@(TLS.HandshakeFailed (TLS.Error_Protocol (msg, _b, _alert)))
    | "certificate rejected: [SelfSigned]" `L.isPrefixOf` msg = throwIO ∘ VerboseTlsException
        $ "The server uses a self-signed certificate. If you are sure that no-one"
        ⊕ " is intercepting the connection and this is the correct certificate you"
        ⊕ " may enable usage of this certificate with the following command line option:"
        ⊕ "\n\n"
        ⊕ "   " ⊕ allowCertOption
        ⊕ "\n"

    | "certificate rejected: [CacheSaysNo" `L.isPrefixOf` msg = throwIO ∘ VerboseTlsException
        $ "There is a mismatch between the expected certificate provided for"
        ⊕ " this service and the certificate provided by the service. You may try to remove"
        ⊕ " the expected certificate fingerprint and check if the certificate that is"
        ⊕ " offered by the service validates cleanly. If that is not the case this could"
        ⊕ " mean that someone is intercepting the connections. In this case YOU SHOULD ONLY"
        ⊕ " PROCEED WHEN YOU ARE SURE THAT IT IS SAFE. If you still want to proceed you may"
        ⊕ " accept the new certificate by using following command line option:"
        ⊕ "\n\n"
        ⊕ "   " ⊕ allowCertOption
        ⊕ "\n\n"
        ⊕ " The error message was: " ⊕ T.pack msg
        ⊕ "\n"

    | "certificate rejected: [NameMismatch" `L.isPrefixOf` msg = throwIO ∘ VerboseTlsException
        $ "There is a mismatch between the certificate name and the server name. This"
        ⊕ " could mean that someone is intercepting the connection or that you are not"
        ⊕ " connected to the correct service. YOU SHOULD ONLY PROCEED WHEN YOU ARE SURE"
        ⊕ " THAT IT IS SAFE TO DO SO. If you still want to proceed you may"
        ⊕ " accept the certificate by using following command line option:"
        ⊕ "\n\n"
        ⊕ "   " ⊕ allowCertOption
        ⊕ "\n\n"
        ⊕ " The error message was: " ⊕ T.pack msg
        ⊕ "\n"

    | "certificate rejected:" `L.isPrefixOf` msg = throwIO ∘ VerboseTlsException
        $ "The certificate that was offered by the service was rejected. This"
        ⊕ " could mean that someone is intercepting the connection or that you are not"
        ⊕ " connected to the correct service. YOU SHOULD ONLY PROCEED WHEN YOU ARE SURE"
        ⊕ " THAT IT IS SAFE TO DO SO. If you still want to proceed you may"
        ⊕ " accept the certificate by using following command line option:"
        ⊕ "\n\n"
        ⊕ "   " ⊕ allowCertOption
        ⊕ "\n\n"
        ⊕ " The error message was: " ⊕ T.pack msg
        ⊕ "\n"
    | otherwise = throwIO e
  where
    printFingerprint (TLS.Fingerprint f) = fromString ∘ B8.unpack ∘ B64.encode $ f
    printCertF c = printFingerprint (TLS.getFingerprint c fingerprintAlg)
    fingerprintAlg = TLS.HashSHA256
    hostText = T.decodeUtf8 $ HTTP.host request
    portText = sshow $ HTTP.port request

    allowCertOption = case cert of
        Nothing → "--insecure-remote-config-files"
        (Just c) →
            "--remote-config-fingerprint=" ⊕ hostText ⊕ ":" ⊕ portText ⊕ ":" ⊕ printCertF c

handleTlsException _ _  e = throwIO e

-- -------------------------------------------------------------------------- --
-- TLS Settings

-- | The usage of the 'certVar' parameter is not thread-safe!
--
-- FIXME We could make this thread-safe by using a cache for
-- "unvalidated" certificates.
--
getSettings
    ∷ HttpsCertPolicy
    → IORef (Maybe (TLS.SignedExact TLS.Certificate))
    → IO HTTP.ManagerSettings
getSettings policy certVar = do
    certstore ← TLS.getSystemCertificateStore
    return $ HTTP.mkManagerSettings
        (HTTP.TLSSettings (tlsSettings certstore))
        Nothing
  where
    -- It is safe to pass empty strings for host and port since 'TLS.connectFromHandle'
    -- and 'TLS.connectTo' are going to overwrite this anyways.
    --
    tlsSettings certstore = (TLS.defaultParamsClient "" "")
        { TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_default }
        , TLS.clientShared = def
            { TLS.sharedCAStore = certstore
            , TLS.sharedValidationCache = validationCache
            }
        , TLS.clientHooks = def
            { TLS.onServerCertificate = \store cache serviceId certChain@(TLS.CertificateChain certs) → do
                modifyIORef' certVar (const $ listToMaybe certs)
                TLS.onServerCertificate def store cache serviceId certChain
            }
        }

    validationCache
        | _certPolicyInsecure policy = TLS.ValidationCache
            (\_ _ _ → return TLS.ValidationCachePass)
            (\_ _ _ → return ())
        | otherwise = certCache (_certPolicyHostFingerprints policy)

    -- 'TLS.exceptionValidationCache' would have worked to here, but it's hard to get
    -- the certificate fingerprint of the failing certificate from the exceptions it
    -- generates. Unfortunately, the TLS package allows us to pass only a string message,
    -- so that we have to encode and to decode the fingerprint.
    --
    certCache ∷ HM.HashMap TLS.ServiceID TLS.Fingerprint → TLS.ValidationCache
    certCache fingerprints = TLS.ValidationCache
        (queryCallback fingerprints)
        (\_ _ _ → return ())

    queryCallback ∷ HM.HashMap TLS.ServiceID TLS.Fingerprint → TLS.ValidationCacheQueryCallback
    queryCallback cache serviceID fingerprint _ = return $
        case HM.lookup serviceID cache of
            Nothing → TLS.ValidationCacheUnknown
            Just f
                | fingerprint ≡ f → TLS.ValidationCachePass
                | otherwise → TLS.ValidationCacheDenied
                    $ "for host: " ⊕ fst serviceID ⊕ ":" ⊕ B8.unpack (snd serviceID)
                    ⊕ " expected fingerprint: " ⊕ printFingerprint f
                    ⊕ " but got fingerprint: " ⊕ printFingerprint fingerprint
      where
        printFingerprint (TLS.Fingerprint f) = fromString ∘ B8.unpack ∘ B64.encode $ f
