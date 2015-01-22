{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.Validation
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Utilities for validating configuration values
--
module Configuration.Utils.Validation
(
-- * Networking
  validateHttpOrHttpsUrl
, validateHttpUrl
, validateHttpsUrl
, validateUri
, validateAbsoluteUri
, validateAbsoluteUriFragment
, validateIPv4
, validateIPv6
, validatePort

-- * Monoids, Foldables and Co
, validateNonEmpty
, validateLength
, validateMinLength
, validateMaxLength
, validateMinMaxLength

-- * Files
, validateFilePath
, validateFile
, validateFileReadable
, validateFileWritable
, validateExecutable
, validateDirectory

-- * Boolean Values
, validateFalse
, validateTrue
, validateBool
) where

import Configuration.Utils

import Control.Monad.Error.Class
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Foldable as F
import Data.Monoid
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T

import Network.URI

import Prelude.Unicode

import System.Directory

-- -------------------------------------------------------------------------- --
-- Utils

sshow
    ∷ (Show α, IsString τ)
    ⇒ α
    → τ
sshow = fromString ∘ show

-- -------------------------------------------------------------------------- --
-- Networking

-- | Validates that a value is an HTTP or HTTPS URL
--
validateHttpOrHttpsUrl
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateHttpOrHttpsUrl configName uri =
    case parseURI uri of
        Nothing → throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"
        Just u → unless (uriScheme u ≡ "http:" || uriScheme u ≡ "https:") ∘ throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not an HTTP or HTTPS URL"

-- | Validates that a value is an HTTP URL
--
validateHttpUrl
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateHttpUrl configName uri =
    case parseURI uri of
        Nothing → throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"
        Just u → unless (uriScheme u ≡ "http:") ∘ throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not an HTTP URL"

-- | Validates that a value is an HTTPS URL
--
validateHttpsUrl
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateHttpsUrl configName uri =
    case parseURI uri of
        Nothing → throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"
        Just u → unless (uriScheme u ≡ "https:") ∘ throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not an HTTPS URL"

-- | Validates that a value is an URI without a fragment identifier
--
validateUri
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateUri configName uri =
    unless (isURIReference uri) ∘ throwError $
        "The value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"

-- | Validates that a value is an absolute URI without a fragment identifier
--
validateAbsoluteUri
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateAbsoluteUri configName uri =
    unless (isAbsoluteURI uri) ∘ throwError $
        "The value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"

-- | Validates that a value is an absolute URI with an optional fragment
-- identifier
--
validateAbsoluteUriFragment
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateAbsoluteUriFragment configName uri =
    unless (isURI uri) ∘ throwError $
        "The value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"

validateIPv4
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateIPv4 configName ipv4 =
    unless (isIPv4address ipv4) ∘ throwError $
        "The value " ⊕ T.pack ipv4 ⊕ " for " ⊕ configName ⊕ " is not a valid IPv4 address"

validateIPv6
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation String λ
validateIPv6 configName ipv6 =
    unless (isIPv6address ipv6) ∘ throwError $
        "The value " ⊕ T.pack ipv6 ⊕ " for " ⊕ configName ⊕ " is not a valid IPv6 address"

validatePort
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation Int λ
validatePort configName p =
    unless (p > 1 && p < 65535) ∘ throwError $
        "port value " ⊕ T.pack (show p) ⊕ " for " ⊕ configName ⊕ " is not valid port number"

-- -------------------------------------------------------------------------- --
-- Monoids, Foldables, and Co

validateNonEmpty
    ∷ (Eq α, Monoid α)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation α λ
validateNonEmpty configName x =
    when (x ≡ mempty) ∘ throwError $
        "value for " ⊕ configName ⊕ " must not be empty"

validateLength
    ∷ (F.Foldable φ)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ exact length of the validated value
    → ConfigValidation (φ α) λ
validateLength configName len x =
    unless (length (F.toList x) ≡ len) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be of length exactly " ⊕ sshow len

validateMaxLength
    ∷ (F.Foldable φ)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ maximum length of the validated value
    → ConfigValidation (φ α) λ
validateMaxLength configName u x =
    unless (length (F.toList x) ≤ u) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be of length at most " ⊕ sshow u

validateMinLength
    ∷ (F.Foldable φ)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ minimum length of the validated value
    → ConfigValidation (φ α) λ
validateMinLength configName l x =
    unless (length (F.toList x) ≥ l) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be of length at least " ⊕ sshow l

validateMinMaxLength
    ∷ (F.Foldable φ)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ minimum length of the validated value
    → Int
        -- ^ maximum length of the validated value
    → ConfigValidation (φ α) λ
validateMinMaxLength configName l u x =
    unless (len ≥ l && len ≤ u) ∘ throwError $
        "the length of the value for " ⊕ configName ⊕
        " must be at least " ⊕ sshow l ⊕ " and at most " ⊕ sshow u
  where
    len = length $ F.toList x

-- -------------------------------------------------------------------------- --
-- Files

validateFilePath
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateFilePath configName file =
    when (null file) ∘ throwError $
        "file path for " ⊕ configName ⊕ " must not be empty"

validateFile
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateFile configName file = do
    exists ← liftIO $ doesFileExist file
    unless exists ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " does not exist"

validateFileReadable
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateFileReadable configName file = do
    validateFile configName file
    liftIO (getPermissions file) >>= \x → unless (readable x) ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " is not readable"

validateFileWritable
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateFileWritable configName file = do
    validateFile configName file
    liftIO (getPermissions file) >>= \x → unless (writable x) ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " is not writable"

validateFileExecutable
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateFileExecutable configName file = do
    validateFile configName file
    liftIO (getPermissions file) >>= \x → unless (executable x) ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " is not excutable"

validateDirectory
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateDirectory configName dir = do
    exists ← liftIO $ doesDirectoryExist dir
    unless exists ∘ throwError $
        "the directory " ⊕ T.pack dir ⊕ " for " ⊕ configName ⊕ " does not exist"

-- | Validates if the given executable name can be found in the system
-- and can be executed.
--
validateExecutable
    ∷ T.Text
        -- ^ configuration property name that is used in the error message
    → ConfigValidation FilePath λ
validateExecutable configName file = do
    execFile ← (file <$ validateFile configName file) `catchError` \_ ->
        liftIO (findExecutable file) >>= \case
            Nothing → throwError $
                "the executable " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " could not be found in the system;"
                ⊕ " you may check your SearchPath and PATH variable settings"
            Just f → return f
    validateFileExecutable configName execFile

-- -------------------------------------------------------------------------- --
-- Boolean Values

validateFalse
    ∷ (MonadError T.Text m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Bool
    → m ()
validateFalse configName = validateBool configName False

validateTrue
    ∷ (MonadError T.Text m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Bool
    → m ()
validateTrue configName = validateBool configName True

validateBool
    ∷ (MonadError T.Text m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Bool
        -- ^ expected value
    → Bool
    → m ()
validateBool configName expected x = unless (x ≡ expected) ∘ throwError $
    "expected " ⊕ configName ⊕ " to be " ⊕ sshow expected ⊕ ", but was " ⊕ sshow x


