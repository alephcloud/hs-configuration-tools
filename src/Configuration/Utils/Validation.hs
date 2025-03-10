{-# LANGUAGE CPP #-}
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
( ConfigValidation
, ConfigValidation'

-- * Networking
, validateHttpOrHttpsUrl
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
, validateConfigFile

-- * Boolean Values
, validateFalse
, validateTrue
, validateBool

-- * Numeric Values
, validateNonNegative
, validatePositive
, validateNonPositive
, validateNegative
, validateNonNull

-- * Orders
, validateLess
, validateLessEq
, validateGreater
, validateGreaterEq
, validateRange
) where

import Configuration.Utils.Internal

import Control.Monad.Error.Class
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer.Class

import qualified Data.Foldable as F
import Data.Monoid.Unicode
import qualified Data.Text as T

import Network.URI

import Prelude.Unicode

import System.Directory

-- | A validation function. The type in the 'MonadWriter' is excpected to
-- be a 'Foldable' structure for collecting warnings.
--
type ConfigValidation a f = ConfigValidation' a f ()
type ConfigValidation' a f r = ∀ m . (MonadIO m, Functor m, Applicative m, MonadError T.Text m, MonadWriter (f T.Text) m) ⇒ a → m r

-- -------------------------------------------------------------------------- --
-- Networking

-- | Validates that a value is an HTTP or HTTPS URL
--
validateHttpOrHttpsUrl
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateHttpOrHttpsUrl configName uri =
    case parseURI uri of
        Nothing → throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"
        Just u → unless (uriScheme u ≡ "http:" || uriScheme u ≡ "https:") ∘ throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not an HTTP or HTTPS URL"

-- | Validates that a value is an HTTP URL
--
validateHttpUrl
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateHttpUrl configName uri =
    case parseURI uri of
        Nothing → throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"
        Just u → unless (uriScheme u ≡ "http:") ∘ throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not an HTTP URL"

-- | Validates that a value is an HTTPS URL
--
validateHttpsUrl
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateHttpsUrl configName uri =
    case parseURI uri of
        Nothing → throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"
        Just u → unless (uriScheme u ≡ "https:") ∘ throwError $
            "the value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not an HTTPS URL"

-- | Validates that a value is an URI without a fragment identifier
--
validateUri
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateUri configName uri =
    unless (isURIReference uri) ∘ throwError $
        "The value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"

-- | Validates that a value is an absolute URI without a fragment identifier
--
validateAbsoluteUri
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateAbsoluteUri configName uri =
    unless (isAbsoluteURI uri) ∘ throwError $
        "The value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"

-- | Validates that a value is an absolute URI with an optional fragment
-- identifier
--
validateAbsoluteUriFragment
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateAbsoluteUriFragment configName uri =
    unless (isURI uri) ∘ throwError $
        "The value " ⊕ T.pack uri ⊕ " for " ⊕ configName ⊕ " is not a valid URI"

validateIPv4
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateIPv4 configName ipv4 =
    unless (isIPv4address ipv4) ∘ throwError $
        "The value " ⊕ T.pack ipv4 ⊕ " for " ⊕ configName ⊕ " is not a valid IPv4 address"

validateIPv6
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → String
    → m ()
validateIPv6 configName ipv6 =
    unless (isIPv6address ipv6) ∘ throwError $
        "The value " ⊕ T.pack ipv6 ⊕ " for " ⊕ configName ⊕ " is not a valid IPv6 address"

validatePort
    ∷ (MonadError T.Text m, Integral n, Show n)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → n
    → m ()
validatePort configName p =
    unless (p > 1 && p < 65535) ∘ throwError $
        "port value " ⊕ T.pack (show p) ⊕ " for " ⊕ configName ⊕ " is not valid port number"

-- -------------------------------------------------------------------------- --
-- Monoids, Foldables, and Co

validateNonEmpty
    ∷ (MonadError T.Text m, Eq a, Monoid a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
    → m ()
validateNonEmpty configName x =
    when (x ≡ mempty) ∘ throwError $
        "value for " ⊕ configName ⊕ " must not be empty"

validateLength
    ∷ (MonadError T.Text m, F.Foldable f)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ exact length of the validated value
    → f a
    → m ()
validateLength configName len x =
    unless (length (F.toList x) ≡ len) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be of length exactly " ⊕ sshow len

validateMaxLength
    ∷ (MonadError T.Text m, F.Foldable f)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ maximum length of the validated value
    → f a
    → m ()
validateMaxLength configName u x =
    unless (length (F.toList x) ≤ u) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be of length at most " ⊕ sshow u

validateMinLength
    ∷ (MonadError T.Text m, F.Foldable f)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ minimum length of the validated value
    → f a
    → m ()
validateMinLength configName l x =
    unless (length (F.toList x) ≥ l) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be of length at least " ⊕ sshow l

validateMinMaxLength
    ∷ (MonadError T.Text m, F.Foldable f)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → Int
        -- ^ minimum length of the validated value
    → Int
        -- ^ maximum length of the validated value
    → f a
    → m ()
validateMinMaxLength configName l u x =
    unless (len ≥ l && len ≤ u) ∘ throwError $
        "the length of the value for " ⊕ configName ⊕
        " must be at least " ⊕ sshow l ⊕ " and at most " ⊕ sshow u
  where
    len = length $ F.toList x

-- -------------------------------------------------------------------------- --
-- Files

validateFilePath
    ∷ MonadError T.Text m
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateFilePath configName file =
    when (null file) ∘ throwError $
        "file path for " ⊕ configName ⊕ " must not be empty"

validateFile
    ∷ (MonadError T.Text m, MonadIO m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateFile configName file = do
    exists ← liftIO $ doesFileExist file
    unless exists ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " does not exist"

validateFileReadable
    ∷ (MonadError T.Text m, MonadIO m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateFileReadable configName file = do
    validateFile configName file
    liftIO (getPermissions file) >>= \x → unless (readable x) ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " is not readable"

validateFileWritable
    ∷ (MonadError T.Text m, MonadIO m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateFileWritable configName file = do
    validateFile configName file
    liftIO (getPermissions file) >>= \x → unless (writable x) ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " is not writable"

validateFileExecutable
    ∷ (MonadError T.Text m, MonadIO m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateFileExecutable configName file = do
    validateFile configName file
    liftIO (getPermissions file) >>= \x → unless (executable x) ∘ throwError $
        "the file " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " is not excutable"

validateDirectory
    ∷ (MonadError T.Text m, MonadIO m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateDirectory configName dir = do
    exists ← liftIO $ doesDirectoryExist dir
    unless exists ∘ throwError $
        "the directory " ⊕ T.pack dir ⊕ " for " ⊕ configName ⊕ " does not exist"

-- | Validates if the given executable name can be found in the system
-- and can be executed.
--
validateExecutable
    ∷ (Functor m, MonadError T.Text m, MonadIO m)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → FilePath
    → m ()
validateExecutable configName file = do
    execFile ← (file <$ validateFile configName file) `catchError` \_ ->
        liftIO (findExecutable file) >>= \case
            Nothing → throwError $
                "the executable " ⊕ T.pack file ⊕ " for " ⊕ configName ⊕ " could not be found in the system;"
                ⊕ " you may check your SearchPath and PATH variable settings"
            Just f → return f
    validateFileExecutable configName execFile

-- | Validate that the input is a config file
--
validateConfigFile
    ∷ (MonadIO m, MonadError T.Text m)
    ⇒ String
    → m ()
validateConfigFile filepath =
    validateFileReadable "config-file" filepath
#ifdef REMOTE_CONFIGS
    `catchError` \_ ->
    validateHttpOrHttpsUrl "config-file" filepath
#endif

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

-- -------------------------------------------------------------------------- --
-- Numeric Values

validateNonNegative
    ∷ (MonadError T.Text m, Ord a, Num a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
    → m ()
validateNonNegative configName x =
    when (x < 0) ∘ throwError $
        "value for " ⊕ configName ⊕ " must not be negative"

validatePositive
    ∷ (MonadError T.Text m, Ord a, Num a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
    → m ()
validatePositive configName x =
    when (x ≤ 0) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be positive"

validateNonPositive
    ∷ (MonadError T.Text m, Ord a, Num a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
    → m ()
validateNonPositive configName x =
    when (x > 0) ∘ throwError $
        "value for " ⊕ configName ⊕ " must not be positive"

validateNegative
    ∷ (MonadError T.Text m, Ord a, Num a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
    → m ()
validateNegative configName x =
    when (x ≥ 0) ∘ throwError $
        "value for " ⊕ configName ⊕ " must be negative"

validateNonNull
    ∷ (MonadError T.Text m, Eq a, Num a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
    → m ()
validateNonNull configName x = when (x ≡ 0) ∘ throwError $
    "value for " ⊕ configName ⊕ " must not be zero"

-- -------------------------------------------------------------------------- --
-- Orders

validateLess
    ∷ (MonadError T.Text m, Ord a, Show a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
        -- ^ a strict upper bound for the configuration value
    → a
    → m ()
validateLess configName upper x = unless (x < upper) ∘ throwError $
    "value for " ⊕ configName ⊕ " must be strictly less than " ⊕ sshow upper ⊕ ", but was " ⊕ sshow x

validateLessEq
    ∷ (MonadError T.Text m, Ord a, Show a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
        -- ^ a upper bound for the configuration value
    → a
    → m ()
validateLessEq configName upper x = unless (x ≤ upper) ∘ throwError $
    "value for " ⊕ configName ⊕ " must be less or equal than " ⊕ sshow upper ⊕ ", but was " ⊕ sshow x

validateGreater
    ∷ (MonadError T.Text m, Ord a, Show a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
        -- ^ a strict lower bound for the configuration value
    → a
    → m ()
validateGreater configName lower x = unless (x > lower) ∘ throwError $
    "value for " ⊕ configName ⊕ " must be strictly greater than " ⊕ sshow lower ⊕ ", but was " ⊕ sshow x

validateGreaterEq
    ∷ (MonadError T.Text m, Ord a, Show a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → a
        -- ^ a lower bound for the configuration value
    → a
    → m ()
validateGreaterEq configName lower x = unless (x ≥ lower) ∘ throwError $
    "value for " ⊕ configName ⊕ " must be greater or equal than " ⊕ sshow lower ⊕ ", but was " ⊕ sshow x

validateRange
    ∷ (MonadError T.Text m, Ord a, Show a)
    ⇒ T.Text
        -- ^ configuration property name that is used in the error message
    → (a, a)
        -- ^ the valid range for the configuration value
    → a
    → m ()
validateRange configName (lower,upper) x = unless (x ≥ lower ∧ x ≤ upper) ∘ throwError $
    "value for " ⊕ configName ⊕ " must be within the range of (" ⊕ sshow lower ⊕ ", " ⊕ sshow upper ⊕ "), but was " ⊕ sshow x
