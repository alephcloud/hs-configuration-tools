{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
) where

import Configuration.Utils.CommandLine
import Configuration.Utils.Internal

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

import Prelude hiding (concatMap, mapM_, any)

#ifdef REMOTE_CONFIGS
import Configuration.Utils.Internal.HttpsCertPolicy
import Configuration.Utils.Operators
#endif

-- | A JSON 'Value' parser for a property of a given
-- 'Object' that updates a setter with the parsed value.
--
-- > data Auth = Auth
-- >     { _userId ∷ !Int
-- >     , _pwd ∷ !String
-- >     }
-- >
-- > userId ∷ Functor f ⇒ (Int → f Int) → Auth → f Auth
-- > userId f s = (\u → s { _userId = u }) <$> f (_userId s)
-- >
-- > pwd ∷ Functor f ⇒ (String → f String) → Auth → f Auth
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
    ∷ Lens' a b -- ^ a lens into the target that is updated by the parser
    → T.Text -- ^ the JSON property name
    → (Value → Parser b) -- ^ the JSON 'Value' parser that is used to parse the value of the property
    → Object -- ^ the parsed JSON 'Value' 'Object'
    → Parser (a → a)
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
-- > user ∷ Functor f ⇒ (String → f String) → Auth → f Auth
-- > user f s = (\u → s { _user = u }) <$> f (_user s)
-- >
-- > pwd ∷ Functor f ⇒ (String → f String) → Auth → f Auth
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
(..:) ∷ FromJSON b ⇒ Lens' a b → T.Text → Object → Parser (a → a)
(..:) s k = setProperty s k parseJSON
infix 6 ..:
{-# INLINE (..:) #-}

-- | A JSON parser for a function that modifies a property
-- of a given 'Object' and updates a setter with the parsed
-- function.
--
-- This function is useful when a 'FromJSON' instance isn't available.
-- When a 'FromJSON' instance exists, the '%.:' provides a more
-- ideomatic alternative.
--
-- > data HttpURL = HttpURL
-- >     { _auth ∷ !Auth
-- >     , _domain ∷ !String
-- >     }
-- >
-- > auth ∷ Functor f ⇒ (Auth → f Auth) → HttpURL → f HttpURL
-- > auth f s = (\u → s { _auth = u }) <$> f (_auth s)
-- >
-- > domain ∷ Functor f ⇒ (String → f String) → HttpURL → f HttpURL
-- > domain f s = (\u → s { _domain = u }) <$> f (_domain s)
-- >
-- > path ∷ Functor f ⇒ (String → f String) → HttpURL → f HttpURL
-- > path f s = (\u → s { _path = u }) <$> f (_path s)
-- >
-- > -- or with lenses and TemplateHaskell just:
-- > -- $(makeLenses ''HttpURL)
-- >
-- > instance FromJSON (HttpURL → HttpURL) where
-- >     parseJSON = withObject "HttpURL" $ \o → id
-- >         <$< updateProperty auth "auth" parseJSON o
-- >         <*< setProperty domain "domain" parseJSON o
--
updateProperty
    ∷ Lens' a b
    → T.Text
    → (Value → Parser (b → b))
    → Object
    → Parser (a → a)
updateProperty s k p o = case H.lookup k o of
    Nothing → pure id
    Just v → over s <$> p v
{-# INLINE updateProperty #-}

-- | A variant of 'updateProperty' that uses the 'FromJSON' instance
-- for the update function. It mimics the aeson operator '.:'.
-- It creates a parser that modifies a setter with a parsed function.
--
-- > data HttpURL = HttpURL
-- >     { _auth ∷ !Auth
-- >     , _domain ∷ !String
-- >     }
-- >
-- > auth ∷ Functor f ⇒ (Auth → f Auth) → HttpURL → f HttpURL
-- > auth f s = (\u → s { _auth = u }) <$> f (_auth s)
-- >
-- > domain ∷ Functor f ⇒ (String → f String) → HttpURL → f HttpURL
-- > domain f s = (\u → s { _domain = u }) <$> f (_domain s)
-- >
-- > path ∷ Functor f ⇒ (String → f String) → HttpURL → f HttpURL
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
(%.:) ∷ FromJSON (b → b) ⇒ Lens' a b → T.Text → Object → Parser (a → a)
(%.:) s k = updateProperty s k parseJSON
infix 6 %.:
{-# INLINE (%.:) #-}

-- | This operator requires that a value is explicitly provided in a
-- configuration file, thus preventing the default value from being used.
-- Otherwise this operator does the same as '(..:)'.
--
(!..:)
    ∷ FromJSON b
    ⇒ Lens' a b
    → T.Text
    → Object
    → Parser (a → a)
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

pConfigFilesConfig ∷ Maybe String → MParser ConfigFilesConfig
pConfigFilesConfig optionGroup = id
    <$< cfcHttpsPolicy %:: pHttpsCertPolicy optionGroup "config-"

#else

data ConfigFilesConfig = ConfigFilesConfig {}

defaultConfigFilesConfig ∷ ConfigFilesConfig
defaultConfigFilesConfig = ConfigFilesConfig {}

pConfigFilesConfig ∷ Maybe String → MParser ConfigFilesConfig
pConfigFilesConfig _ = pure id
#endif

-- -------------------------------------------------------------------------- --
-- Miscellaneous Utilities

dropAndUncaml ∷ Int → String → String
dropAndUncaml i l
    | length l < i + 1 = l
    | otherwise = let (h:t) = drop i l
        in toLower h : concatMap (\x → if isUpper x then "-" ⊕ [toLower x] else [x]) t

