{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.CommandLine
-- Description: Command Line Option Parsing with Default Values
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides tools for defining command line parsers for
-- configuration types.
--
-- Unlike /normal/ command line parsers the parsers for configuration
-- types are expected to yield an update function that takes
-- a value and updates the value with the settings from the command line.
--
-- Assuming that
--
-- * all configuration types are nested Haskell records or
--   simple types and
--
-- * that there are lenses for all record fields
--
-- usually the operators '.::' and '%::' are all that is needed from this module.
--
-- The module "Configuration.Utils.Monoid" provides tools for the case that
-- a /simple type/ is a container with a monoid instance, such as @List@ or
-- @HashMap@.
--
-- The module "Configuration.Utils.Maybe" explains the usage of optional
-- 'Maybe' values in configuration types.
--
module Configuration.Utils.CommandLine
( MParser
, (.::)
, (%::)

-- * Misc Utils
, boolReader
, boolOption
, boolOption_
, enableDisableFlag
, fileOption
, eitherReadP
, jsonOption
, jsonReader
, module Options.Applicative
) where

import Configuration.Utils.Internal
import Configuration.Utils.Operators

import Control.Applicative
import Control.Monad.Writer hiding (mapM_)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.CaseInsensitive as CI
import Data.Maybe
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T

import Options.Applicative hiding (Parser, Success)
import qualified Options.Applicative.Types as O

import qualified Options.Applicative as O
import qualified Options.Applicative.Builder.Internal as O

import Prelude hiding (any, concatMap, mapM_)

import qualified Text.ParserCombinators.ReadP as P hiding (string)

#if MIN_VERSION_base(4,13,0)
import Prelude.Unicode hiding ((×))
#else
import Prelude.Unicode
#endif


-- -------------------------------------------------------------------------- --
-- Applicative Option Parsing with Default Values

-- | Type of option parsers that yield a modification function.
--
type MParser a = O.Parser (a → a)

-- | An operator for applying a setter to an option parser that yields a value.
--
-- Example usage:
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
-- > pAuth ∷ MParser Auth
-- > pAuth = id
-- >    <$< user .:: strOption
-- >        % long "user"
-- >        ⊕ short 'u'
-- >        ⊕ help "user name"
-- >    <*< pwd .:: strOption
-- >        % long "pwd"
-- >        ⊕ help "password for user"
--
(.::) ∷ (Alternative f, Applicative f) ⇒ Lens' a b → f b → f (a → a)
(.::) a opt = set a <$> opt <|> pure id
infixr 5 .::
{-# INLINE (.::) #-}

-- | An operator for applying a setter to an option parser that yields
-- a modification function.
--
-- Example usage:
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
-- > pHttpURL ∷ MParser HttpURL
-- > pHttpURL = id
-- >     <$< auth %:: pAuth
-- >     <*< domain .:: strOption
-- >         % long "domain"
-- >         ⊕ short 'd'
-- >         ⊕ help "HTTP domain"
--
(%::) ∷ (Alternative f, Applicative f) ⇒ Lens' a b → f (b → b) → f (a → a)
(%::) a opt = over a <$> opt <|> pure id
infixr 5 %::
{-# INLINE (%::) #-}

-- -------------------------------------------------------------------------- --
-- Misc Utilities for Command Line Option Parsing

boolReader
    ∷ (Eq a, Show a, CI.FoldCase a, IsString a, IsString e, Monoid e)
    ⇒ a
    → Either e Bool
boolReader x = case CI.mk x of
    "true" → Right True
    "false" → Right False
    _ → Left $ "failed to read Boolean value " ⊕ fromString (show x)
        ⊕ ". Expected either \"true\" or \"false\""

-- | The 'boolOption' is an alternative to 'O.switch'.
--
-- Using 'O.switch' with command line parsers that overwrite settings
-- from a configuration file is problematic: the absence of the 'switch'
-- is interpreted as setting the respective configuration value to 'False'.
-- So there is no way to specify on the command line that the value from
-- the configuration file shall be used. Some command line UIs use two
-- different options for those values, for instance @--enable-feature@ and
-- @--disable-feature@. This option instead expects a Boolean value. Beside
-- that it behaves like any other option.
--
boolOption
    ∷ O.Mod O.OptionFields Bool
    → O.Parser Bool
boolOption mods = O.option (O.eitherReader (boolReader ∷ String → Either String Bool))
    % O.metavar "true|false"
    ⊕ O.completeWith ["true", "false", "TRUE", "FALSE", "True", "False"]
    ⊕ mods

-- | An alternative syntax for 'boolOption' for options with long names.
--
-- Instead of taking a boolean argument the presence of the option acts as a
-- switch to set the respective configuration setting to 'True'. If the option
-- is not present the setting is left unchanged.
--
-- In addition for long option names a respective /unset flag/ is provided. For
-- instance for a flag @--verbose@ there will also be a flag @--no-verbose@.
--
-- This can still be used with short option names only, but no /unset flag/
-- would be provided.
--
boolOption_
    ∷ O.Mod O.FlagFields Bool
    → O.Parser Bool
boolOption_ mods = flag' True mods <|> flag' False nomods
  where
    O.Mod f d o = mods
    O.FlagFields names _ = f $ O.FlagFields [] False

    longName (O.OptShort _) = Nothing
    longName (O.OptLong l) = Just l
    longNames = mapMaybe longName names

    noName l = "no-" ⊕ l
    mapFlags flags = flags
        { O.flagNames = mapMaybe (\l → O.OptLong ∘ noName <$> longName l) (O.flagNames flags)
        }
    nomods = O.Mod (mapFlags ∘ f) d o
        ⊕ maybe mempty (\l → help $ "unset flag " ⊕ l) (listToMaybe $ reverse longNames)

-- | An option parser for flags that are enabled via the flag name prefixed
-- with @--enable-@ and disabled via the flag name prefix @--disable-@. The
-- prefixes are applied to all long option names. Short option names are parsed
-- unchanged and and cause the flag to be enabled.
--
-- This resembles the style of flags that is used for instances with Cabal.
--
enableDisableFlag
    ∷ O.Mod O.FlagFields Bool
    → O.Parser Bool
enableDisableFlag mods = flag' True enmods <|> flag' False dismods
  where
    O.Mod f d o = mods
    O.FlagFields names _ = f $ O.FlagFields [] False

    longName (O.OptShort _) = Nothing
    longName (O.OptLong l) = Just l
    longNames = mapMaybe longName names

    disName l = "disable-" ⊕ l
    enName l = "enable-" ⊕ l

    -- disable flags
    mapDisFlags flags = flags
        { O.flagNames = mapMaybe (\l → O.OptLong ∘ disName <$> longName l) (O.flagNames flags)
        }
    dismods = O.Mod (mapDisFlags ∘ f) d o
        ⊕ maybe mempty (\l → help $ "unset flag " ⊕ l) (listToMaybe $ reverse longNames)

    -- enable flags
    mapLong g (O.OptLong l) = O.OptLong (g l)
    mapLong _ s = s
    mapEnFlags flags = flags
        { O.flagNames = map (mapLong enName) (O.flagNames flags)
        }
    enmods = O.Mod (mapEnFlags ∘ f) d o

fileOption
    ∷ O.Mod O.OptionFields String
    → O.Parser FilePath
fileOption mods = O.strOption
    % O.metavar "FILE"
    ⊕ O.action "file"
    ⊕ mods

eitherReadP
    ∷ T.Text
    → P.ReadP a
    → T.Text
    → Either T.Text a
eitherReadP label p s =
    case [ x | (x,"") ← P.readP_to_S p (T.unpack s) ] of
        [x] → Right x
        [] → Left $ "eitherReadP: no parse for " ⊕ label ⊕ " of " ⊕ s
        _ → Left $ "eitherReadP: ambigous parse for " ⊕ label ⊕ " of " ⊕ s

jsonOption ∷ FromJSON a ⇒ Mod OptionFields a → O.Parser a
jsonOption = O.option jsonReader

jsonReader ∷ FromJSON a ⇒ ReadM a
jsonReader = eitherReader $ eitherDecode' ∘ BL8.pack

