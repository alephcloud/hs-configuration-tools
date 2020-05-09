{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Configuration.Utils.Interal
-- Description: Internal utilities of the configuration-tools package
-- Copyright: Copyright © 2014-2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
module Configuration.Utils.Internal
(
-- * Lenses
  lens
, over
, set
, view
, Lens'
, Lens
, Iso'
, Iso
, iso

-- * Misc Utils
, (&)
, (<&>)
, sshow
, exceptT
, errorT
) where

import Control.Applicative (Const(..))
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Except

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.Monoid.Unicode
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.String
import qualified Data.Text as T

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Lenses

-- Just what we need of van Laarhoven Lenses
--
-- These few lines of code do safe us a lot of dependencies

-- | This is the same type as the type from the lens library with the same name.
--
-- In case it is already import from the lens package this should be hidden
-- from the import.
--
type Lens s t a b = ∀ f . Functor f ⇒ (a → f b) → s → f t

-- | This is the same type as the type from the lens library with the same name.
--
-- In case it is already import from the lens package this should be hidden
-- from the import.
--
type Lens' s a = Lens s s a a

lens ∷ (s → a) → (s → b → t) → Lens s t a b
lens getter setter lGetter s = setter s `fmap` lGetter (getter s)
{-# INLINE lens #-}

over ∷ ((a → Identity b) → s → Identity t) → (a → b) → s → t
over s f = runIdentity . s (Identity . f)
{-# INLINE over #-}

set ∷ ((a → Identity b) → s → Identity t) → b → s → t
set s a = runIdentity . s (const $ Identity a)
{-# INLINE set #-}

view ∷ MonadReader r m ⇒ ((a → Const a a) → r → Const a r) → m a
view l = asks (getConst #. l Const)
{-# INLINE view #-}

-- | This is the same type as the type from the lens library with the same name.
--
-- In case it is already import from the lens package this should be hidden
-- from the import.
--
type Iso s t a b = ∀ p f . (Profunctor p, Functor f) ⇒ p a (f b) → p s (f t)
type Iso' s a = Iso s s a a

iso ∷ (s → a) → (b → t) → Iso s t a b
iso f g = dimap f (fmap g)
{-# INLINE iso #-}

-- -------------------------------------------------------------------------- --
-- Misc Utils

sshow
    ∷ (Show a, IsString s)
    ⇒ a
    → s
sshow = fromString ∘ show
{-# INLINE sshow #-}

exceptT
    ∷ Monad m
    ⇒ (e → m b)
    → (a → m b)
    → ExceptT e m a
    → m b
exceptT a b = runExceptT >=> either a b
{-# INLINE exceptT #-}

errorT
    ∷ Monad m
    ⇒ ExceptT T.Text m a
    → m a
errorT = exceptT (\e → error ∘ T.unpack $ "Error: " ⊕ e) return
{-# INLINE errorT #-}

