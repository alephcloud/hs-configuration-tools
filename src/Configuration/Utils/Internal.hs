-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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
, sshow
, exceptT
, errorT
) where

import Control.Applicative (Const(..))
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Except

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
type Lens σ τ α β = Functor φ ⇒ (α → φ β) → σ → φ τ

-- | This is the same type as the type from the lens library with the same name.
--
-- In case it is already import from the lens package this should be hidden
-- from the import.
--
type Lens' σ α = Lens σ σ α α

lens ∷ (σ → α) → (σ → β → τ) → Lens σ τ α β
lens getter setter lGetter s = setter s `fmap` lGetter (getter s)
{-# INLINE lens #-}

over ∷ ((α → Identity α) → β → Identity β) → (α → α) → β → β
over s f = runIdentity . s (Identity . f)
{-# INLINE over #-}

set ∷ ((α → Identity α) → β → Identity β) → α → β → β
set s a = runIdentity . s (const $ Identity a)
{-# INLINE set #-}

view ∷ MonadReader σ μ ⇒ ((α → Const α α) → σ → Const α σ) → μ α
view l = asks (getConst #. l Const)

-- | This is the same type as the type from the lens library with the same name.
--
-- In case it is already import from the lens package this should be hidden
-- from the import.
--
type Iso σ τ α β = (Profunctor π, Functor φ) ⇒ π α (φ β) → π σ (φ τ)
type Iso' σ α = Iso σ σ α α

iso ∷ (σ → α) → (β → τ) → Iso σ τ α β
iso f g = dimap f (fmap g)
{-# INLINE iso #-}

-- -------------------------------------------------------------------------- --
-- Misc Utils

sshow
    ∷ (Show α, IsString τ)
    ⇒ α
    → τ
sshow = fromString ∘ show

exceptT
    ∷ Monad μ
    ⇒ (ε → μ β)
    → (α → μ β)
    → ExceptT ε μ α
    → μ β
exceptT a b = runExceptT >=> either a b

errorT
    ∷ Monad μ
    ⇒ ExceptT T.Text μ α
    → μ α
errorT = exceptT (\e → error ∘ T.unpack $ "Error: " ⊕ e) return

