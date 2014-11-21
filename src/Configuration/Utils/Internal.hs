-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Configuration.Utils.Internal
( lens
, over
, set
, view
, Lens'
, Lens
, Iso'
, iso
) where

import Control.Applicative (Const(..))
import Control.Monad.Reader.Class

import Data.Functor.Identity
import Data.Profunctor
import Data.Profunctor.Unsafe

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
type Iso' β α = (Profunctor π, Functor φ) ⇒ π α (φ α) → π β (φ β)

iso ∷ (β → α) → (α → β) → Iso' β α
iso f g = dimap f (fmap g)
{-# INLINE iso #-}

