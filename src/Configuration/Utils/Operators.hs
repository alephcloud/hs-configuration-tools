{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-- |
-- Module: Configuration.Utils.Operators
-- Description: Useful operators for defining functions in an applicative context
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- Useful operators for defining functions in an applicative context
--
module Configuration.Utils.Operators
( (%)
, (×)
, (<*<)
, (>*>)
, (<$<)
, (>$>)
, (<.>)
, (⊙)
) where

#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- -------------------------------------------------------------------------- --
-- Useful Operators

-- | This operator is an alternative for '$' with a higher precedence. It is
-- suitable for usage within applicative style code without the need to add
-- parenthesis.
--
(%) ∷ (α → β) → α → β
(%) = ($)
infixr 5 %
{-# INLINE (%) #-}

-- | This operator is a UTF-8 version of '%' which is an alternative for '$'
-- with a higher precedence. It is suitable for usage within applicative style
-- code without the need to add parenthesis.
--
-- The hex value of the UTF-8 character × is 0x00d7.
--
-- In VIM type: @Ctrl-V u 00d7@
--
-- You may also define a key binding by adding something like the following line
-- to your vim configuration file:
--
-- > iabbrev <buffer> >< ×
--
(×) ∷ (α → β) → α → β
(×) = ($)
infixr 5 ×
{-# INLINE (×) #-}

-- | Functional composition for applicative functors.
--
(<*<) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(<*<) a b = pure (.) <*> a <*> b
infixr 4 <*<
{-# INLINE (<*<) #-}

-- | Functional composition for applicative functors with its arguments
-- flipped.
--
(>*>) ∷ Applicative φ ⇒ φ (α → β) → φ (β → γ) → φ (α → γ)
(>*>) = flip (<*<)
infixr 4 >*>
{-# INLINE (>*>) #-}

-- | Applicative functional composition between a pure function
-- and an applicative function.
--
(<$<) ∷ Functor φ ⇒ (β → γ) → φ (α → β) → φ (α → γ)
(<$<) a b = (a .) <$> b
infixr 4 <$<
{-# INLINE (<$<) #-}

-- | Applicative functional composition between a pure function
-- and an applicative function with its arguments flipped.
--
(>$>) ∷ Functor φ ⇒ φ (α → β) → (β → γ) → φ (α → γ)
(>$>) = flip (<$<)
infixr 4 >$>
{-# INLINE (>$>) #-}

-- | Functional composition for applicative functors.
--
-- This is a rather popular operator. Due to conflicts (for instance with the
-- lens package) it may have to be imported qualified.
--
(<.>) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(<.>) = (<*<)
infixr 4 <.>
{-# INLINE (<.>) #-}
{-# DEPRECATED (<.>) "use '<*<' instead" #-}

-- | For people who like nicely aligned code and do not mind messing with
-- editor key-maps: here a version of '<.>' that uses a unicode symbol
--
-- The hex value of the UTF-8 character ⊙ is 0x2299.
--
-- A convenient VIM key-map is:
--
-- > iabbrev <buffer> ../ ⊙
--
(⊙) ∷ Applicative φ ⇒ φ (β → γ) → φ (α → β) → φ (α → γ)
(⊙) = (<.>)
infixr 4 ⊙
{-# INLINE (⊙) #-}
{-# DEPRECATED (⊙) "use '<*<' instead" #-}

