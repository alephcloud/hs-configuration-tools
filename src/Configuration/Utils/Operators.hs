{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

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
) where

-- -------------------------------------------------------------------------- --
-- Useful Operators

-- | This operator is an alternative for '$' with a higher precedence. It is
-- suitable for usage within applicative style code without the need to add
-- parenthesis.
--
(%) ∷ (a → b) → a → b
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
(×) ∷ (a → b) → a → b
(×) = ($)
infixr 5 ×
{-# INLINE (×) #-}
{-# DEPRECATED (×) "use '%' instead" #-}

-- | Functional composition for applicative functors.
--
(<*<) ∷ Applicative f ⇒ f (b → c) → f (a → b) → f (a → c)
(<*<) a b = ((.) <$> a) <*> b
infixr 4 <*<
{-# INLINE (<*<) #-}

-- | Functional composition for applicative functors with its arguments
-- flipped.
--
(>*>) ∷ Applicative f ⇒ f (a → b) → f (b → c) → f (a → c)
(>*>) = flip (<*<)
infixr 4 >*>
{-# INLINE (>*>) #-}

-- | Applicative functional composition between a pure function
-- and an applicative function.
--
(<$<) ∷ Functor f ⇒ (b → c) → f (a → b) → f (a → c)
(<$<) a b = (a .) <$> b
infixr 4 <$<
{-# INLINE (<$<) #-}

-- | Applicative functional composition between a pure function
-- and an applicative function with its arguments flipped.
--
(>$>) ∷ Functor f ⇒ f (a → b) → (b → c) → f (a → c)
(>$>) = flip (<$<)
infixr 4 >$>
{-# INLINE (>$>) #-}
