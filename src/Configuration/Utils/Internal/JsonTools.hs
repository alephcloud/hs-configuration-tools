{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.Internal.JsonTools
-- Copyright: Copyright © 2020 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- The difference algorithms uses the following identies on JSON Values:
--
-- * An array equals the same array with all Null entries removed.
-- * An object equals the same object with all Null valued properties removed.
--
module Configuration.Utils.Internal.JsonTools
( Diff(..)
, diff
, resolve

-- * Conflict Resoluation Strategies
, merge
, mergeLeft
, mergeRight
, resolveLeft
, resolveOnlyLeft
, resolveRight
, resolveOnlyRight
) where

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import GHC.Generics

-- -------------------------------------------------------------------------- --
-- Representation of Difference between to Values

-- | Represent differences between two values
--
data Diff a
    = OnlyLeft a
    | OnlyRight a
    | Conflict a a
    | Both a
    deriving (Eq, Ord, Generic)

instance ToJSON a ⇒ ToJSON (Diff a) where
    toJSON (OnlyLeft a) = object ["$left" .= a]
    toJSON (OnlyRight a) = object ["$right" .= a]
    toJSON (Both a) = object ["$both" .= a]
    toJSON (Conflict a b) = object ["$left" .= a, "$right" .= b]
    {-# INLINE toJSON #-}

instance FromJSON a ⇒ FromJSON (Diff a) where
    parseJSON a = conflict a <|> right a <|> left a <|> both a
      where
        conflict = withObject "Diff.Conflict" $ \o → Conflict
            <$> o .: "$left"
            <*> o .: "$right"
        right = withObject "Diff.OnlyRight" $ \o → OnlyRight
            <$> o .: "$right"
        left = withObject "Diff.OnlyLeft" $ \o → OnlyLeft
            <$> o .: "$left"
        both = withObject "Diff.Both" $ \o → Both
            <$> o .: "$both"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Resolve Diff Value

-- | Resolve differences between two JSON values using the provided conflict
-- resolution function.
--
resolve ∷ (Diff Value → Value) → Value → Value
resolve f = go
  where
    go v = case f <$> parseMaybe parseJSON v of
        Just x → x
        Nothing → case v of
            (Object a) → Object $ HM.filter (/= Null) $ go <$> a
            (Array a) → Array $ V.filter (/= Null) $ go <$> a
            a → a

-- | Merge all non-conflicting differences. Leave the conflict annotations in
-- the result.
--
merge ∷ Diff Value → Value
merge (OnlyLeft a) = a
merge (OnlyRight a) = a
merge (Conflict a b) = toJSON $ Conflict a b
merge (Both a) = a

-- | Merge all differences. Pick the left value in case of a conflict.
--
mergeLeft ∷ Diff Value → Value
mergeLeft (OnlyLeft a) = a
mergeLeft (OnlyRight a) = a
mergeLeft (Conflict a _) = a
mergeLeft (Both a) = a

-- | Merge all differences. Pick the right value in case of a conflict.
--
mergeRight ∷ Diff Value → Value
mergeRight (OnlyLeft a) = a
mergeRight (OnlyRight a) = a
mergeRight (Conflict _ a) = a
mergeRight (Both a) = a

-- | Resolve all differences by choosing the left value.
--
resolveLeft ∷ Diff Value → Value
resolveLeft (OnlyLeft a) = a
resolveLeft (OnlyRight _) = Null
resolveLeft (Conflict a _) = a
resolveLeft (Both a) = a

-- | Keep values that /only/ occure in the left value. Remove all values that
-- occur in the right value or in both.
--
-- The result is the left value minus the right value.
--
resolveOnlyLeft ∷ Diff Value → Value
resolveOnlyLeft (OnlyLeft a) = a
resolveOnlyLeft (OnlyRight _) = Null
resolveOnlyLeft (Conflict a _) = a
resolveOnlyLeft (Both _) = Null

-- | Resolve all differences by choosing the right value.
--
resolveRight ∷ Diff Value → Value
resolveRight (OnlyLeft _) = Null
resolveRight (OnlyRight a) = a
resolveRight (Conflict _ a) = a
resolveRight (Both a) = a

-- | Keep values that /only/ occure in the right value. Remove all values that
-- occur in the left value or in both.
--
-- The result is the right value minus the left value.
--
resolveOnlyRight ∷ Diff Value → Value
resolveOnlyRight (OnlyLeft _) = Null
resolveOnlyRight (OnlyRight a) = a
resolveOnlyRight (Conflict _ a) = a
resolveOnlyRight (Both _) = Null

-- -------------------------------------------------------------------------- --
-- Compute Difference between two JSON Values

-- | Merge two JSON values and annotate the result with the differences.
--
diff ∷ Value → Value → Value
diff a b | a == b = toJSON $ Both a
diff (Object a) (Object b) = Object $ mergeObjects a b
diff (Array a) (Array b) = Array $ mergeVectors a b
diff a b
    | a == Null = toJSON $ OnlyRight b
    | b == Null = toJSON $ OnlyLeft a
    | otherwise = toJSON $ Conflict a b

mergeObjects ∷ Object → Object → Object
mergeObjects l r
    = (toJSON . OnlyLeft <$> HM.difference l r)
    <> (toJSON . OnlyRight <$> HM.difference r l)
    <> HM.intersectionWith diff l r

-- | A naive list merge with a lookAhead of 1
--
mergeVectors ∷ Array → Array → Array
mergeVectors a b = V.fromList $ toJSON <$> go (toList a) (toList b)
  where
    go a' [] = OnlyLeft <$> a'
    go [] b' = OnlyRight <$> b'
    go al@(ha0 : ha1 : ta) bl@(hb0 : hb1 : tb)
        | ha0 == hb0 = Both ha0 : go (ha1 : ta) (hb1 : tb)
        | ha0 == hb1 = OnlyRight hb0 : go al (hb1 : tb)
        | ha1 == hb0 = OnlyLeft ha0 : go (ha1 : ta) bl
        | otherwise = Conflict ha0 hb0 : go (ha1 : ta) (hb1 : tb)
    go (ha0 : ta) (hb0 : tb)
        | ha0 == hb0 = Both ha0 : go ta tb
        | otherwise = Conflict ha0 hb0 : go ta tb

