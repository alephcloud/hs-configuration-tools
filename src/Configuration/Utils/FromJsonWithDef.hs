-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Configuration.Utils.FromJsonWithDef
(
-- * Parsing with Default Values
  FromJsonWithDef(..)
, decodeWithDef'
, (∴)

-- * Misc Utils
, checkUnexpected
, identifyJSON
) where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.HashMap.Strict as H
import Data.Monoid.Unicode
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Word
import Data.Maybe

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Misc Utils

identifyJSON ∷ B.ByteString → Either String Value
identifyJSON = A.eitherResult . A.parse json'

checkUnexpected ∷ String → [T.Text] → Object → Parser ()
checkUnexpected section props o =
    if H.null unexpected
        then return ()
        else fail $ "Unexpected properties in " ⊕ section ⊕ " : " ⊕ showUnexpected
    where
      unexpected = o `H.difference` (H.fromList ∘ map (,()) $ props)
      showUnexpected = B8.unpack ∘ encode ∘ Object $ unexpected

-- -------------------------------------------------------------------------- --
-- Parse With Default Values

decodeWithDef' ∷ FromJsonWithDef b ⇒ b → B.ByteString → Either String b
decodeWithDef' base s = do
    v ← identifyJSON s
    parseEither (parseJsonWithDef $ Just base) v

-- | The purpose of this class is to decode JSON objects that are missing some
-- properties.
--
-- The Aeson library provides the @.:?@ and @.!=@ operators for this purpose.
-- However usage of those operators requires static (a-priory known) default
-- value.
--
-- NOTE that the purpose is /NOT/ to compensate for parsing failures, but solely
-- to provide default values for omitted fields. In particular a field value of
-- @null@ is /NOT/ an omitted value. Of cource the parser invokes
-- 'parseJsonWithDefault' only on values that arn't ommited. This means that only
-- for types with some sort of indexed components there are meaningful
-- instances. For other types, in particular for types with strictly less the
-- two parameters in all constructors, the only meaningful instances omit any
-- given default value and fall back to 'parseJSON'.
--
-- For values with a 'FromJSON' instance there is a default implementation
-- that simply ignores the given default value.
--
-- Primitive types and types without a reasonable default logic instances should
-- satisfy
--
-- >  parseJsonWithDef _ ≡ parseJSON
--
-- For types with fromJSON instances that are objects instances should replace
-- missing properties with the respective property of the default value.
--
-- For sum types
--
-- TODO: It would be possible to generate instances for types with an FromJSON
-- isntance generically: 'parseJsonWithDef' would map the default tree onto
-- the parser structure such that if the parse "deviates" from the construction
-- of the default value, the default value wouldn't be used and any missing
-- value would result in a failure.
--
class FromJsonWithDef a where
    parseJsonWithDef ∷ Maybe a → Value → Parser a

    default parseJsonWithDef ∷ FromJSON a ⇒ Maybe a → Value → Parser a
    parseJsonWithDef _ = parseJSON

instance FromJsonWithDef ()
instance FromJsonWithDef Bool
instance FromJsonWithDef Int
instance FromJsonWithDef Integer
instance FromJsonWithDef Float
instance FromJsonWithDef Double
instance FromJsonWithDef Rational
instance FromJsonWithDef Word8
instance FromJsonWithDef Word16
instance FromJsonWithDef Word32
instance FromJsonWithDef Word64
instance FromJsonWithDef UTCTime
instance FromJSON NominalDiffTime ⇒ FromJsonWithDef NominalDiffTime
instance FromJsonWithDef String
instance FromJsonWithDef T.Text

instance (FromJSON a, FromJsonWithDef a) ⇒ FromJsonWithDef (Maybe a) where
    parseJsonWithDef (Just a) = parseJSON >=> \case
        Nothing → pure Nothing
        Just a_ → Just <$> parseJsonWithDef a a_
    parseJsonWithDef Nothing = parseJSON

instance
    ( FromJSON a, FromJsonWithDef a
    , FromJSON b, FromJsonWithDef b
    )
    ⇒ FromJsonWithDef (a, b)
  where
    parseJsonWithDef (Just (a,b)) = parseJSON >=> \(a_ ::Value, b_ ::Value) → (,)
        <$> parseJsonWithDef (Just a) a_
        <*> parseJsonWithDef (Just b) b_
    parseJsonWithDef Nothing = parseJSON

instance
    ( FromJSON a, FromJsonWithDef a
    , FromJSON b, FromJsonWithDef b
    , FromJSON c, FromJsonWithDef c
    )
    ⇒ FromJsonWithDef (a, b, c)
  where
    parseJsonWithDef (Just (a,b,c)) = parseJSON >=> \(a_,b_,c_) → (,,)
        <$> parseJsonWithDef (Just a) a_
        <*> parseJsonWithDef (Just b) b_
        <*> parseJsonWithDef (Just c) c_
    parseJsonWithDef Nothing = parseJSON

instance
    ( FromJSON a, FromJsonWithDef a
    , FromJSON b, FromJsonWithDef b
    )
    ⇒ FromJsonWithDef (Either a b)
  where
    parseJsonWithDef (Just (Right a)) = parseJSON >=> \case
        Right a_ → Right <$> parseJsonWithDef (Just a) a_
        Left b_ → Left <$> parseJsonWithDef Nothing b_
    parseJsonWithDef (Just (Left b)) = parseJSON >=> \case
        Right a_ → Right <$> parseJsonWithDef Nothing a_
        Left b_ → Left <$> parseJsonWithDef (Just b) b_
    parseJsonWithDef Nothing = parseJSON


-- | This instance applies the default values in order to the parsed list. If
-- the parse list is shorter than the default list, the missing trailing values
-- in the parse result a filled in from the default values.
--
instance (FromJSON a, FromJsonWithDef a) ⇒ FromJsonWithDef [a] where
    parseJsonWithDef (Just l) = parseJSON >=> \l_ → g <$> zipWithM
            f
            (map Just l ⊕ repeat Nothing)
            (map Just l_ ⊕ repeat Nothing)
      where
        f Nothing Nothing = pure Nothing
        f (Just a) Nothing = Just <$> pure a
        f a (Just a_) = Just <$> parseJsonWithDef a a_
        g = map (\(Just x) → x) ∘ takeWhile isJust
    parseJsonWithDef Nothing = parseJSON

-- TODO provide instances for Maps, and Sets.

-- | Parse an object field with a default value.
-- For using this ternary operator within ideomatic
-- applicative style code it can be combined with the '%'
-- operator that is defined below
--
-- @
-- data A = A { a ∷ Int, b ∷ Int }
--
-- instance FromJsonWithDef A where
--     parseJsonWithDef d = withObject "A" $ \o → A
--         <$> o ∴ "a" % fmap a d
--         <*> o ∴ "b" % fmap b d
-- @
--
-- The hex value of the UTF-8 character ∴ is 0x2234.
--
-- In vim type: @Ctrl-k .:@
--
(∴) ∷ (FromJsonWithDef a) ⇒ Object → T.Text → Maybe a → Parser a
(∴) o js d = case H.lookup js o of
    Nothing → maybe err return d
    Just v → parseJsonWithDef d v
  where
    err = fail $ "missing property " ⊕ T.unpack js
