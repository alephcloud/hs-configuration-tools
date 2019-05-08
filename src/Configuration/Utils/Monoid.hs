{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.Monoid
-- Description: Configuration of Monoids
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- The distinction between appending on the left and appending on the right is
-- important for monoids that are sensitive to ordering such as 'List'. It is
-- also of relevance for monoids with set semantics with non-extensional
-- equality such as `HashMap`.
--
module Configuration.Utils.Monoid
  ( LeftMonoidalUpdate
  , leftMonoidalUpdate
  , fromLeftMonoidalUpdate
  , pLeftMonoidalUpdate
  , pLeftSemigroupalUpdate
  , RightMonoidalUpdate
  , rightMonoidalUpdate
  , fromRightMonoidalUpdate
  , pRightMonoidalUpdate
  ) where

import Configuration.Utils.CommandLine
import Configuration.Utils.Internal

import Control.Monad.Writer hiding (mapM_)

import Data.Aeson
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup
import Data.Semigroup.Foldable (fold1)

import qualified Options.Applicative.Types as O

import Prelude hiding (any, concatMap, mapM_)
import Prelude.Unicode

-- | Update a value by appending on the left. Under normal
-- circumstances you'll never use this type directly but only
-- its 'FromJSON' instance. See the 'leftMonoidalUpdate' for an example.
--
newtype LeftMonoidalUpdate a = LeftMonoidalUpdate
    { _getLeftMonoidalUpdate ∷ a
    }
    deriving (Semigroup, Monoid)

-- | Update a value by appending on the left.
--
-- > newtype RoutingTable = RoutingTable { _routingTableMap ∷ HashMap T.Text T.Text }
-- >
-- > $(makeLenses ''RoutingTable)
-- >
-- > instance FromJSON (RoutingTable → RoutingTable) where
-- >     parseJSON = withObject "RoutingTable" $ \o → id
-- >         <$< routingTableMap . from leftMonoidalUpdate %.: "route_map" % o
--
leftMonoidalUpdate ∷ Iso (LeftMonoidalUpdate a) (LeftMonoidalUpdate b) a b
leftMonoidalUpdate = iso _getLeftMonoidalUpdate LeftMonoidalUpdate

-- | This is the same as @from leftMonoidalUpdate@ but doesn't depend on
-- the lens Library.
--
fromLeftMonoidalUpdate ∷ Iso a b (LeftMonoidalUpdate a) (LeftMonoidalUpdate b)
fromLeftMonoidalUpdate = iso LeftMonoidalUpdate _getLeftMonoidalUpdate

instance (FromJSON a, Monoid a) ⇒ FromJSON (LeftMonoidalUpdate a → LeftMonoidalUpdate a) where
    parseJSON = fmap (mappend ∘ LeftMonoidalUpdate) ∘ parseJSON

-- | Update a value by appending on the left.
--
-- > newtype RoutingTable = RoutingTable { _routingTableMap ∷ HashMap T.Text T.Text }
-- >
-- > $(makeLenses ''RoutingTable)
-- >
-- > pRoutingTable ∷ MParser RoutingTable
-- > pRoutingTable = routingTableMap %:: pLeftMonoidalUpdate pRoute
-- >   where
-- >     pRoute = option (eitherReader readRoute)
-- >         % long "route"
-- >         <> help "add a route to the routing table; the APIROUTE part must not contain a colon character"
-- >         <> metavar "APIROUTE:APIURL"
-- >
-- >     readRoute s = case break (== ':') s of
-- >         (a,':':b) → fmapL T.unpack $ do
-- >             validateNonEmpty "APIROUTE" a
-- >             validateHttpOrHttpsUrl "APIURL" b
-- >             return $ HM.singleton (T.pack a) (T.pack b)
-- >         _ → Left "missing colon between APIROUTE and APIURL"
-- >
-- >     fmapL f = either (Left . f) Right
--
pLeftMonoidalUpdate ∷ Monoid a ⇒ O.Parser a → MParser a
pLeftMonoidalUpdate pElement = mappend ∘ mconcat ∘ reverse <$> many pElement

-- | Like `pLeftMonoidalUpdate`, but works for `Semigroup`s instead. Using this
-- parser requires the input to have at least one copy (say, for flags that can
-- be passed multiple times).
--
pLeftSemigroupalUpdate ∷ Semigroup a ⇒ O.Parser a → MParser a
pLeftSemigroupalUpdate pElement = (<>) ∘ fold1 ∘ NEL.fromList ∘ reverse <$> some pElement

-- | Update a value by appending on the right. Under normal
-- circumstances you'll never use this type directly but only
-- its 'FromJSON' instance. See the 'leftMonoidalUpdate' for an example.
--
newtype RightMonoidalUpdate a = RightMonoidalUpdate
    { _getRightMonoidalUpdate ∷ a
    }
    deriving (Semigroup, Monoid)

-- | Update a value by appending on the right. See 'leftMonoidalUpdate' for
-- an usage example.
--
rightMonoidalUpdate ∷ Iso (RightMonoidalUpdate a) (RightMonoidalUpdate b) a b
rightMonoidalUpdate = iso _getRightMonoidalUpdate RightMonoidalUpdate

-- | This is the same as @from rightMonoidalUpdate@ but doesn't depend on
-- the lens Library.
--
fromRightMonoidalUpdate ∷ Iso a b (RightMonoidalUpdate a) (RightMonoidalUpdate b)
fromRightMonoidalUpdate = iso RightMonoidalUpdate _getRightMonoidalUpdate

instance (FromJSON a, Monoid a) ⇒ FromJSON (RightMonoidalUpdate a → RightMonoidalUpdate a) where
    parseJSON = fmap (flip mappend ∘ RightMonoidalUpdate) ∘ parseJSON

-- | Update a value by appending on the right. See 'pLeftMonoidalUpdate'
-- for an usage example.
--
pRightMonoidalUpdate ∷ Monoid a ⇒ O.Parser a → MParser a
pRightMonoidalUpdate pElement = flip mappend ∘ mconcat <$> many pElement
