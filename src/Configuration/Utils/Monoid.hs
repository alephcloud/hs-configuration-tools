{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- The distinction between appending on the left and appending on
-- the right is important for monoids that are sensitive to ordering
-- such as 'List'. It is also of relevance for monoids with set semantics
-- with non-extensional equality such as `HashMap`.
--
module Configuration.Utils.Monoid
( LeftMonoidalUpdate
, leftMonoidalUpdate
, fromLeftMonoidalUpdate
, pLeftMonoidalUpdate
, RightMonoidalUpdate
, rightMonoidalUpdate
, fromRightMonoidalUpdate
, pRightMonoidalUpdate
) where

import Configuration.Utils.CommandLine
import Configuration.Utils.Internal

import Control.Monad.Except hiding (mapM_)
import Control.Monad.Writer hiding (mapM_)

import Data.Aeson

import qualified Options.Applicative.Types as O

import Prelude hiding (concatMap, mapM_, any)
import Prelude.Unicode

-- | Update a value by appending on the left. Under normal
-- circumstances you'll never use this type directly but only
-- its 'FromJSON' instance. See the 'leftMonoidalUpdate' for an example.
--
newtype LeftMonoidalUpdate α = LeftMonoidalUpdate
    { _getLeftMonoidalUpdate ∷ α
    }
    deriving (Monoid)

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
leftMonoidalUpdate ∷ Iso (LeftMonoidalUpdate α) (LeftMonoidalUpdate β) α β
leftMonoidalUpdate = iso _getLeftMonoidalUpdate LeftMonoidalUpdate

-- | This is the same as @from leftMonoidalUpdate@ but doesn't depend on
-- the lens Library.
--
fromLeftMonoidalUpdate ∷ Iso α β (LeftMonoidalUpdate α) (LeftMonoidalUpdate β)
fromLeftMonoidalUpdate = iso LeftMonoidalUpdate _getLeftMonoidalUpdate

instance (FromJSON α, Monoid α) ⇒ FromJSON (LeftMonoidalUpdate α → LeftMonoidalUpdate α) where
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
pLeftMonoidalUpdate ∷ Monoid α ⇒ O.Parser α → MParser α
pLeftMonoidalUpdate pElement = mappend ∘ mconcat ∘ reverse <$> many pElement

-- | Update a value by appending on the right. Under normal
-- circumstances you'll never use this type directly but only
-- its 'FromJSON' instance. See the 'leftMonoidalUpdate' for an example.
--
newtype RightMonoidalUpdate α = RightMonoidalUpdate
    { _getRightMonoidalUpdate ∷ α
    }
    deriving (Monoid)

-- | Update a value by appending on the right. See 'leftMonoidalUpdate' for
-- an usage example.
--
rightMonoidalUpdate ∷ Iso (RightMonoidalUpdate α) (RightMonoidalUpdate β) α β
rightMonoidalUpdate = iso _getRightMonoidalUpdate RightMonoidalUpdate

-- | This is the same as @from rightMonoidalUpdate@ but doesn't depend on
-- the lens Library.
--
fromRightMonoidalUpdate ∷ Iso α β (RightMonoidalUpdate α) (RightMonoidalUpdate β)
fromRightMonoidalUpdate = iso RightMonoidalUpdate _getRightMonoidalUpdate

instance (FromJSON α, Monoid α) ⇒ FromJSON (RightMonoidalUpdate α → RightMonoidalUpdate α) where
    parseJSON = fmap (flip mappend ∘ RightMonoidalUpdate) ∘ parseJSON

-- | Update a value by appending on the right. See 'pLeftMonoidalUpdate'
-- for an usage example.
--
pRightMonoidalUpdate ∷ Monoid α ⇒ O.Parser α → MParser α
pRightMonoidalUpdate pElement = flip mappend ∘ mconcat <$> many pElement

