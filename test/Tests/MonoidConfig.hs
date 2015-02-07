{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Tests.MonoidConfig
-- Description: Test cases for monoidal configuration types
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
module Tests.MonoidConfig
( monoidUpdateTests
) where

import TestTools

import Configuration.Utils
import Configuration.Utils.Internal
import Configuration.Utils.Validation

import qualified Data.HashMap.Strict as HM
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Test cases

monoidUpdateTests ∷ PkgInfo → [IO Bool]
monoidUpdateTests pkgInfo = concatMap ($ pkgInfo)
    [ routingTableTests
    , textAppendTestsR
    , textAppendTestsFilesR
    , textAppendTestsL
    , textAppendTestsFilesL
    ]

-- -------------------------------------------------------------------------- --
-- HashMap

newtype RoutingTable = RoutingTable { _routingTableMap ∷ HM.HashMap T.Text T.Text }

routingTableMap ∷ Lens' RoutingTable (HM.HashMap T.Text T.Text)
routingTableMap = lens _routingTableMap $ \a b → a { _routingTableMap = b }

defaultRoutingTable ∷ RoutingTable
defaultRoutingTable = RoutingTable HM.empty

instance ToJSON RoutingTable where
    toJSON RoutingTable{..} = object
        [ "route_map" .= _routingTableMap
        ]

instance FromJSON (RoutingTable → RoutingTable) where
    parseJSON = withObject "RoutingTable" $ \o → id
        <$< routingTableMap . fromLeftMonoidalUpdate %.: "route_map" × o

pRoutingTable ∷ MParser RoutingTable
pRoutingTable = routingTableMap %:: pLeftMonoidalUpdate pRoute
  where
    pRoute = option (eitherReader readRoute)
        × long "route"
        ⊕ help "add a route to the routing table; the APIROUTE part must not contain a colon character"
        ⊕ metavar "APIROUTE:APIURL"

    readRoute s = case break (== ':') s of
        (a,':':b) → fmapL T.unpack $ do
            validateNonEmpty "APIROUTE" a
            validateHttpOrHttpsUrl "APIURL" b
            return $ HM.singleton (T.pack a) (T.pack b)
        _ → Left "missing colon between APIROUTE and APIURL"

mainInfoRoutingTable ∷ ProgramInfoValidate RoutingTable []
mainInfoRoutingTable = programInfoValidate "Routing Table" pRoutingTable defaultRoutingTable (const $ return ())

-- Test Cases

routingTableTests ∷ PkgInfo → [IO Bool]
routingTableTests pkgInfo =
    [ run 0 [ConfAssertion ["--route=a:" ⊕ b0] (routingTableMap ∘ at "a") $ Just b0]
    , run 1 [ConfAssertion ["--route=a:" ⊕ b0, "--route=a:" ⊕ b1] (routingTableMap ∘ at "a") $ Just b1]
    , run 2 [ConfAssertion ["--route=a:" ⊕ b0, "--route=a:" ⊕ b1] (routingTableMap ∘ at "a") $ Just b1]
    , run 3 [ConfAssertion ["--route=a:" ⊕ b0, "--route=b:" ⊕ b1] (routingTableMap ∘ at "a") $ Just b0]
    , run 4 [ConfAssertion ["--route=a:" ⊕ b0, "--route=b:" ⊕ b1] (routingTableMap ∘ at "b") $ Just b1]
    , run 5 [ConfAssertion ["--route=a:" ⊕ b0, "--route=b:" ⊕ b1] (routingTableMap ∘ at "c") Nothing]
    ]
  where
    b0,b1 ∷ IsString a ⇒ a
    b0 = "http://b0"
    b1 = "https://b1"
    run (x ∷ Int) = runTest pkgInfo mainInfoRoutingTable ("routing-table-" ⊕ sshow x) True

    at k f m = f mv <&> \r → case r of
        Nothing → maybe m (const (HM.delete k m)) mv
        Just v' → HM.insert k v' m
      where
        mv = HM.lookup k m

-- -------------------------------------------------------------------------- --
-- Text with right append

newtype StringConfigR = StringConfigR { _stringConfigR ∷ T.Text }

stringConfigR ∷ Lens' StringConfigR T.Text
stringConfigR = lens _stringConfigR $ \a b → a { _stringConfigR = b }

defaultStringConfigR ∷ StringConfigR
defaultStringConfigR = StringConfigR "|"

instance ToJSON StringConfigR where
    toJSON StringConfigR{..} = object
        [ "string" .= _stringConfigR
        ]

instance FromJSON (StringConfigR → StringConfigR) where
    parseJSON = withObject "StringConfigR" $ \o → id
        <$< stringConfigR . fromRightMonoidalUpdate %.: "string" × o

pStringConfigR ∷ MParser StringConfigR
pStringConfigR = stringConfigR %:: pRightMonoidalUpdate pString
  where
    pString = T.pack <$> strOption × long "string"

-- Test cases

textAppendTestsR ∷ PkgInfo → [IO Bool]
textAppendTestsR pkgInfo =
    [ run 0 True [ConfAssertion [] stringConfigR "|"]
    , run 1 True [ConfAssertion ["--string=a"] stringConfigR "|a"]

    , run 2 True [ConfAssertion ["--string=a", "--string=b"] stringConfigR "|ab"]
    , run 3 False [ConfAssertion ["--string=a", "--string=b"] stringConfigR "|ba"]
    , run 4 False [ConfAssertion ["--string=b", "--string=a"] stringConfigR "|ab"]
    , run 5 True [ConfAssertion ["--string=b", "--string=a"] stringConfigR "|ba"]

    , run 6 False [ConfAssertion ["--string=aaa", "--string=bbb"] stringConfigR "|bbbaaa"]
    , run 7 True [ConfAssertion ["--string=aaa", "--string=bbb"] stringConfigR "|aaabbb"]
    , run 8 True [ConfAssertion ["--string=bbb", "--string=aaa"] stringConfigR "|bbbaaa"]
    , run 9 False [ConfAssertion ["--string=bbb", "--string=aaa"] stringConfigR "|aaabbb"]
    ]
  where
    run (x ∷ Int) = runTest pkgInfo mi ("stringR-" ⊕ sshow x)
    mi = programInfoValidate "Text right append" pStringConfigR defaultStringConfigR (const $ return ())

textAppendTestsFilesR ∷ PkgInfo → [IO Bool]
textAppendTestsFilesR pkgInfo =
    [ run ca 0 True [ConfAssertion [] stringConfigR "|a"]

    , run ca 2 True [ConfAssertion ["--string=b"] stringConfigR "|ab"]
    , run ca 3 False [ConfAssertion ["--string=b"] stringConfigR "|ba"]
    , run cb 4 False [ConfAssertion ["--string=a"] stringConfigR "|ab"]
    , run cb 5 True [ConfAssertion ["--string=a"] stringConfigR "|ba"]

    , run2 ca ca 6 True [ConfAssertion [] stringConfigR "|aa"]
    , run2 ca cb 6 False [ConfAssertion [] stringConfigR "|ba"]
    , run2 ca cb 7 True [ConfAssertion [] stringConfigR "|ab"]
    , run2 cb ca 8 True [ConfAssertion [] stringConfigR "|ba"]
    , run2 cb ca 9 False [ConfAssertion [] stringConfigR "|ab"]
    ]
  where
    ca = StringConfigR "a"
    cb = StringConfigR "b"
    run c (x ∷ Int) b a = withConfigFile c $ \file →
        runTest pkgInfo (mi [file]) ("stringR-file1-" ⊕ sshow x) b a

    run2 c0 c1 (x ∷ Int) b a =
        withConfigFile c0 $ \file0 →
        withConfigFile c1 $ \file1 →
        runTest pkgInfo (mi [file0,file1]) ("stringR-file2-" ⊕ sshow x) b a

    mi files = set piConfigurationFiles (map ConfigFileRequired files) $
      programInfoValidate "Text right append with files" pStringConfigR defaultStringConfigR (const $ return ())

-- -------------------------------------------------------------------------- --
-- Text with left append

newtype StringConfigL = StringConfigL { _stringConfigL ∷ T.Text }

stringConfigL ∷ Lens' StringConfigL T.Text
stringConfigL = lens _stringConfigL $ \a b → a { _stringConfigL = b }

defaultStringConfigL ∷ StringConfigL
defaultStringConfigL = StringConfigL "|"

instance ToJSON StringConfigL where
    toJSON StringConfigL{..} = object
        [ "string" .= _stringConfigL
        ]

instance FromJSON (StringConfigL → StringConfigL) where
    parseJSON = withObject "StringConfigL" $ \o → id
        <$< stringConfigL . fromLeftMonoidalUpdate %.: "string" × o

pStringConfigL ∷ MParser StringConfigL
pStringConfigL = stringConfigL %:: pLeftMonoidalUpdate pString
  where
    pString = T.pack <$> strOption × long "string"

-- Test cases

textAppendTestsL ∷ PkgInfo → [IO Bool]
textAppendTestsL pkgInfo =
    [ run 0 True [ConfAssertion [] stringConfigL "|"]
    , run 1 True [ConfAssertion ["--string=a"] stringConfigL "a|"]

    , run 2 True [ConfAssertion ["--string=a", "--string=b"] stringConfigL "ba|"]
    , run 3 False [ConfAssertion ["--string=a", "--string=b"] stringConfigL "ab|"]
    , run 4 False [ConfAssertion ["--string=b", "--string=a"] stringConfigL "ba|"]
    , run 5 True [ConfAssertion ["--string=b", "--string=a"] stringConfigL "ab|"]

    , run 6 True [ConfAssertion ["--string=aaa", "--string=bbb"] stringConfigL "bbbaaa|"]
    , run 7 False [ConfAssertion ["--string=aaa", "--string=bbb"] stringConfigL "aaabbb|"]
    , run 8 False [ConfAssertion ["--string=bbb", "--string=aaa"] stringConfigL "bbbaaa|"]
    , run 9 True [ConfAssertion ["--string=bbb", "--string=aaa"] stringConfigL "aaabbb|"]
    ]
  where
    run (x ∷ Int) = runTest pkgInfo mi ("stringL-" ⊕ sshow x)
    mi = programInfoValidate "Text left append" pStringConfigL defaultStringConfigL (const $ return ())

textAppendTestsFilesL ∷ PkgInfo → [IO Bool]
textAppendTestsFilesL pkgInfo =
    [ run ca 1 True [ConfAssertion [] stringConfigL "a|"]

    , run ca 2 True [ConfAssertion ["--string=b"] stringConfigL "ba|"]
    , run ca 3 False [ConfAssertion ["--string=b"] stringConfigL "ab|"]
    , run cb 4 False [ConfAssertion ["--string=a"] stringConfigL "ba|"]
    , run cb 5 True [ConfAssertion ["--string=a"] stringConfigL "ab|"]

    , run2 ca ca 1 True [ConfAssertion [] stringConfigL "aa|"]
    , run2 ca cb 2 True [ConfAssertion [] stringConfigL "ba|"]
    , run2 ca cb 3 False [ConfAssertion [] stringConfigL "ab|"]
    , run2 cb ca 4 False [ConfAssertion [] stringConfigL "ba|"]
    , run2 cb ca 5 True [ConfAssertion [] stringConfigL "ab|"]
    ]
  where
    ca = StringConfigL "a"
    cb = StringConfigL "b"
    run c (x ∷ Int) b a = withConfigFile c $ \file →
        runTest pkgInfo (mi [file]) ("stringL-file1-" ⊕ sshow x) b a

    run2 c0 c1 (x ∷ Int) b a =
        withConfigFile c0 $ \file0 →
        withConfigFile c1 $ \file1 →
        runTest pkgInfo (mi [file0,file1]) ("stringL-file2-" ⊕ sshow x) b a

    mi files = set piConfigurationFiles (map ConfigFileRequired files) $
      programInfoValidate "Text left append with file" pStringConfigL defaultStringConfigL (const $ return ())

