{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
module Main
( main
) where

import TestTools

import Configuration.Utils
import Configuration.Utils.Internal
import Configuration.Utils.Validation

import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Example hiding (main)

import Prelude.Unicode

import PkgInfo_url_example_test

-- -------------------------------------------------------------------------- --
-- main

main ∷ IO ()
main =
    withConfigFile config0 $ \tmpPath0 →
    withConfigFileText config1Part$ \tmpPath1 → do

#ifdef REMOTE_CONFIGS
    withConfigFileServer [("config0", ConfigType config0)] [("config1", config1Part), ("invalid", "invalid: invalid")] $ do
#endif
        (successes, failures) ← L.partition id <$> sequence
            × tests0
            ⊕ testsConfigFile [tmpPath0, tmpPath1]
            ⊕ tests2Files1 [tmpPath0, tmpPath1]
            ⊕ tests2Files2 "local-" (tmpPath0) (tmpPath1)
            ⊕ tests2Files3 "local-" (tmpPath0) (tmpPath1)
#ifdef REMOTE_CONFIGS
            ⊕ tests2Files2 "remote-" (serverUrl ⊕ "/config0") (serverUrl ⊕ "/config1")
            ⊕ tests2Files3 "remote-" (serverUrl ⊕ "/config0") (serverUrl ⊕ "/config1")
            ⊕ testsInvalidUrl
#endif
            ⊕ routingTableTests
            ⊕ textAppendTestsR
            ⊕ textAppendTestsFilesR
            ⊕ textAppendTestsL
            ⊕ textAppendTestsFilesL
            ⊕ testPrintHelp [tmpPath0, tmpPath1]


        T.putStrLn $ "success: " ⊕ sshow (length successes)
        T.putStrLn $ "failures: " ⊕ sshow (length failures)
        unless (length failures ≡ 0) $ do
            debug $ do
                T.readFile (T.unpack tmpPath0) >>= T.putStrLn
                T.readFile (T.unpack tmpPath1) >>= T.putStrLn
            error "test suite failed"

-- -------------------------------------------------------------------------- --
-- Test Cases

-- | This always succeeds. It prints the help message for manual
-- inspection.
--
testPrintHelp ∷ [T.Text] → [IO Bool]
testPrintHelp files =
    [ runTest pkgInfo (mainInfoConfigFile configFiles) "print-help" False [trueAssertion ["-?"]]
    ]
  where
    configFiles = zipWith ($) (ConfigFileRequired : repeat ConfigFileOptional) files

testsConfigFile ∷ [T.Text] → [IO Bool]
testsConfigFile files =
    [ runTest pkgInfo (mainInfoConfigFile configFiles0) "config-file-1" True [trueAssertion []]
    , runTest pkgInfo (mainInfoConfigFile configFiles1) "config-file-2" False [trueAssertion []]
    ]
  where
    configFiles0 = zipWith ($) (ConfigFileRequired : repeat ConfigFileOptional) (files ⊕ ["./invalid"])
    configFiles1 = zipWith ($) (ConfigFileRequired : repeat ConfigFileOptional) ("./invalid":files)

#ifdef REMOTE_CONFIGS
-- | Test with invalid remote URLs
--
testsInvalidUrl ∷ [IO Bool]
testsInvalidUrl =
    [ runTest pkgInfo mainInfo "invalidUrl-0" False [x0 d1]
    , runTest pkgInfo mainInfo "invalidUrl-1" False [x1 d1]
    ]
  where
    x0 (ConfAssertion args l v) = ConfAssertion (("--config-file=http://invalid"):args) l v
    x1 (ConfAssertion args l v) = ConfAssertion (("--config-file=" ⊕ T.unpack serverUrl ⊕ "/invalid"):args) l v
#endif

-- -------------------------------------------------------------------------- --
-- Tests with two configuration files

-- | Tests with two configuration files
--
tests2Files1 ∷ [T.Text] → [IO Bool]
tests2Files1 files =
    twoFileCasesC0C1 "2files-1-" files (trueAssertion [])
    ⊕ twoFileCasesC1C0 "2files-1-" selif (trueAssertion [])
  where
    selif = reverse files

-- | Tests with two configuration files
--
tests2Files2
    ∷ T.Text
        -- ^ test label prefix
    → T.Text
        -- ^ file for config0
    → T.Text
        -- ^ file for config1
    → [IO Bool]
tests2Files2 suffix file0 file1 =
    twoFileCasesC0C1 ("2files-2-" ⊕ suffix) [file0] x1
    ⊕ twoFileCasesC1C0 ("2files-2-" ⊕ suffix) [file1] x0
  where
    x0 = trueAssertion ["--config-file=" ⊕ T.unpack file0]
    x1 = trueAssertion ["--config-file=" ⊕ T.unpack file1]

-- | Tests with two configuration files
--
tests2Files3
    ∷ T.Text
        -- ^ test label prefix
    → T.Text
        -- ^ file for config0
    → T.Text
        -- ^ file for config1
    → [IO Bool]
tests2Files3 suffix file0 file1 =
    twoFileCasesC0C1 ("2files-3-" ⊕ suffix) [] x01
    ⊕ twoFileCasesC1C0 ("2files-3-" ⊕ suffix) [] x10
  where
    x01 = trueAssertion ["--config-file=" ⊕ T.unpack file0, "--config-file=" ⊕ T.unpack file1]
    x10 = trueAssertion ["--config-file=" ⊕ T.unpack file1, "--config-file=" ⊕ T.unpack file0]

-- | Tests with two configuration files c0 then c1
--
twoFileCasesC0C1 ∷ T.Text → [T.Text] → ConfAssertion HttpURL → [IO Bool]
twoFileCasesC0C1 prefix files x =
    [ runf files (prefix ⊕ "c0c1-0") True [x, f1 c1, f2 c0, f3 c1, f4 c0]
    , runf files (prefix ⊕ "c0c1-1") False [x, f1 c0]
    , runf files (prefix ⊕ "c0c1-2") False [x, d1]
    , runf files (prefix ⊕ "c0c1-3") False [x, f3 c0]
    , runf files (prefix ⊕ "c0c1-4") False [x, d4]
    ]
  where
    c0 = config0
    c1 = config1
    runf = runTest pkgInfo ∘ mainInfoConfigFile ∘ map ConfigFileRequired

-- | Tests with two configuration files c1 then c0
--
twoFileCasesC1C0 ∷ T.Text → [T.Text] → ConfAssertion HttpURL → [IO Bool]
twoFileCasesC1C0 prefix files x =
    [ runf files (prefix ⊕ "c1c0-0") True [x, f1 c0, f2 c0, f3 c0, f4 c0]
    , runf files (prefix ⊕ "c1c0-1") False [x, f1 c1]
    , runf files (prefix ⊕ "c1c0-2") False [x, f2 c1]
    , runf files (prefix ⊕ "c1c0-3") False [x, f3 c1]
    , runf files (prefix ⊕ "c1c0-4") False [x, f4 c1]
    ]
  where
    c0 = config0
    c1 = config1
    runf = runTest pkgInfo ∘ mainInfoConfigFile ∘ map ConfigFileRequired

-- -------------------------------------------------------------------------- --
-- Command Line argument tests

-- | Command Line argument test
--
tests0 ∷ [IO Bool]
tests0 =
    [ run "test0" False [d1, d2, d3, d4]

    , run "test1" True [t0, d2, d3, d4]
    , run "test2" True [t1, d2, d3, d4]
    , run "test3" True [d1, t2, d3, d4]
    , run "test4" False [d1, d2, t3, d4]
    , run "test5" False [d1, d2, d3, t4]

    , run "test6" False [t0, t1, d2, d3, d4]
    , run "test7" True [t0, t2, d3, d4]
    , run "test8" True [t0, d2, t3, d4]
    , run "test9" True [t0, d2, d3, t4]
    , run "test10" True [t1, t2, d3, d4]
    , run "test11" True [t1, d2, t3, d4]
    , run "test12" True [t1, d2, d3, t4]
    , run "test13" True [d1, t2, t3, d4]
    , run "test14" True [d1, t2, d3, t4]
    , run "test15" False [d1, d2, t3, t4]

    , run "test16" False [t0, t1, t2, d3, d4]
    , run "test17" False [t0, t1, t3, d4]
    , run "test18" False [t0, t1, d3, t4]
    , run "test19" True [t0, t2, t3, d4]
    , run "test20" True [t0, t2, d3, t4]
    , run "test21" True [t0, d2, t3, t4]
    , run "test22" True [t1, t2, t3, d4]
    , run "test23" True [t1, t2, d3, t4]
    , run "test24" True [t1, d2, t3, t4]
    , run "test25" True [d1, t2, t3, t4]

    , run "test26" False [t0, t1, t2, t3, d4]
    , run "test27" False [t0, t1, t2, d3, t4]
    , run "test28" False [t0, t1, d2, t3, t4]
    , run "test29" True [t0, t2, t3, t4]
    , run "test30" True [t1, t2, t3, t4]

    , run "test31" False [t0, t1, t2, t3, t4]
    ]
  where
    run = runTest pkgInfo mainInfo

-- -------------------------------------------------------------------------- --
-- Test Data

-- | Test configuration 0
--
config0 ∷ HttpURL
config0 = defaultHttpURL
    { _domain = "f0_localhost"
    , _path = "f0_path"
    , _auth = defaultAuth
        { _user = "f0_user"
        , _pwd = "f0_pwd"
        }
    }

-- | Test configuration 1
--
config1 ∷ HttpURL
config1 = defaultHttpURL
    { _domain = "f1_localhost"
    , _path = "f1_path"
    , _auth = defaultAuth
        { _user = "f1_user"
        , _pwd = "f1_pwd"
        }
    }

-- | A partial version of configuration 1
--
config1Part ∷ T.Text
config1Part = T.unlines
    [ "domain: " ⊕ T.pack (view domain config1)
    , "auth:"
    , "    user: " ⊕ T.pack (view (auth ∘ user) config1)
    ]

mainInfo ∷ ProgramInfoValidate HttpURL []
mainInfo = programInfoValidate "HTTP URL" pHttpURL defaultHttpURL validateHttpURL

mainInfoConfigFile
    ∷ [ConfigFile]
    → ProgramInfoValidate HttpURL []
mainInfoConfigFile files = set piConfigurationFiles files mainInfo

-- -------------------------------------------------------------------------- --
-- Building blocks for tests

-- assert values from given configuration

f1 ∷ HttpURL → ConfAssertion HttpURL
f1 conf = ConfAssertion [] domain (view domain conf)

f2 ∷ HttpURL → ConfAssertion HttpURL
f2 conf = ConfAssertion [] path (view path conf)

f3 ∷ HttpURL → ConfAssertion HttpURL
f3 conf = ConfAssertion [] (auth ∘ user) (view (auth ∘ user) conf)

f4 ∷ HttpURL → ConfAssertion HttpURL
f4 conf = ConfAssertion [] (auth ∘ pwd) (view (auth ∘ pwd) conf)

-- assert default values

d1 ∷ ConfAssertion HttpURL
d1 = f1 defaultHttpURL

d2 ∷ ConfAssertion HttpURL
d2 = f2 defaultHttpURL

d3 ∷ ConfAssertion HttpURL
d3 = f3 defaultHttpURL

d4 ∷ ConfAssertion HttpURL
d4 = f4 defaultHttpURL

-- assert values from command line

-- t0 and t1 are the same option
t0 ∷ ConfAssertion HttpURL
t0 = ConfAssertion ["--domain=c_localhost"] domain "c_localhost"

t1 ∷ ConfAssertion HttpURL
t1 = ConfAssertion ["-d", "c_localhost"] domain "c_localhost"

t2 ∷ ConfAssertion HttpURL
t2 = ConfAssertion ["--path=c_abc"] path "c_abc"

t3 ∷ ConfAssertion HttpURL
t3 = ConfAssertion ["--user=c_u"] (auth ∘ user) "c_u"

t4 ∷ ConfAssertion HttpURL
t4 = ConfAssertion ["--pwd=c_pwd"] (auth ∘ pwd) "c_pwd"

-- -------------------------------------------------------------------------- --
-- Test Monoid Updates

-- HashMap
--
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

    fmapL f = either (Left . f) Right

mainInfoRoutingTable ∷ ProgramInfoValidate RoutingTable []
mainInfoRoutingTable = programInfoValidate "Routing Table" pRoutingTable defaultRoutingTable (const $ return ())

routingTableTests ∷ [IO Bool]
routingTableTests =
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

    at k f m = f mv <&> \r -> case r of
        Nothing -> maybe m (const (HM.delete k m)) mv
        Just v' -> HM.insert k v' m
      where
        mv = HM.lookup k m
        (<&>) = flip fmap

-- Text with right append
--
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

textAppendTestsR ∷ [IO Bool]
textAppendTestsR =
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
    mi = programInfoValidate "Routing Table" pStringConfigR defaultStringConfigR (const $ return ())

textAppendTestsFilesR ∷ [IO Bool]
textAppendTestsFilesR =
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
        runTest pkgInfo (mi [file]) ("stringL-file1-" ⊕ sshow x) b a

    run2 c0 c1 (x ∷ Int) b a =
        withConfigFile c0 $ \file0 →
        withConfigFile c1 $ \file1 →
        runTest pkgInfo (mi [file0,file1]) ("stringL-file2-" ⊕ sshow x) b a

    mi files = set piConfigurationFiles (map ConfigFileRequired files) $
      programInfoValidate "Routing Table" pStringConfigR defaultStringConfigR (const $ return ())

-- Text with left append
--
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

textAppendTestsL ∷ [IO Bool]
textAppendTestsL =
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
    mi = programInfoValidate "Routing Table" pStringConfigL defaultStringConfigL (const $ return ())

textAppendTestsFilesL ∷ [IO Bool]
textAppendTestsFilesL =
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
      programInfoValidate "Routing Table" pStringConfigL defaultStringConfigL (const $ return ())
