{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014-2015 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
module Main
( main
) where

import Tests.BoolOption
import Tests.MonoidConfig
import TestTools

import Configuration.Utils
import Configuration.Utils.Internal
import Configuration.Utils.Internal.ConfigFileReader

import Control.Monad

import qualified Data.List as L
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Example hiding (main)

#if MIN_VERSION_base(4,13,0)
import Prelude.Unicode hiding ((×))
#else
import Prelude.Unicode
#endif

import PkgInfo

-- -------------------------------------------------------------------------- --
-- main

main ∷ IO ()
main = do

    -- run tests
    localResults ← sequence
        × tests0
        ⊕ monoidUpdateTests pkgInfo
        ⊕ boolOptionTests pkgInfo
    localFileResults ← localFileTests
    remoteResults ← remoteTests
    helpResults ← helpTests

    -- report results
    let (successes, failures) = L.partition id
            × localResults
            ⊕ remoteResults
            ⊕ localFileResults
            ⊕ helpResults

    T.putStrLn $ "success: " ⊕ sshow (length successes)
    T.putStrLn $ "failures: " ⊕ sshow (length failures)
    unless (length failures ≡ 0) $ do
        error "test suite failed"

-- -------------------------------------------------------------------------- --
-- Test categories

helpTests ∷ IO [Bool]
helpTests =
    withConfigFile Yaml config0 $ \tmpPath0 →
    withConfigFile Json config1Part $ \tmpPath1 → sequence
        × testPrintHelp [tmpPath0, tmpPath1]

localFileTests ∷ IO [Bool]
localFileTests = concat <$> mapM run
    [ (Yaml, Yaml, "yaml-yaml-")
    , (Json, Json, "json-json-")
    , (Yaml, Json, "yaml-json-")
    , (Json, Yaml, "json-yaml-")
    ]
  where
    run (format1, format2, label) =
        withConfigFile format1 config0 $ \tmpPath0 →
        withConfigFile format2 config1Part $ \tmpPath1 → sequence
            × testsConfigFile ("configFile-" ⊕ label) [tmpPath0, tmpPath1]
            ⊕ tests2Files1 ("local-" ⊕ label) [tmpPath0, tmpPath1]
            ⊕ tests2Files2 ("local-" ⊕ label) (tmpPath0) (tmpPath1)
            ⊕ tests2Files3 ("local-" ⊕ label) (tmpPath0) (tmpPath1)

remoteTests ∷ IO [Bool]
#ifdef REMOTE_CONFIGS
remoteTests = concat <$> mapM run
    [ (Just Yaml, "yaml")
    , (Just Json, "json")
    ]
  where
    typedConfigs = [("config0", ConfigType config0), ("config1", ConfigType config1Part)]
    textConfigs = [("invalid", "invalid: invalid")]

    run (format, label) = withConfigFileServer typedConfigs textConfigs format $
        sequence
            × tests2Files2 ("remote-" ⊕ label) (serverUrl ⊕ "/config0") (serverUrl ⊕ "/config1")
            ⊕ tests2Files3 ("remote-" ⊕ label) (serverUrl ⊕ "/config0") (serverUrl ⊕ "/config1")
            ⊕ testsInvalidUrl
            ⊕ testsTlsUrl
#else
remoteTests = return []
#endif

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

testsConfigFile ∷ T.Text → [T.Text] → [IO Bool]
testsConfigFile prefix files =
    [ runTest pkgInfo (mainInfoConfigFile configFiles0) (prefix ⊕ "-1") True [trueAssertion []]
    , runTest pkgInfo (mainInfoConfigFile configFiles1) (prefix ⊕ "-2") False [trueAssertion []]
    ]
  where
    configFiles0 = zipWith ($) (ConfigFileRequired : repeat ConfigFileOptional) (files ⊕ ["./invalid"])
    configFiles1 = zipWith ($) (ConfigFileRequired : repeat ConfigFileOptional) ("./invalid":files)

#ifdef REMOTE_CONFIGS
-- | Test with invalid remote URLs
--
testsInvalidUrl ∷ [IO Bool]
testsInvalidUrl =
    [ runTest pkgInfo mainInfo "invalidUrl-0" False [x0, d1]
    , runTest pkgInfo mainInfo "invalidUrl-1" False [x1, d1]
    ]
  where
    x0 = trueAssertion ["--config-file=http://invalid"]
    x1 = trueAssertion ["--config-file=" ⊕ T.unpack serverUrl ⊕ "/invalid"]

testsTlsUrl ∷ [IO Bool]
testsTlsUrl =
    [ runTest pkgInfo mainInfo "tlsUrl-0" True [cf0, f1 c0]
    , runTest pkgInfo mainInfo "tlsUrl-1" False [cf0t, f1 c0]
    , runTest pkgInfo mainInfo "tlsUrl-2" False [cf0tl, f1 c0]
    , runTest pkgInfo mainInfo "tlsUrl-3" False [cf0t, f1 c0, fingerF]
    , runTest pkgInfo mainInfo "tlsUrl-4" True [cf0t, f1 c0, fingerT]
    , runTest pkgInfo mainInfo "tlsUrl-5" True [insec, cf0, f1 c0]
    , runTest pkgInfo mainInfo "tlsUrl-6" True [insec, cf0t, f1 c0]
    ]
  where
    cf0 = trueAssertion ["--config-file=" ⊕ T.unpack serverUrl ⊕ "/config0"]
    cf0t = trueAssertion ["--config-file=" ⊕ T.unpack serverTlsUrl ⊕ "/config0"]
    cf0tl = trueAssertion ["--config-file=" ⊕ "https://localhost:8284" ⊕ "/config0"] -- FIXME don't hardcode this
    insec = trueAssertion ["--config-https-insecure"]
    fingerF = trueAssertion ["--config-https-allow-cert=" ⊕ drop 8 (T.unpack serverTlsUrl) ⊕ ":0x+SV6/D6JSIKK8pPCpaMZvMXelXb2CnJ8xWo8qi4Fo="]
    fingerT = trueAssertion ["--config-https-allow-cert=" ⊕ drop 8 (T.unpack serverTlsUrl) ⊕ ":HK4/ZeG/3c+H5R/3eTlysmJxmrBil6w8oLdvOdHFlsg="]
    c0 = config0
#endif

-- -------------------------------------------------------------------------- --
-- Tests with two configuration files

-- | Tests with two configuration files
--
tests2Files1 ∷ T.Text → [T.Text] → [IO Bool]
tests2Files1 prefix files =
    twoFileCasesC0C1 (prefix ⊕ "2files-1-") files (trueAssertion [])
    ⊕ twoFileCasesC1C0 (prefix ⊕ "2files-1-") selif (trueAssertion [])
  where
    selif = reverse files

-- | Tests with two configuration files
--
tests2Files2
    ∷ T.Text
        -- ^ test label suffix
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
        -- ^ test label suffix
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
config1Part ∷ Value
config1Part = object
    [ "domain" .= view domain config1
    , "auth" .= object
        [ "user" .= view (auth ∘ user) config1
        ]
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
