{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import Configuration.Utils
import Configuration.Utils.Internal

import Control.Exception
import Control.Monad

import Data.IORef
import qualified Data.List as L
import Data.Monoid.Unicode
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Example hiding (main)

import Prelude.Unicode

import System.Environment

import PkgInfo_url_example_test

-- -------------------------------------------------------------------------- --
-- main

main ∷ IO ()
main = do
    (successes, failures) ← L.partition id <$> (sequence
        [ run "test0" False []

        , run "test1" True [t0]
        , run "test2" True [t1]
        , run "test3" True [t2]
        , run "test4" False [t3]
        , run "test5" False [t4]

        , run "test6" False [t0, t1]
        , run "test7" True [t0, t2]
        , run "test8" True [t0, t3]
        , run "test9" True [t0, t4]
        , run "test10" True [t1, t2]
        , run "test11" True [t1, t3]
        , run "test12" True [t1, t4]
        , run "test13" True [t2, t3]
        , run "test14" True [t2, t4]
        , run "test15" False [t3, t4]

        , run "test16" False [t0, t1, t2]
        , run "test17" False [t0, t1, t3]
        , run "test18" False [t0, t1, t4]
        , run "test19" True [t0, t2, t3]
        , run "test20" True [t0, t2, t4]
        , run "test21" True [t0, t3, t4]
        , run "test22" True [t1, t2, t3]
        , run "test23" True [t1, t2, t4]
        , run "test24" True [t1, t3, t4]
        , run "test25" True [t2, t3, t4]

        , run "test26" False [t0, t1, t2, t3]
        , run "test27" False [t0, t1, t2, t4]
        , run "test28" False [t0, t1, t3, t4]
        , run "test29" True [t0, t2, t3, t4]
        , run "test30" True [t1, t2, t3, t4]

        , run "test31" False [t0, t1, t2, t3, t4]
        ] ∷ IO [Bool])

    T.putStrLn $ "success: " ⊕ sshow (length successes)
    T.putStrLn $ "failures: " ⊕ sshow (length failures)
    unless (length failures ≡ 0) $ error "test suite failed"
  where
    run = runTest mainInfo

mainInfo ∷ ProgramInfoValidate HttpURL []
mainInfo = programInfoValidate "HTTP URL" pHttpURL defaultHttpURL validateHttpURL

-- -------------------------------------------------------------------------- --
-- Test Vectors

data ConfAssertion β = ∀ α . Eq α ⇒ ConfAssertion [String] (Lens' β α) α

t0 ∷ ConfAssertion HttpURL
t0 = ConfAssertion ["--domain=localhost"] domain "localhost"

t1 ∷ ConfAssertion HttpURL
t1 = ConfAssertion ["-d", "localhost"] domain "localhost"

t2 ∷ ConfAssertion HttpURL
t2 = ConfAssertion ["--path=abc"] path "abc"

t3 ∷ ConfAssertion HttpURL
t3 = ConfAssertion ["--user=u"] (auth ∘ user) "u"

t4 ∷ ConfAssertion HttpURL
t4 = ConfAssertion ["--pwd=pwd"] (auth ∘ pwd) "pwd"

-- -------------------------------------------------------------------------- --
-- Test execution

check
    ∷ α
    → [ConfAssertion α]
    → Bool
check conf = all (\(ConfAssertion _ l v) → view l conf ≡ v)

runTest
    ∷ (FromJSON (α → α), ToJSON α)
    ⇒ ProgramInfoValidate α []
    → T.Text
        -- ^ label for the test case
    → Bool
        -- ^ expected outcome
    → [ConfAssertion α]
        -- ^ test assertions
    → IO Bool
runTest mInfo label succeed assertions = do
    a ← run $ runWithPkgInfoConfiguration mInfo pkgInfo
    b ← run $ runWithConfiguration mInfo
    if a ≡ b && succeed ≡ (a && b)
      then
        return True
      else do
        T.putStrLn $ "test " ⊕ label ⊕ " failed"
        return False
  where
    run f = do
        ref ← newIORef False
        handle (\(_ ∷ SomeException) → writeIORef ref False) $ withArgs args ∘ f $ \conf →
            writeIORef ref $ check conf assertions
        readIORef ref

    args = concatMap (\(ConfAssertion x _ _) → x) assertions

