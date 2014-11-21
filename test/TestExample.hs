{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Tests for Example
--
module Main
( main
) where

import Control.Exception

import qualified Example

import System.Environment

main ∷ IO ()
main = do
    withArgs ["--domain=localhost"] Example.main
    withArgs ["--path=abc"] Example.main
    handle (\(_ ∷ SomeException) → return ()) $ do
        withArgs [] Example.main
        error "expected failure but got success"
    return ()

