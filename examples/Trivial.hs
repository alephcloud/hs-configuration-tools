-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main ( main ) where

import Configuration.Utils

import PkgInfo

instance FromJSON (() → ()) where
    parseJSON _ = pure id

mainInfo ∷ ProgramInfo ()
mainInfo = programInfo "Hello World" (pure id) ()

main ∷ IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"
