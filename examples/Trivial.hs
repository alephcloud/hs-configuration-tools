-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
( main
) where

import Configuration.Utils
import PkgInfo_trivial

instance FromJSON (() → ()) where
    parseJSON _ = pure id

mainInfo ∷ ProgramInfo ()
mainInfo = programInfo "Hello World" (pure id) ()

main ∷ IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"

