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

#if MIN_VERSION_Cabal(2,0,0)
import PkgInfo
#else
import PkgInfo_trivial
#endif

instance FromJSON (() → ()) where
    parseJSON _ = pure id

mainInfo ∷ ProgramInfo ()
mainInfo = programInfo "Hello World" (pure id) ()

main ∷ IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"
