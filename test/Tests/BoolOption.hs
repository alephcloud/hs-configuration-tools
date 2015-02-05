{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Tests.BoolOption
-- Description: Tests for 'boolOption' and 'boolOption_'
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- load in ghci with:
--
-- > ghci -isrc -idist/build/autogen -itest -iexamples -DREMOTE_CONFIGS test/TestExample.hs
--
module Tests.BoolOption
( mainA
, boolOptionTests
) where

import Configuration.Utils
import Configuration.Utils.Internal
import TestTools

import Data.Monoid.Unicode

-- -------------------------------------------------------------------------- --
-- Setup

data A = A
    { _a ∷ !Bool
    , _b ∷ !Bool
    , _c ∷ !Bool
    }
    deriving (Show, Read, Eq, Ord)

a ∷ Lens' A Bool
a = lens _a $ \s x → s { _a = x }

b ∷ Lens' A Bool
b = lens _b $ \s x → s { _b = x }

c ∷ Lens' A Bool
c = lens _c $ \s x → s { _c = x }

defaultA ∷ A
defaultA = A True True True

pA ∷ MParser A
pA = id
    <$< a .:: boolOption_
        × long "a"
        ⊕ short 'a'
        ⊕ help "a flag"
    <*< b .:: boolOption
        × long "b"
        ⊕ short 'b'
        ⊕ help "b flag"
    <*< c .:: enableDisableFlag
        × long "c"
        ⊕ long "c_"
        ⊕ short 'd' -- 'c' is taken by --config-file
        ⊕ help "c flag"

instance ToJSON A where
    toJSON A{..} = object
        [ "a" .= _a
        , "b" .= _b
        , "c" .= _c
        ]

instance FromJSON (A → A) where
    parseJSON = withObject "A" $ \o → id
        <$< a ..: "a" % o
        <*< b ..: "b" % o
        <*< c ..: "c" % o

infoA ∷ ProgramInfo A
infoA = programInfo "BoolOptionTest" pA (A True True True)

infoA_ ∷ ProgramInfo A
infoA_ = programInfo "BoolOptionTest" pA (A False False False)

mainA ∷ IO ()
mainA = runWithConfiguration infoA print

-- -------------------------------------------------------------------------- --
-- Tests

da ∷ ConfAssertion A
da = ConfAssertion [] a $ _a defaultA

db ∷ ConfAssertion A
db = ConfAssertion [] b $ _b defaultA

dc ∷ ConfAssertion A
dc = ConfAssertion [] c $ _c defaultA

boolOptionTests ∷ PkgInfo → [IO Bool]
boolOptionTests pkgInfo = atests ⊕ btests ⊕ ctests ⊕ ctests_
  where
    atests =
        [ runA 1 True [da, db, dc]
        , runA 2 True [ConfAssertion ["--a"] a True]
        , runA 3 True [ConfAssertion ["-a"] a True]
        , runA 4 False [ConfAssertion ["-no-a"] a True]

        , runA 5 False [ConfAssertion ["--a=true"] a True]
        , runA 6 False [ConfAssertion ["--a=false"] a False]
        , runA 7 False [ConfAssertion ["--a", "true"] a True]
        , runA 8 False [ConfAssertion ["--a", "false"] a False]

        , runA 9 True [ConfAssertion ["--no-a"] a False]
        , runA 10 False [ConfAssertion ["--no-a=true"] a True]
        , runA 11 False [ConfAssertion ["--no-a=false"] a False]
        , runA 12 False [ConfAssertion ["--no-a", "true"] a True]
        , runA 13 False [ConfAssertion ["--no-a", "false"] a False]

        , runA 14 True [ConfAssertion ["-a"] a True]
        , runA 15 False [ConfAssertion ["-a=true"] a True]
        , runA 16 False [ConfAssertion ["-a=false"] a False]
        , runA 17 False [ConfAssertion ["-a", "true"] a True]
        , runA 18 False [ConfAssertion ["-a", "false"] a False]
        ]

    btests =
        [ runB 1 False [ConfAssertion ["--b"] b True]
        , runB 2 False [ConfAssertion ["-b"] b True]
        , runB 3 False [ConfAssertion ["-no-b"] b True]

        , runB 4 True [ConfAssertion ["--b=true"] b True]
        , runB 5 True [ConfAssertion ["--b=false"] b False]
        , runB 6 True [ConfAssertion ["--b", "true"] b True]
        , runB 7 True [ConfAssertion ["--b", "false"] b False]

        , runB 8 False [ConfAssertion ["-b=true"] b True]
        , runB 9 False [ConfAssertion ["-b=false"] b False]
        , runB 10 True [ConfAssertion ["-b", "true"] b True]
        , runB 12 True [ConfAssertion ["-b", "false"] b False]

        , runB 13 True [ConfAssertion ["--b=TRUE"] b True]
        , runB 14 True [ConfAssertion ["--b=FALSE"] b False]
        , runB 15 True [ConfAssertion ["--b", "TRUE"] b True]
        , runB 16 True [ConfAssertion ["--b", "FALSE"] b False]

        , runB 17 True [ConfAssertion ["--b=True"] b True]
        , runB 18 True [ConfAssertion ["--b=False"] b False]
        , runB 19 True [ConfAssertion ["--b", "True"] b True]
        , runB 20 True [ConfAssertion ["--b", "False"] b False]
        ]

    ctests =
        [ runC 1 True [da, db, dc]
        , runC 2 False [ConfAssertion ["--c"] c True]
        , runC 3 False [ConfAssertion ["--c_"] c True]
        , runC 4 False [ConfAssertion ["--c"] c False]
        , runC 5 False [ConfAssertion ["--c_"] c False]
        , runC 6 True [ConfAssertion ["--enable-c"] c True]
        , runC 7 True [ConfAssertion ["--enable-c_"] c True]
        , runC 8 True [ConfAssertion ["-d"] c True]
        , runC 9 False [ConfAssertion ["-disable-c"] c False]
        , runC 10 False [ConfAssertion ["-disable-c"] c True]
        , runC 9 False [ConfAssertion ["-disable-d"] c False]
        , runC 10 False [ConfAssertion ["-disable-d"] c True]
        , runC 10 False [ConfAssertion ["--disable-d"] c False]
        , runC 11 True [ConfAssertion ["--disable-c"] c False]
        , runC 12 True [ConfAssertion ["--disable-c_"] c False]
        ]

    ctests_ =
        [ runC_ 1 False [da, db, dc]
        , runC_ 2 False [ConfAssertion ["--c"] c True]
        , runC_ 3 False [ConfAssertion ["--c_"] c True]
        , runC_ 4 False [ConfAssertion ["--c"] c False]
        , runC_ 5 False [ConfAssertion ["--c_"] c False]
        , runC_ 6 True [ConfAssertion ["--enable-c"] c True]
        , runC_ 7 True [ConfAssertion ["--enable-c_"] c True]
        , runC_ 8 True [ConfAssertion ["-d"] c True]
        , runC_ 9 False [ConfAssertion ["-disable-c"] c False]
        , runC_ 10 False [ConfAssertion ["-disable-c"] c True]
        , runC_ 11 True [ConfAssertion ["--disable-c"] c False]
        , runC_ 12 True [ConfAssertion ["--disable-c_"] c False]
        ]

    runA (x ∷ Int) = runTest pkgInfo infoA ("boolOption-a-" ⊕ sshow x)
    runB (x ∷ Int) = runTest pkgInfo infoA ("boolOption-b-" ⊕ sshow x)

    runC (x ∷ Int) = runTest pkgInfo infoA ("boolOption-c1-" ⊕ sshow x)
    runC_ (x ∷ Int) = runTest pkgInfo infoA_ ("boolOption-c2-" ⊕ sshow x)

