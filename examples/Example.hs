-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
( main
) where

import Configuration.Utils

import Data.Monoid.Unicode

import Prelude.Unicode

-- | Specification of the authentication section of a URL.
--
data Auth = Auth
    { _user ∷ !String
    , _pwd ∷ !String
    }

$(makeLenses ''Auth)

defaultAuth ∷ Auth
defaultAuth = Auth
    { _user = ""
    , _pwd = ""
    }

instance FromJSON (Auth → Auth) where
    parseJSON = withObject "Auth" $ \o → pure id
        ⊙ user ..: "user" × o
        ⊙ pwd ..: "pwd" × o

instance ToJSON Auth where
    toJSON a = object
        [ "user" .= (a ^. user)
        , "pwd" .=  (a ^. pwd)
        ]

pAuth ∷ MParser Auth
pAuth = pure id
    ⊙ user .:: strOption
        × long "user"
        ⊕ help "user name"
    ⊙ pwd .:: strOption
        × long "pwd"
        ⊕ help "password for user"

-- | Simplified specification of an HTTP URL
--
data HttpURL = HttpURL
    { _auth ∷ !Auth
    , _domain ∷ !String
    , _path ∷ !String
    }

$(makeLenses ''HttpURL)

defaultHttpURL ∷ HttpURL
defaultHttpURL = HttpURL
    { _auth = defaultAuth
    , _domain = ""
    , _path = ""
    }

instance FromJSON (HttpURL → HttpURL) where
    parseJSON = withObject "HttpURL" $ \o → pure id
        ⊙ auth %.: "auth" × o
        ⊙ domain ..: "domain" × o
        ⊙ path ..: "path" × o

instance ToJSON HttpURL where
    toJSON a = object
        [ "auth" .= (a ^. auth)
        , "domain" .= (a ^. domain)
        , "path" .= (a ^. path)
        ]

pHttpURL ∷ MParser HttpURL
pHttpURL = pure id
    ⊙ auth %:: pAuth
    ⊙ domain .:: strOption
        × long "domain"
        ⊕ short 'd'
        ⊕ help "HTTP domain"
    ⊙ path .:: strOption
        × long "path"
        ⊕ short 'p'
        ⊕ help "HTTP URL path"

-- | Information about the main Application
--
mainInfo ∷ ProgramInfo HttpURL
mainInfo = programInfo "HTTP URL" pHttpURL defaultHttpURL

main ∷ IO ()
main = runWithConfiguration mainInfo $ \conf → do
    putStrLn
        $ "http://"
        ⊕ conf ^. auth ∘ user
        ⊕ ":"
        ⊕ conf ^. auth ∘ pwd
        ⊕ "@"
        ⊕ conf ^. domain
        ⊕ "/"
        ⊕ conf ^. path
