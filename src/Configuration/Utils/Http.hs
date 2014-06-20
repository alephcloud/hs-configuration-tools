-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Configuration.Utils.Http
(
-- * HTTP Service TLS Configuration
  HttpServiceTLSConfiguration
, hstcCertFile
, hstcKeyFile

-- * HTTP Service Configuration
, HttpServiceConfiguration
, hscHost
, hscPort
, hscUseTLS
, defaultHttpServiceConfiguration
, pHttpServiceConfiguration

-- * Http Client Configuration
, HttpClientConfiguration
, hccHost
, hccPort
, hccUseTLS
, defaultHttpClientConfiguration
, pHttpClientConfiguration
, httpService2clientConfiguration
) where

import Configuration.Utils
import Configuration.Utils.Internal

import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isJust)
import Data.Monoid.Unicode

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Http Service TLS Configuration

-- | In order to make TLS optional this type should be used
-- wrapped into a Maybe.
--
data HttpServiceTLSConfiguration = HttpServiceTLSConfiguration
    { _hstcCertFile ∷ !FilePath
    , _hstcKeyFile ∷ !FilePath
    }
    deriving (Show, Read, Eq, Ord)

hstcCertFile ∷ Lens' HttpServiceTLSConfiguration FilePath
hstcCertFile = lens _hstcCertFile $ \s a → s { _hstcCertFile = a}

hstcKeyFile ∷ Lens' HttpServiceTLSConfiguration FilePath
hstcKeyFile = lens _hstcKeyFile $ \s a → s { _hstcKeyFile = a}

defaultHttpServiceTLSConfiguration ∷ HttpServiceTLSConfiguration
defaultHttpServiceTLSConfiguration = HttpServiceTLSConfiguration
    { _hstcCertFile = "cert.pem"
    , _hstcKeyFile = "key.pem"
    }

instance FromJSON (HttpServiceTLSConfiguration → HttpServiceTLSConfiguration) where
    parseJSON = withObject "HttpServiceTLSConfiguration" $ \o → pure id
        ⊙ hstcCertFile ..: "cert-file" × o
        ⊙ hstcKeyFile ..: "pem-file" × o

-- | This is used as default when wrapped into Maybe and
--
-- 1. the parsed value is not 'Null' and
-- 2. the given default is not 'Nothing'.
--
instance FromJSON HttpServiceTLSConfiguration where
    parseJSON v = parseJSON v <*> pure defaultHttpServiceTLSConfiguration

instance ToJSON HttpServiceTLSConfiguration where
    toJSON HttpServiceTLSConfiguration{..} = object
        [ "cert-file" .= _hstcCertFile
        , "key-file" .= _hstcKeyFile
        ]

-- | This option parser does not allow to enable or disable
-- usage of TLS. The option will have effect only when TLS
-- usage is configured in the configuration file or the default
-- configuration.
--
-- FIXME: print a warning and exit when one of these options is
-- provided even though TLS is turned off.
--
pHttpServiceTLSConfiguration ∷ String → MParser HttpServiceTLSConfiguration
pHttpServiceTLSConfiguration prefix = pure id
    ⊙ hstcCertFile .:: strOption
        × long (prefix ⊕ "cert-file")
        ⊕ help "File with PEM encoded TLS Certificate"
    ⊙ hstcKeyFile .:: strOption
        × long (prefix ⊕ "key-file")
        ⊕ help "File with PEM encoded TLS key"

-- -------------------------------------------------------------------------- --
-- Http Service Configuration

-- | We restrict services to use either HTTP or HTTPS but not both.
--
-- TLS can be turned off explicitely in the configuration file by
-- setting the respective section to @null@. It can not be
-- turned on or off via command line options. But once it is turned
-- on the values for the certificate and key file can be changed
-- by command line options.
--
data HttpServiceConfiguration = HttpServiceConfiguration
    { _hscHost ∷ !B8.ByteString
    , _hscPort ∷ !Int
    , _hscInterface ∷ !B8.ByteString
    , _hscUseTLS ∷ !(Maybe HttpServiceTLSConfiguration)
    }
    deriving (Show, Read, Eq, Ord)

hscHost ∷ Lens' HttpServiceConfiguration B8.ByteString
hscHost = lens _hscHost $ \s a → s { _hscHost = a}

hscPort ∷ Lens' HttpServiceConfiguration Int
hscPort = lens _hscPort $ \s a → s { _hscPort = a}

hscInterface ∷ Lens' HttpServiceConfiguration B8.ByteString
hscInterface = lens _hscInterface $ \s a → s { _hscInterface = a}

hscUseTLS ∷ Lens' HttpServiceConfiguration (Maybe HttpServiceTLSConfiguration)
hscUseTLS = lens _hscUseTLS $ \s a → s { _hscUseTLS = a}


defaultHttpServiceConfiguration ∷ HttpServiceConfiguration
defaultHttpServiceConfiguration = HttpServiceConfiguration
    { _hscHost = "localhost"
    , _hscPort = 80
    , _hscInterface = "0.0.0.0"
    , _hscUseTLS = Nothing
    }

instance FromJSON (HttpServiceConfiguration → HttpServiceConfiguration) where
    parseJSON = withObject "HttpServiceConfiguration" $ \o → pure id
        ⊙ hscHost ∘ bs ..: "host" × o
        ⊙ hscPort ..: "port" × o
        ⊙ hscInterface ∘ bs ..: "interface" × o
        ⊙ hscUseTLS %.: "use-tls" × o
      where
        bs ∷ Iso' B8.ByteString String
        bs = iso B8.unpack B8.pack

instance ToJSON HttpServiceConfiguration where
    toJSON HttpServiceConfiguration{..} = object
        [ "host" .= B8.unpack _hscHost
        , "port" .= _hscPort
        , "interface" .= B8.unpack _hscInterface
        , "use-tls" .= _hscUseTLS
        ]

pHttpServiceConfiguration ∷ String → MParser HttpServiceConfiguration
pHttpServiceConfiguration serviceName = pure id
    ⊙ hscHost ∘ bs .:: strOption
        × long (serviceName ⊕ "-host")
        ⊕ help ("Hostname of " ⊕ serviceName)
    ⊙ hscPort .:: option
        × long (serviceName ⊕ "-port")
        ⊕ help ("Port of " ⊕ serviceName)
    ⊙ hscInterface ∘ bs .:: option
        × long (serviceName ⊕ "-interface")
        ⊕ help ("Port of " ⊕ serviceName)
    ⊙ (hscUseTLS %:: (fmap <$> pHttpServiceTLSConfiguration (serviceName ⊕ "-")))
  where
    bs ∷ Iso' B8.ByteString String
    bs = iso B8.unpack B8.pack

-- -------------------------------------------------------------------------- --
-- Http Client Configuration

data HttpClientConfiguration = HttpClientConfiguration
    { _hccHost ∷ !B8.ByteString
    , _hccPort ∷ !Int
    , _hccUseTLS ∷ !Bool
    }
    deriving (Show, Read, Eq, Ord)

hccHost ∷ Lens' HttpClientConfiguration B8.ByteString
hccHost = lens _hccHost $ \s a → s { _hccHost = a}

hccPort ∷ Lens' HttpClientConfiguration Int
hccPort = lens _hccPort $ \s a → s { _hccPort = a}

hccUseTLS ∷ Lens' HttpClientConfiguration Bool
hccUseTLS = lens _hccUseTLS $ \s a → s { _hccUseTLS = a}

defaultHttpClientConfiguration ∷ HttpClientConfiguration
defaultHttpClientConfiguration = HttpClientConfiguration
    { _hccHost = "localhost"
    , _hccPort = 80
    , _hccUseTLS = False
    }

instance FromJSON (HttpClientConfiguration → HttpClientConfiguration) where
    parseJSON = withObject "HttpClientConfiguration" $ \o → pure id
        ⊙ hccHost ∘ bs ..: "host" × o
        ⊙ hccPort ..: "port" × o
        ⊙ hccUseTLS ..: "use-tls" × o
      where
        bs ∷ Iso' B8.ByteString String
        bs = iso B8.unpack B8.pack

instance ToJSON HttpClientConfiguration where
    toJSON HttpClientConfiguration{..} = object
        [ "host" .= B8.unpack _hccHost
        , "port" .= _hccPort
        , "use-tls" .= _hccUseTLS
        ]

pHttpClientConfiguration ∷ String → MParser HttpClientConfiguration
pHttpClientConfiguration serviceName = pure id
    ⊙ hccHost ∘ bs .:: strOption
        × long (serviceName ⊕ "-host")
        ⊕ help ("Hostname of " ⊕ serviceName)
    ⊙ hccPort .:: option
        × long (serviceName ⊕ "-port")
        ⊕ help ("Port of " ⊕ serviceName)
    ⊙ hccUseTLS .:: switch
        × long (serviceName ⊕ "-use-tls")
        ⊕ help ("Connect to " ⊕ serviceName ⊕ " via TLS")
  where
    bs ∷ Iso' B8.ByteString String
    bs = iso B8.unpack B8.pack

httpService2clientConfiguration ∷ HttpServiceConfiguration → HttpClientConfiguration
httpService2clientConfiguration HttpServiceConfiguration{..} = HttpClientConfiguration
    { _hccHost = _hscHost
    , _hccPort = _hscPort
    , _hccUseTLS = isJust _hscUseTLS
    }

