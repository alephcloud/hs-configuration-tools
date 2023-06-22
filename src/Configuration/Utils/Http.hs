-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Configuration.Utils.Http
(
-- * HTTP Service TLS Configuration
  HttpServiceTLSConfiguration
, hstcCertFile
, hstcKeyFile
, defaultHttpServiceTLSConfiguration
, pHttpServiceTLSConfiguration
, validateHttpServiceTLSConfiguration

-- * HTTP Service Configuration
, HttpServiceConfiguration
, hscHost
, hscPort
, hscUseTLS
, defaultHttpServiceConfiguration
, pHttpServiceConfiguration
, validateHttpServiceConfiguration

-- * Http Client Configuration
, HttpClientConfiguration
, hccHost
, hccPort
, hccUseTLS
, defaultHttpClientConfiguration
, pHttpClientConfiguration
, validateHttpClientConfiguration
, httpService2clientConfiguration
) where

import Configuration.Utils
import Configuration.Utils.Internal
import Configuration.Utils.Validation

import Control.Monad (when)
import Control.Monad.Writer.Class (tell)

import qualified Data.ByteString.Char8 as B8
import qualified Data.DList as DL
import Data.Maybe (isJust)
import Data.Monoid.Unicode

import Prelude.Unicode hiding ((×))

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

validateHttpServiceTLSConfiguration
    ∷ ConfigValidation HttpServiceTLSConfiguration f
validateHttpServiceTLSConfiguration conf = do
    validateFileReadable "cert-file" $ _hstcCertFile conf
    validateFileReadable "key-file" $ _hstcKeyFile conf

instance FromJSON (HttpServiceTLSConfiguration → HttpServiceTLSConfiguration) where
    parseJSON = withObject "HttpServiceTLSConfiguration" $ \o → id
        <$< hstcCertFile ..: "cert-file" % o
        <*< hstcKeyFile ..: "pem-file" % o

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
pHttpServiceTLSConfiguration prefix = id
    <$< hstcCertFile .:: strOption
        % long (prefix ⊕ "cert-file")
        ⊕ help "File with PEM encoded TLS Certificate"
    <*< hstcKeyFile .:: strOption
        % long (prefix ⊕ "key-file")
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

validateHttpServiceConfiguration ∷ ConfigValidation HttpServiceConfiguration DL.DList
validateHttpServiceConfiguration conf = do
    maybe (return ()) validateHttpServiceTLSConfiguration $ _hscUseTLS conf
    validatePort "port" $ _hscPort conf
    when (_hscPort conf < 1024) $
        tell ["listening on a priviledged port requires super user rights"]
    validateNonEmpty "host" $ _hscHost conf
    validateIPv4 "interface" . B8.unpack $ _hscInterface conf

instance FromJSON (HttpServiceConfiguration → HttpServiceConfiguration) where
    parseJSON = withObject "HttpServiceConfiguration" $ \o → id
        <$< hscHost ∘ bs ..: "host" % o
        <*< hscPort ..: "port" % o
        <*< hscInterface ∘ bs ..: "interface" % o
        <*< hscUseTLS %.: "use-tls" % o
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
pHttpServiceConfiguration prefix = id
    <$< hscHost ∘ bs .:: strOption
        % long (prefix ⊕ "host")
        ⊕ help "Hostname of the service"
    <*< hscPort .:: option auto
        % long (prefix ⊕ "port")
        ⊕ help "Port of the service"
    <*< hscInterface ∘ bs .:: option auto
        % long (prefix ⊕ "interface")
        ⊕ help "Interface of the service"
    <*< (hscUseTLS %:: (fmap <$> pHttpServiceTLSConfiguration prefix))
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

validateHttpClientConfiguration ∷ ConfigValidation HttpClientConfiguration f
validateHttpClientConfiguration conf = do
    validatePort "port" $ _hccPort conf
    validateNonEmpty "host" $ _hccHost conf

instance FromJSON (HttpClientConfiguration → HttpClientConfiguration) where
    parseJSON = withObject "HttpClientConfiguration" $ \o → id
        <$< hccHost ∘ bs ..: "host" % o
        <*< hccPort ..: "port" % o
        <*< hccUseTLS ..: "use-tls" % o
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
pHttpClientConfiguration serviceName = id
    <$< hccHost ∘ bs .:: strOption
        % long (serviceName ⊕ "-host")
        ⊕ help ("Hostname of " ⊕ serviceName)
    <*< hccPort .:: option auto
        % long (serviceName ⊕ "-port")
        ⊕ help ("Port of " ⊕ serviceName)
    <*< hccUseTLS .:: switch
        % long (serviceName ⊕ "-use-tls")
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
