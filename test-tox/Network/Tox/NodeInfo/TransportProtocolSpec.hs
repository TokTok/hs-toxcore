{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.TransportProtocolSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                             (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.TransportProtocol (TransportProtocol)
import qualified Network.Tox.NodeInfo.TransportProtocol as TransportProtocol


spec :: Spec
spec = do
  jsonSpec (Proxy :: Proxy TransportProtocol)
  binarySpec (Proxy :: Proxy TransportProtocol)
  readShowSpec (Proxy :: Proxy TransportProtocol)

  --bitEncodingSpec (Proxy :: Proxy TransportProtocol)
