{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.TransportProtocolSpec where

import           Test.Hspec

import           Data.Proxy                             (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.TransportProtocol (TransportProtocol)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy TransportProtocol)
  binarySpec (Proxy :: Proxy TransportProtocol)
  readShowSpec (Proxy :: Proxy TransportProtocol)
  bitEncodingSpec (Proxy :: Proxy TransportProtocol)
