{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtRequestPacketSpec where

import           Test.Hspec

import           Data.Proxy                       (Proxy (..))
import           Network.Tox.DHT.DhtRequestPacket (DhtRequestPacket (..))
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy DhtRequestPacket)
  binarySpec (Proxy :: Proxy DhtRequestPacket)
  readShowSpec (Proxy :: Proxy DhtRequestPacket)
