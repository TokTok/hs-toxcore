{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.PingPacketSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                 (Proxy (..))
import           Network.Tox.DHT.PingPacket (PingPacket)
import qualified Network.Tox.DHT.PingPacket as PingPacket
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy PingPacket)
  binarySpec (Proxy :: Proxy PingPacket)
  readShowSpec (Proxy :: Proxy PingPacket)
