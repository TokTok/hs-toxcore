{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.RpcPacketSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                (Proxy (..))
import           Data.Word                 (Word64)
import           Network.Tox.DHT.RpcPacket (RpcPacket)
import qualified Network.Tox.DHT.RpcPacket as RpcPacket
import           Network.Tox.EncodingSpec

spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy (RpcPacket Word64))
  binarySpec (Proxy :: Proxy (RpcPacket Word64))
  readShowSpec (Proxy :: Proxy (RpcPacket Word64))
