{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.RpcPacketSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                (Proxy (..))
import           Network.Tox.DHT.RpcPacket (RpcPacket)
import qualified Network.Tox.DHT.RpcPacket as RpcPacket
import           Network.Tox.EncodingSpec

spec :: Spec
spec = do
  jsonSpec (Proxy :: Proxy (RpcPacket Int))
  binarySpec (Proxy :: Proxy (RpcPacket Int))
  readShowSpec (Proxy :: Proxy (RpcPacket Int))
