{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Protocol.PacketSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                  (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.Protocol.Packet (Packet)
import qualified Network.Tox.Protocol.Packet as Packet


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy (Packet Int))
  binarySpec (Proxy :: Proxy (Packet Int))
  readShowSpec (Proxy :: Proxy (Packet Int))
