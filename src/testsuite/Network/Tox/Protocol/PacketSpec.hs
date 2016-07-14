{-# LANGUAGE Trustworthy #-}
module Network.Tox.Protocol.PacketSpec where

import           Test.Hspec

import           Data.Proxy                  (Proxy (..))
import           Data.Word                   (Word64)
import           Network.Tox.EncodingSpec
import           Network.Tox.Protocol.Packet (Packet)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy (Packet Word64))
  binarySpec (Proxy :: Proxy (Packet Word64))
  readShowSpec (Proxy :: Proxy (Packet Word64))
