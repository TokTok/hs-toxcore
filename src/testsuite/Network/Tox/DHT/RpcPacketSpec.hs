{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.RpcPacketSpec where

import           Test.Hspec

import           Data.Proxy                (Proxy (..))
import           Data.Word                 (Word64)
import           Network.Tox.DHT.RpcPacket (RequestId (..), RpcPacket (..))
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy (RpcPacket Word64))
  binarySpec (Proxy :: Proxy (RpcPacket Word64))
  readShowSpec (Proxy :: Proxy (RpcPacket Word64))

  it "has a payload and a request ID" $ do
    let packet = RpcPacket ["heyo"] (RequestId 0x12345678)
    rpcPayload packet `shouldBe` ["heyo"]
    requestId packet `shouldBe` RequestId 0x12345678
