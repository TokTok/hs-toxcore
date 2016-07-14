{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtPacketSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Binary (get, put)
import qualified Data.Binary.Get               as Binary (runGet)
import qualified Data.Binary.Put               as Binary (runPut)
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.Crypto.Key        (Nonce)
import           Network.Tox.Crypto.KeyPair    (KeyPair (..))
import           Network.Tox.DHT.DhtPacket     (DhtPacket (..))
import qualified Network.Tox.DHT.DhtPacket     as DhtPacket
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)


encodeAndDecode :: (Binary a, Binary b) => KeyPair -> KeyPair -> Nonce -> a -> Maybe b
encodeAndDecode senderKeyPair receiverKeyPair nonce payload =
  let
    KeyPair _ receiverPublicKey = receiverKeyPair
    packet = DhtPacket.encode senderKeyPair receiverPublicKey nonce payload
    packet' = Binary.runGet Binary.get $ Binary.runPut $ Binary.put packet
  in
  DhtPacket.decode receiverKeyPair packet'


encodeAndDecodeString :: KeyPair -> KeyPair -> Nonce -> String -> Maybe String
encodeAndDecodeString = encodeAndDecode


encodeCharAndDecodeString :: KeyPair -> KeyPair -> Nonce -> Char -> Maybe String
encodeCharAndDecodeString = encodeAndDecode


encodeIntAndDecodeNodeInfo :: KeyPair -> KeyPair -> Nonce -> Int -> Maybe NodeInfo
encodeIntAndDecodeNodeInfo = encodeAndDecode


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy DhtPacket)
  binarySpec (Proxy :: Proxy DhtPacket)
  readShowSpec (Proxy :: Proxy DhtPacket)

  it "encodes and decodes packets" $
    property $ \senderKeyPair receiverKeyPair nonce payload ->
      encodeAndDecodeString senderKeyPair receiverKeyPair nonce payload `shouldBe` Just payload

  it "fails to decode packets with the wrong secret key" $
    property $ \senderKeyPair (KeyPair _ receiverPublicKey) badSecretKey nonce payload ->
      encodeAndDecodeString senderKeyPair (KeyPair badSecretKey receiverPublicKey) nonce payload `shouldBe` Nothing

  it "fails to decode packets with the wrong payload type (Partial)" $
    property $ \senderKeyPair receiverKeyPair nonce payload ->
      encodeCharAndDecodeString senderKeyPair receiverKeyPair nonce payload `shouldBe` Nothing

  it "fails to decode packets with the wrong payload type (Fail)" $
    property $ \senderKeyPair receiverKeyPair nonce payload ->
      encodeIntAndDecodeNodeInfo senderKeyPair receiverKeyPair nonce payload `shouldBe` Nothing

  it "should decode empty CipherText correctly" $
    expectDecoded
      [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      ] $
      DhtPacket
        (read "\"0000000000000000000000000000000000000000000000000000000000000000\"")
        (read "\"000000000000000000000000000000000000000000000000\"")
        (read "\"\"")
