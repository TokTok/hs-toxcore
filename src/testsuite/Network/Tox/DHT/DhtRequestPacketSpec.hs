{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtRequestPacketSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Binary (get, put)
import qualified Data.Binary.Get               as Binary (runGet)
import qualified Data.Binary.Put               as Binary (runPut)
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.Crypto.Key        (Nonce)
import           Network.Tox.Crypto.KeyPair    (KeyPair (..))
import           Network.Tox.DHT.DhtRequestPacket     (DhtRequestPacket (..))
import qualified Network.Tox.DHT.DhtRequestPacket     as DhtRequestPacket
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)


encodeAndDecode :: (Binary a, Binary b) => KeyPair -> KeyPair -> Nonce -> a -> Maybe b
encodeAndDecode senderKeyPair addresseeKeyPair nonce payload =
  let
    KeyPair _ addresseePublicKey = addresseeKeyPair
    packet = DhtRequestPacket.encode senderKeyPair addresseePublicKey nonce payload
    packet' = Binary.runGet Binary.get $ Binary.runPut $ Binary.put packet
  in
  DhtRequestPacket.decode addresseeKeyPair packet'


encodeAndDecodeString :: KeyPair -> KeyPair -> Nonce -> String -> Maybe String
encodeAndDecodeString = encodeAndDecode


encodeCharAndDecodeString :: KeyPair -> KeyPair -> Nonce -> Char -> Maybe String
encodeCharAndDecodeString = encodeAndDecode


encodeIntAndDecodeNodeInfo :: KeyPair -> KeyPair -> Nonce -> Int -> Maybe NodeInfo
encodeIntAndDecodeNodeInfo = encodeAndDecode


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy DhtRequestPacket)
  binarySpec (Proxy :: Proxy DhtRequestPacket)
  readShowSpec (Proxy :: Proxy DhtRequestPacket)

  it "encodes and decodes packets" $
    property $ \senderKeyPair addresseeKeyPair nonce payload ->
      encodeAndDecodeString senderKeyPair addresseeKeyPair nonce payload `shouldBe` Just payload

  it "fails to decode packets with the wrong secret key" $
    property $ \senderKeyPair (KeyPair _ addresseePublicKey) badSecretKey nonce payload ->
      encodeAndDecodeString senderKeyPair (KeyPair badSecretKey addresseePublicKey) nonce payload `shouldBe` Nothing

  it "fails to decode packets with the wrong payload type (Partial)" $
    property $ \senderKeyPair addresseeKeyPair nonce payload ->
      encodeCharAndDecodeString senderKeyPair addresseeKeyPair nonce payload `shouldBe` Nothing

  it "fails to decode packets with the wrong payload type (Fail)" $
    property $ \senderKeyPair addresseeKeyPair nonce payload ->
      encodeIntAndDecodeNodeInfo senderKeyPair addresseeKeyPair nonce payload `shouldBe` Nothing

  it "should decode empty CipherText correctly" $
    expectDecoded
      [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      ] $
      DhtRequestPacket
        (read "\"0000000000000000000000000000000000000000000000000000000000000000\"")
        (read "\"0000000000000000000000000000000000000000000000000000000000000000\"")
        (read "\"000000000000000000000000000000000000000000000000\"")
        (read "\"00000000000000000000000000000000\"")
