{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.Crypto.BoxSpec where

import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString                as ByteString
import qualified Data.MessagePack.Result        as R
import           Data.Proxy                     (Proxy (..))
import           Network.MessagePack.Rpc        (rpc)
import           Network.Tox.RPCTest            (equiv3, equivProp, equivProp3,
                                                 runTest)
import           Test.Hspec
import           Test.QuickCheck

import           Network.Tox.Crypto.Box         (CipherText, PlainText (..))
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  describe "Text" $ do
    rpcSpec (Proxy :: Proxy CipherText)
    rpcSpec (Proxy :: Proxy PlainText)
    binarySpec (Proxy :: Proxy CipherText)
    binarySpec (Proxy :: Proxy PlainText)
    readShowSpec (Proxy :: Proxy CipherText)
    readShowSpec (Proxy :: Proxy PlainText)

    it "encodes/decodes arbitrary texts" $
      property $ \(bytes :: String) ->
        Box.decode (Box.encode bytes) `shouldBe` Just bytes

    it "should return an error message in a monad that supports fail" $
      case Box.decode (PlainText (ByteString.pack [0x00])) of
        R.Success success -> expectationFailure $ "Expected failure, but got success: " ++ success
        R.Failure failure -> failure `shouldContain` "not enough bytes"

  describe "encrypt" $ do
    equivProp3 Box.encrypt (rpc Box.encryptR)

    it "encrypts data with a random keypair" $
      property $ \nonce plainText -> runTest $ do
        KeyPair sk pk <- rpc KeyPair.newKeyPairR
        combinedKey <- rpc CombinedKey.precomputeR sk pk
        cipherText <- rpc Box.encryptR combinedKey nonce plainText
        let decryptedText = Box.decrypt combinedKey nonce cipherText
        liftIO $ decryptedText `shouldBe` Just plainText

  describe "decrypt" $ do
    equivProp $ \combinedKey nonce plainText -> runTest $ do
      let cipherText = Box.encrypt combinedKey nonce plainText
      equiv3 Box.decrypt (rpc Box.decryptR) combinedKey nonce cipherText

    it "decrypts data encrypted with 'encrypt'" $
      property $ \combinedKey nonce plainText -> runTest $ do
        cipherText <- rpc Box.encryptR combinedKey nonce plainText
        decryptedText <- rpc Box.decryptR combinedKey nonce cipherText
        liftIO $ decryptedText `shouldBe` Just plainText

    it "decrypts encrypted data with a random keypair" $
      property $ \nonce plainText -> runTest $ do
        KeyPair sk pk <- rpc KeyPair.newKeyPairR
        combinedKey <- rpc CombinedKey.precomputeR sk pk
        let cipherText = Box.encrypt combinedKey nonce plainText
        decryptedText <- rpc Box.decryptR combinedKey nonce cipherText
        liftIO $ decryptedText `shouldBe` Just plainText

  it "supports communication with asymmetric keys" $
    property $ \nonce plainText -> runTest $ do
      KeyPair sk1 pk1 <- rpc KeyPair.newKeyPairR
      KeyPair sk2 pk2 <- liftIO KeyPair.newKeyPair

      key1 <- rpc CombinedKey.precomputeR sk1 pk2
      let key2 = CombinedKey.precompute sk2 pk1
      liftIO $ key1 `shouldBe` key2

      cipherText <- rpc Box.encryptR key1 nonce plainText
      let decryptedText = Box.decrypt key2 nonce cipherText
      liftIO $ decryptedText `shouldBe` Just plainText
