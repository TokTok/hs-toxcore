{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.Crypto.BoxSpec where

import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString                as ByteString
import           Data.Proxy                     (Proxy (..))
import qualified Data.Result                    as R
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

  describe "encrypt" $
    it "encrypts data with a random keypair" $
      property $ \nonce plainText -> do
        KeyPair sk pk <- KeyPair.newKeyPair
        let combinedKey = CombinedKey.precompute sk pk
        let cipherText = Box.encrypt combinedKey nonce plainText
        let decryptedText = Box.decrypt combinedKey nonce cipherText
        decryptedText `shouldBe` Just plainText

  describe "decrypt" $ do
    it "decrypts data encrypted with 'encrypt'" $
      property $ \combinedKey nonce plainText -> do
        let cipherText = Box.encrypt combinedKey nonce plainText
        let decryptedText = Box.decrypt combinedKey nonce cipherText
        decryptedText `shouldBe` Just plainText

    it "decrypts encrypted data with a random keypair" $
      property $ \nonce plainText -> do
        KeyPair sk pk <- KeyPair.newKeyPair
        let combinedKey = CombinedKey.precompute sk pk
        let cipherText = Box.encrypt combinedKey nonce plainText
        let decryptedText = Box.decrypt combinedKey nonce cipherText
        decryptedText `shouldBe` Just plainText

  it "supports communication with asymmetric keys" $
    property $ \nonce plainText -> do
      KeyPair sk1 pk1 <- KeyPair.newKeyPair
      KeyPair sk2 pk2 <- KeyPair.newKeyPair

      let key1 = CombinedKey.precompute sk1 pk2
      let key2 = CombinedKey.precompute sk2 pk1
      key1 `shouldBe` key2

      let cipherText = Box.encrypt key1 nonce plainText
      let decryptedText = Box.decrypt key2 nonce cipherText
      decryptedText `shouldBe` Just plainText
