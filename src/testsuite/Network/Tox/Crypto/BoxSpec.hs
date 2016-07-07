{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module Network.Tox.Crypto.BoxSpec where

import           Control.Monad.IO.Class         (liftIO)
import           Network.Tox.RPCTest            (runTest)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import qualified Network.Tox.Crypto.KeyPair     as KeyPair


spec :: Spec
spec = do
  it "should decrypt its own encrypted data" $
    property $ \combinedKey nonce plainText -> runTest $ do
      cipherText <- Box.encryptC combinedKey nonce plainText
      decryptedText <- Box.decryptC combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` Just plainText

  it "should encrypt data with a generated keypair" $
    property $ \nonce plainText -> runTest $ do
      KeyPair sk pk <- KeyPair.newKeyPairC
      combinedKey <- CombinedKey.precomputeC sk pk
      cipherText <- Box.encryptC combinedKey nonce plainText
      let decryptedText = Box.decrypt combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` Just plainText

  it "should decrypt encrypted data with a generated keypair" $
    property $ \nonce plainText -> runTest $ do
      KeyPair sk pk <- KeyPair.newKeyPairC
      combinedKey <- CombinedKey.precomputeC sk pk
      let cipherText = Box.encrypt combinedKey nonce plainText
      decryptedText <- Box.decryptC combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` Just plainText

  it "supports communication with asymmetric keys" $
    property $ \nonce plainText -> runTest $ do
      KeyPair sk1 pk1 <- KeyPair.newKeyPairC
      KeyPair sk2 pk2 <- liftIO KeyPair.newKeyPair

      key1 <- CombinedKey.precomputeC sk1 pk2
      let key2 = CombinedKey.precompute sk2 pk1
      liftIO $ key1 `shouldBe` key2

      cipherText <- Box.encryptC key1 nonce plainText
      let decryptedText = Box.decrypt key2 nonce cipherText
      liftIO $ decryptedText `shouldBe` Just plainText
