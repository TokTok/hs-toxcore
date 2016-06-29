{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module Network.Tox.Crypto.BoxSpec where

import           Control.Monad.IO.Class (liftIO)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Box as Box
import           Network.Tox.RPC        (runClient)


spec :: Spec
spec = do
  it "should decrypt \"hello\"" $
    property $ \combinedKey nonce -> runClient $ do
      cipherText <- Box.encryptC combinedKey nonce "Hello"
      decryptedText <- Box.decryptC combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` Just "Hello"

  it "should decrypt encrypted data" $
    property $ \combinedKey nonce plainText -> runClient $ do
      cipherText <- Box.encryptC combinedKey nonce plainText
      decryptedText <- Box.decryptC combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` Just plainText
