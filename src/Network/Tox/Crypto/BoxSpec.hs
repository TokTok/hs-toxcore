{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.BoxSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Box as Box


spec :: Spec
spec =
  it "should decrypt encrypted data" $
    property $ \combinedKey nonce plainText ->
      let
        cipherText = Box.encrypt combinedKey nonce plainText
        Just decryptedText = Box.decrypt combinedKey nonce cipherText
      in
      decryptedText `shouldBe` plainText
