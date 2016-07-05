{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module Network.Tox.Crypto.BoxSpec where

import           Control.Monad.IO.Class (liftIO)
import           Network.Tox.RPCTest    (runTest)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Box as Box


spec :: Spec
spec =
  it "should decrypt encrypted data" $
    property $ \combinedKey nonce plainText -> runTest $ do
      cipherText <- Box.encryptC combinedKey nonce plainText
      Just decryptedText <- Box.decryptC combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` plainText
