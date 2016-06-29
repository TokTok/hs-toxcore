{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module Network.Tox.Crypto.BoxSpec where

import           Control.Monad.IO.Class (liftIO)
import           Network.Tox.RPC        (runClient)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Box as Box


spec :: Spec
spec = do
  it "should decrypt encrypted data" $
    property $ \combinedKey nonce plainText -> runClient $ do
      cipherText <- Box.encryptC combinedKey nonce plainText
      Just decryptedText <- Box.decryptC combinedKey nonce cipherText
      liftIO $ decryptedText `shouldBe` plainText
