{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.KeyPairSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Crypto.Saltine.Class           as Sodium (encode)
import           Data.Proxy                     (Proxy (..))
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import qualified Network.Tox.Crypto.Key         as Key
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy KeyPair)

  describe "newKeyPair" $ do

    it "generates different key pairs on subsequent calls" $ do
      kp1 <- KeyPair.newKeyPair
      kp2 <- KeyPair.newKeyPair
      kp1 `shouldNotBe` kp2

    it "generates different secret keys on subsequent calls" $ do
      KeyPair sk1 _ <- KeyPair.newKeyPair
      KeyPair sk2 _ <- KeyPair.newKeyPair
      sk1 `shouldNotBe` sk2

    it "generates different public keys on subsequent calls" $ do
      KeyPair _ pk1 <- KeyPair.newKeyPair
      KeyPair _ pk2 <- KeyPair.newKeyPair
      pk1 `shouldNotBe` pk2

    it "generates a public key that is different from the secret key" $ do
      KeyPair (Key.Key sk) (Key.Key pk) <- KeyPair.newKeyPair
      Sodium.encode pk `shouldNotBe` Sodium.encode sk

  describe "fromSecretKey" $ do

    it "doesn't modify the secret key" $
      property $ \sk ->
        let KeyPair sk' _ = KeyPair.fromSecretKey sk in
        sk' `shouldBe` sk

    it "never computes a public key that is equal to the secret key" $
      property $ \sk ->
        let KeyPair _ (Key.Key pk) = KeyPair.fromSecretKey sk in
        Sodium.encode pk `shouldNotBe` Sodium.encode sk

    it "computes a usable public key from an invalid secret key" $
      property $ \plainText nonce ->
        let
          KeyPair sk pk = KeyPair.fromSecretKey $ read "\"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""
          ck = CombinedKey.precompute sk pk
        in
        Box.decrypt ck nonce (Box.encrypt ck nonce plainText) `shouldBe` Just plainText
