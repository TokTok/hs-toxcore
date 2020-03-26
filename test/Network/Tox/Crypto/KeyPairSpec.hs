{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.KeyPairSpec where

import           Control.Monad.IO.Class         (liftIO)
import qualified Crypto.Saltine.Class           as Sodium (encode)
import           Data.Proxy                     (Proxy (..))
import           Network.MessagePack.Rpc        (local)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import qualified Network.Tox.Crypto.Key         as Key
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy KeyPair)
  readShowSpec (Proxy :: Proxy KeyPair)

  describe "newKeyPair" $ do
    it "generates different key pairs on subsequent calls" $ do
      kp1 <- local KeyPair.newKeyPairR
      kp2 <- local KeyPair.newKeyPairR
      kp1 `shouldNotBe` kp2

    it "generates different secret keys on subsequent calls" $ do
      KeyPair sk1 _ <- local KeyPair.newKeyPairR
      KeyPair sk2 _ <- local KeyPair.newKeyPairR
      sk1 `shouldNotBe` sk2

    it "generates different public keys on subsequent calls" $ do
      KeyPair _ pk1 <- local KeyPair.newKeyPairR
      KeyPair _ pk2 <- local KeyPair.newKeyPairR
      pk1 `shouldNotBe` pk2

    it "generates a public key that is different from the secret key" $ do
      kp <- local KeyPair.newKeyPairR
      Sodium.encode (KeyPair.secretKey kp) `shouldNotBe` Sodium.encode (KeyPair.publicKey kp)

  describe "fromSecretKey" $ do
    it "doesn't modify the secret key" $
      property $ \sk -> do
        let KeyPair sk' _ = local KeyPair.fromSecretKeyR sk
        sk' `shouldBe` sk

    it "never computes a public key that is equal to the secret key" $
      property $ \sk -> do
        let KeyPair _ (Key.Key pk) = local KeyPair.fromSecretKeyR sk
        Sodium.encode pk `shouldNotBe` Sodium.encode sk

    it "computes a usable public key from an invalid secret key" $
      property $ \plainText nonce -> do
        let KeyPair sk pk = local KeyPair.fromSecretKeyR $ read "\"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""
        let ck = local CombinedKey.precomputeR sk pk
        let encrypted = local Box.encryptR ck nonce plainText
        let decrypted = local Box.decryptR ck nonce encrypted
        decrypted `shouldBe` Just plainText
