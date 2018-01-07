{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.KeyPairSpec where

import           Control.Monad.IO.Class         (liftIO)
import qualified Crypto.Saltine.Class           as Sodium (encode)
import           Data.Proxy                     (Proxy (..))
import           Network.MessagePack.Rpc        (rpc)
import           Network.Tox.RPCTest            (equivProp1, runTest)
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
    it "generates different key pairs on subsequent calls" $ runTest $ do
      kp1 <- rpc KeyPair.newKeyPairR
      kp2 <- rpc KeyPair.newKeyPairR
      liftIO $ kp1 `shouldNotBe` kp2

    it "generates different secret keys on subsequent calls" $ runTest $ do
      KeyPair sk1 _ <- rpc KeyPair.newKeyPairR
      KeyPair sk2 _ <- rpc KeyPair.newKeyPairR
      liftIO $ sk1 `shouldNotBe` sk2

    it "generates different public keys on subsequent calls" $ runTest $ do
      KeyPair _ pk1 <- rpc KeyPair.newKeyPairR
      KeyPair _ pk2 <- rpc KeyPair.newKeyPairR
      liftIO $ pk1 `shouldNotBe` pk2

    it "generates a public key that is different from the secret key" $ runTest $ do
      kp <- rpc KeyPair.newKeyPairR
      liftIO $
        Sodium.encode (KeyPair.secretKey kp)
        `shouldNotBe`
        Sodium.encode (KeyPair.publicKey kp)

  describe "fromSecretKey" $ do
    equivProp1 KeyPair.fromSecretKey (rpc KeyPair.fromSecretKeyR)

    it "doesn't modify the secret key" $
      property $ \sk -> runTest $ do
        KeyPair sk' _ <- rpc KeyPair.fromSecretKeyR sk
        liftIO $ sk' `shouldBe` sk

    it "never computes a public key that is equal to the secret key" $
      property $ \sk -> runTest $ do
        KeyPair _ (Key.Key pk) <- rpc KeyPair.fromSecretKeyR sk
        liftIO $ Sodium.encode pk `shouldNotBe` Sodium.encode sk

    it "computes a usable public key from an invalid secret key" $
      property $ \plainText nonce -> runTest $ do
        KeyPair sk pk <- rpc KeyPair.fromSecretKeyR $ read "\"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""
        ck <- rpc CombinedKey.precomputeR sk pk
        encrypted <- rpc Box.encryptR ck nonce plainText
        decrypted <- rpc Box.decryptR ck nonce encrypted
        liftIO $ decrypted `shouldBe` Just plainText
