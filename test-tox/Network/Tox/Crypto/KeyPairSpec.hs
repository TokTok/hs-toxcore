{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.KeyPairSpec where

import           Control.Monad.IO.Class         (liftIO)
import           Network.Tox.RPC                (runClient)
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

    it "generates different key pairs on subsequent calls" $ runClient $ do
      kp1 <- KeyPair.newKeyPairC
      kp2 <- KeyPair.newKeyPairC
      liftIO $ kp1 `shouldNotBe` kp2

    it "generates different secret keys on subsequent calls" $ runClient $ do
      KeyPair sk1 _ <- KeyPair.newKeyPairC
      KeyPair sk2 _ <- KeyPair.newKeyPairC
      liftIO $ sk1 `shouldNotBe` sk2

    it "generates different public keys on subsequent calls" $ runClient $ do
      KeyPair _ pk1 <- KeyPair.newKeyPairC
      KeyPair _ pk2 <- KeyPair.newKeyPairC
      liftIO $ pk1 `shouldNotBe` pk2

    it "generates a public key that is different from the secret key" $ runClient $ do
      KeyPair (Key.Key sk) (Key.Key pk) <- KeyPair.newKeyPairC
      liftIO $ Sodium.encode pk `shouldNotBe` Sodium.encode sk

  describe "fromSecretKey" $ do

    it "doesn't modify the secret key" $
      property $ \sk -> runClient $ do
        KeyPair sk' _ <- KeyPair.fromSecretKeyC sk
        liftIO $ sk' `shouldBe` sk

    it "never computes a public key that is equal to the secret key" $
      property $ \sk -> runClient $ do
        KeyPair _ (Key.Key pk) <- KeyPair.fromSecretKeyC sk
        liftIO $ Sodium.encode pk `shouldNotBe` Sodium.encode sk

    it "computes a usable public key from an invalid secret key" $
      property $ \plainText nonce -> runClient $ do
        KeyPair sk pk <- KeyPair.fromSecretKeyC $ read "\"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""
        ck <- CombinedKey.precomputeC sk pk
        encrypted <- Box.encryptC ck nonce plainText
        decrypted <- Box.decryptC ck nonce encrypted
        liftIO $ decrypted `shouldBe` Just plainText
