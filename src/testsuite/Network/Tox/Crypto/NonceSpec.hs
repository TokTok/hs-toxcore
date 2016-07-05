{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.NonceSpec where

import           Control.Monad.IO.Class   (liftIO)
import           Network.Tox.RPC          (runClient)
import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative      ((<$>))
import           Control.Monad            (unless)
import           Network.Tox.Crypto.Key   (Nonce)
import qualified Network.Tox.Crypto.Nonce as Nonce
import           System.Environment       (lookupEnv)


spec :: Spec
spec = do
  it "generates a different nonce on subsequent calls to newNonce" $ do
    -- This test is broken on Travis. Apparently /dev/urandom is broken there.
    isTravis <- (Nothing /=) <$> lookupEnv "TRAVIS"
    unless isTravis $ runClient $ do
      nonce1 <- Nonce.newNonceC
      nonce2 <- Nonce.newNonceC
      liftIO $ nonce1 `shouldNotBe` nonce2

  describe "nudge" $
    it "creates a nonce that is different from the passed nonce" $
      property $ \nonce ->
        Nonce.nudge nonce `shouldNotBe` nonce

  describe "increment" $ do
    it "generates a different nonce for arbitrary nonces" $
      property $ \nonce -> runClient $ do
        incremented <- Nonce.incrementC nonce
        liftIO $ incremented `shouldNotBe` nonce

    it "increments a 0 nonce to 1" $ runClient $ do
      let nonce = read "\"000000000000000000000000000000000000000000000000\""
      let nonce' = read "\"000000000000000000000000000000000000000000000001\""
      incremented <- Nonce.incrementC nonce
      liftIO $ incremented `shouldBe` nonce'

    it "increments a max nonce to 0" $ runClient $ do
      let nonce = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\""
      let nonce' = read "\"000000000000000000000000000000000000000000000000\""
      incremented <- Nonce.incrementC nonce
      liftIO $ incremented `shouldBe` nonce'

    it "increments a max-1 nonce to max" $ runClient $ do
      let nonce = read "\"fffffffffffffffffffffffffffffffffffffffffffffffe\""
      let nonce' = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\""
      incremented <- Nonce.incrementC nonce
      liftIO $ incremented `shouldBe` nonce'

    it "increments a little endian max-1 nonce to little endian 255" $ runClient $ do
      let nonce = read "\"feffffffffffffffffffffffffffffffffffffffffffffff\""
      let nonce' = read "\"ff0000000000000000000000000000000000000000000000\""
      incremented <- Nonce.incrementC nonce
      liftIO $ incremented `shouldBe` nonce'
