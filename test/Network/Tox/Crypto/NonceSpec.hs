{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.NonceSpec where

import           Control.Monad.IO.Class   (liftIO)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Nonce as Nonce


spec :: Spec
spec = do
  describe "newNonce" $
    it "generates a different nonce on subsequent calls to newNonce" $ do
      nonce1 <- Nonce.newNonce
      nonce2 <- Nonce.newNonce
      liftIO $ nonce1 `shouldNotBe` nonce2

  describe "nudge" $
    it "creates a nonce that is different from the passed nonce" $
      property $ \nonce ->
        Nonce.nudge nonce `shouldNotBe` nonce

  describe "increment" $ do
    it "generates a different nonce for arbitrary nonces" $
      property $ \nonce -> do
        let incremented = Nonce.increment nonce
        incremented `shouldNotBe` nonce

    it "increments a 0 nonce to 1" $ do
      let nonce = read "\"000000000000000000000000000000000000000000000000\""
      let nonce' = read "\"000000000000000000000000000000000000000000000001\""
      let incremented = Nonce.increment nonce
      incremented `shouldBe` nonce'

    it "increments a max nonce to 0" $ do
      let nonce = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\""
      let nonce' = read "\"000000000000000000000000000000000000000000000000\""
      let incremented = Nonce.increment nonce
      incremented `shouldBe` nonce'

    it "increments a max-1 nonce to max" $ do
      let nonce = read "\"fffffffffffffffffffffffffffffffffffffffffffffffe\""
      let nonce' = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\""
      let incremented = Nonce.increment nonce
      incremented `shouldBe` nonce'

    it "increments a little endian max-1 nonce to little endian 255" $ do
      let nonce = read "\"feffffffffffffffffffffffffffffffffffffffffffffff\""
      let nonce' = read "\"ff0000000000000000000000000000000000000000000000\""
      let incremented = Nonce.increment nonce
      incremented `shouldBe` nonce'
