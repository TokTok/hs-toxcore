{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.NonceSpec where

import           Control.Monad.IO.Class   (liftIO)
import           Network.MessagePack.Rpc  (rpc, rpc)
import           Network.Tox.RPCTest      (equivProp1, runTest)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Nonce as Nonce


spec :: Spec
spec = do
  describe "newNonce" $
    it "generates a different nonce on subsequent calls to newNonce" $ runTest $ do
      nonce1 <- rpc Nonce.newNonceR
      nonce2 <- rpc Nonce.newNonceR
      liftIO $ nonce1 `shouldNotBe` nonce2

  describe "nudge" $
    it "creates a nonce that is different from the passed nonce" $
      property $ \nonce ->
        Nonce.nudge nonce `shouldNotBe` nonce

  describe "increment" $ do
    equivProp1 Nonce.increment (rpc Nonce.incrementR)

    it "generates a different nonce for arbitrary nonces" $
      property $ \nonce -> runTest $ do
        incremented <- rpc Nonce.incrementR nonce
        liftIO $ incremented `shouldNotBe` nonce

    it "increments a 0 nonce to 1" $ runTest $ do
      let nonce = read "\"000000000000000000000000000000000000000000000000\""
      let nonce' = read "\"000000000000000000000000000000000000000000000001\""
      incremented <- rpc Nonce.incrementR nonce
      liftIO $ incremented `shouldBe` nonce'

    it "increments a max nonce to 0" $ runTest $ do
      let nonce = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\""
      let nonce' = read "\"000000000000000000000000000000000000000000000000\""
      incremented <- rpc Nonce.incrementR nonce
      liftIO $ incremented `shouldBe` nonce'

    it "increments a max-1 nonce to max" $ runTest $ do
      let nonce = read "\"fffffffffffffffffffffffffffffffffffffffffffffffe\""
      let nonce' = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\""
      incremented <- rpc Nonce.incrementR nonce
      liftIO $ incremented `shouldBe` nonce'

    it "increments a little endian max-1 nonce to little endian 255" $ runTest $ do
      let nonce = read "\"feffffffffffffffffffffffffffffffffffffffffffffff\""
      let nonce' = read "\"ff0000000000000000000000000000000000000000000000\""
      incremented <- rpc Nonce.incrementR nonce
      liftIO $ incremented `shouldBe` nonce'
