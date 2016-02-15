{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.NonceSpec where

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
    unless isTravis $ do
      nonce1 <- Nonce.newNonce
      nonce2 <- Nonce.newNonce
      nonce1 `shouldNotBe` nonce2

  it "generates a different nonce with 'nudge' for arbitrary nonces" $
    property $ \nonce ->
      Nonce.nudge nonce `shouldNotBe` nonce

  describe "increment" $ do
    it "increments a 0 nonce to 1" $
      let nonce = read "\"000000000000000000000000000000000000000000000000\"" in
      let nonce' = read "\"000000000000000000000000000000000000000000000001\"" in
      Nonce.increment nonce `shouldBe` nonce'

    it "increments a max nonce to 0" $
      let nonce = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\"" in
      let nonce' = read "\"000000000000000000000000000000000000000000000000\"" in
      Nonce.increment nonce `shouldBe` nonce'

    it "increments a max-1 nonce to max" $
      let nonce = read "\"fffffffffffffffffffffffffffffffffffffffffffffffe\"" in
      let nonce' = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\"" in
      Nonce.increment nonce `shouldBe` nonce'

    it "increments a little endian max-1 nonce to little endian 255" $
      let nonce = read "\"feffffffffffffffffffffffffffffffffffffffffffffff\"" in
      let nonce' = read "\"ff0000000000000000000000000000000000000000000000\"" in
      Nonce.increment nonce `shouldBe` nonce'
