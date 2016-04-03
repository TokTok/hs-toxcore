{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.NonceExtSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.Nonce      as Nonce
import qualified Network.Tox.ExternalTest      as Test
import qualified Network.Tox.ExternalTest.Test as Test (Result (..), Test (..))


spec :: Spec
spec =
  describe "increment" $ do
    it "increments correctly" $
      property $ \nonce ->
        Test.run Test.NonceIncrement nonce $
          Test.Success $ Nonce.increment nonce

    it "increments a 0 nonce to 1" $
      let nonce = read "\"000000000000000000000000000000000000000000000000\"" in
      Test.run Test.NonceIncrement nonce $
        Test.Success $ Nonce.increment nonce

    it "increments a max nonce to 0" $
      let nonce = read "\"ffffffffffffffffffffffffffffffffffffffffffffffff\"" in
      Test.run Test.NonceIncrement nonce $
        Test.Success $ Nonce.increment nonce

    it "increments a max-1 nonce to max" $
      let nonce = read "\"fffffffffffffffffffffffffffffffffffffffffffffffe\"" in
      Test.run Test.NonceIncrement nonce $
        Test.Success $ Nonce.increment nonce

    it "increments a little endian max-1 nonce to little endian 255" $
      let nonce = read "\"feffffffffffffffffffffffffffffffffffffffffffffff\"" in
      Test.run Test.NonceIncrement nonce $
        Test.Success $ Nonce.increment nonce
