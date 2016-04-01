{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.CombinedKeySpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))


spec :: Spec
spec = do
  it "always computes the same combined key for the same public/secret keys" $
    property $ \sk pk ->
      let ck1 = CombinedKey.precompute sk pk in
      let ck2 = CombinedKey.precompute sk pk in
      ck1 `shouldBe` ck2

  it "computes the same combined key for pk1/sk2 and pk2/sk1" $
    property $ \(KeyPair sk1 pk1) (KeyPair sk2 pk2) ->
      let ck1 = CombinedKey.precompute sk1 pk2 in
      let ck2 = CombinedKey.precompute sk2 pk1 in
      ck1 `shouldBe` ck2
