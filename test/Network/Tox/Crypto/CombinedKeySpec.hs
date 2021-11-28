{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.CombinedKeySpec where

import           Control.Monad.IO.Class         (liftIO)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))


spec :: Spec
spec =
  describe "precompute" $ do
    it "always computes the same combined key for the same public/secret keys" $
      property $ \sk pk -> do
        let ck1 = CombinedKey.precompute sk pk
        let ck2 = CombinedKey.precompute sk pk
        ck1 `shouldBe` ck2

    it "computes the same combined key for pk1/sk2 and pk2/sk1" $
      property $ \(KeyPair sk1 pk1) (KeyPair sk2 pk2) -> do
        let ck1 = CombinedKey.precompute sk1 pk2
        let ck2 = CombinedKey.precompute sk2 pk1
        ck1 `shouldBe` ck2
