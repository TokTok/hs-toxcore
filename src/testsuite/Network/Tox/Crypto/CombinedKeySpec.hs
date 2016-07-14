{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.CombinedKeySpec where

import           Control.Monad.IO.Class         (liftIO)
import           Network.Tox.RPCTest            (equivProp2, runTest)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))


spec :: Spec
spec =
  describe "precompute" $ do
    equivProp2 CombinedKey.precompute CombinedKey.precomputeC

    it "always computes the same combined key for the same public/secret keys" $
      property $ \sk pk -> runTest $ do
        ck1 <- CombinedKey.precomputeC sk pk
        ck2 <- CombinedKey.precomputeC sk pk
        liftIO $ ck1 `shouldBe` ck2

    it "computes the same combined key for pk1/sk2 and pk2/sk1" $
      property $ \(KeyPair sk1 pk1) (KeyPair sk2 pk2) -> runTest $ do
        ck1 <- CombinedKey.precomputeC sk1 pk2
        ck2 <- CombinedKey.precomputeC sk2 pk1
        liftIO $ ck1 `shouldBe` ck2
