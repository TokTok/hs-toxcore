{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.CombinedKeySpec where

import           Control.Monad.IO.Class         (liftIO)
import           Network.MessagePack.Rpc        (rpc)
import           Network.Tox.RPCTest            (equivProp2, runTest)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))


spec :: Spec
spec =
  describe "precompute" $ do
    equivProp2 CombinedKey.precompute (rpc CombinedKey.precomputeR)

    it "always computes the same combined key for the same public/secret keys" $
      property $ \sk pk -> runTest $ do
        ck1 <- rpc CombinedKey.precomputeR sk pk
        ck2 <- rpc CombinedKey.precomputeR sk pk
        liftIO $ ck1 `shouldBe` ck2

    it "computes the same combined key for pk1/sk2 and pk2/sk1" $
      property $ \(KeyPair sk1 pk1) (KeyPair sk2 pk2) -> runTest $ do
        ck1 <- rpc CombinedKey.precomputeR sk1 pk2
        ck2 <- rpc CombinedKey.precomputeR sk2 pk1
        liftIO $ ck1 `shouldBe` ck2
