{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Session.FriendSpec (spec) where

import           Data.Bits           (xor)
import qualified Data.ByteString     as BS
import           Data.Word           (Word32)
import           Test.Hspec
import           Test.QuickCheck

import           Tox.Crypto.Core.Key (PublicKey)
import           Tox.Session.Friend

spec :: Spec
spec = do
  describe "Tox ID" $ do
    it "parses back to the public key and nospam" $ property $
      \(pk :: PublicKey) (nospam :: Word32) ->
        let m = initMessenger nospam
        in parseToxId (toxId pk m) `shouldBe` Just (pk, nospamBytes m)

    it "is rejected if any byte changed" $ property $
      \(pk :: PublicKey) (nospam :: Word32) (NonNegative i) (NonZero x) ->
        let bytes = toxId pk (initMessenger nospam)
            j = i `mod` BS.length bytes
            changed = BS.take j bytes <> BS.singleton (BS.index bytes j `xor` x) <> BS.drop (j + 1) bytes
        in parseToxId changed `shouldBe` Nothing

  describe "file chunks" $ do
    it "are counted for a stream of unknown size" $
      chunksBetween 0 maxBound `shouldSatisfy` (> 0)

    it "are shared out between transfers in turn" $
      roundRobin 5 [10, 1, 10] `shouldBe` [2, 1, 2]
