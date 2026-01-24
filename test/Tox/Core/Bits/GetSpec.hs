{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Core.Bits.GetSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Binary.Get      as B
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Word

import           Tox.Core.Bits.Get

spec :: Spec
spec = do
  describe "BitGet" $ do
    it "can read a bool" $
      let bs = BS.pack [0x80] -- 1000 0000
          res = B.runGet (runBitGet getBool) (LBS.fromStrict bs)
      in res `shouldBe` True

    it "can read multiple bools" $
      let bs = BS.pack [0xC0] -- 1100 0000
          res = B.runGet (runBitGet $ (,) <$> getBool <*> getBool) (LBS.fromStrict bs)
      in res `shouldBe` (True, True)

    it "can read word8" $
      let bs = BS.pack [0x70] -- 0111 0000
          res = B.runGet (runBitGet $ getWord8 4) (LBS.fromStrict bs)
      in res `shouldBe` 7

    it "can read across byte boundaries" $
      let bs = BS.pack [0x0F, 0x80] -- 0000 1111, 1000 0000
          res = B.runGet (runBitGet $ getWord8 4 >> getWord8 8) (LBS.fromStrict bs)
      in res `shouldBe` 0xF8 -- 1111 1000

    it "can read word16be" $
      let bs = BS.pack [0x12, 0x34]
          res = B.runGet (runBitGet $ getWord16be 16) (LBS.fromStrict bs)
      in res `shouldBe` 0x1234

    it "can read word32be" $
      let bs = BS.pack [0x12, 0x34, 0x56, 0x78]
          res = B.runGet (runBitGet $ getWord32be 32) (LBS.fromStrict bs)
      in res `shouldBe` 0x12345678

    it "can read word64be" $
      let bs = BS.pack [0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0]
          res = B.runGet (runBitGet $ getWord64be 64) (LBS.fromStrict bs)
      in res `shouldBe` 0x123456789ABCDEF0

  describe "Block" $ do
    it "reads the same as monadic BitGet" $ property $
      \(w1 :: Word8) (w2 :: Word8) ->
        let bs = BS.pack [w1, w2]
            monadic :: (Word16, Word16)
            monadic = B.runGet (runBitGet $ (,) . fromIntegral <$> getWord8 4 <*> getWord16be 12) (LBS.fromStrict bs)
            applicative :: (Word16, Word16)
            applicative = B.runGet (runBitGet $ block $ (,) . fromIntegral <$> word8 4 <*> word16be 12) (LBS.fromStrict bs)
        in monadic `shouldBe` applicative
