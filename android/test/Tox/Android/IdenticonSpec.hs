{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.IdenticonSpec (spec) where

import qualified Data.ByteString       as BS
import           Test.Hspec

import           Tox.Android.Identicon
import           Tox.Android.Model     (unhex)

spec :: Spec
spec = do
  -- What other clients draw for the same key.
  it "draws a key as other clients do" $ do
    let picture = identicon
          (unhex "7A114177E3934588EED87E6FE8B18AF7DC581C1FBB67F973EE5BF07B6EFABB72")
    map (map fromEnum) (identiconBlocks picture) `shouldBe`
      [ [0, 0, 0, 0, 0]
      , [1, 1, 0, 1, 1]
      , [0, 0, 0, 0, 0]
      , [0, 1, 1, 1, 0]
      , [1, 0, 0, 0, 1]
      ]
    identiconColours picture `shouldBe` ((0x73, 0x26, 0x50), (0xe6, 0xb3, 0xb9))

  it "makes a PNG" $ do
    let bytes = identiconPng 50 (identicon "key")
    BS.take 8 bytes `shouldBe` BS.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
    -- Every PNG ends with the same IEND chunk, whose checksum this is.
    BS.drop (BS.length bytes - 8) bytes `shouldBe`
      BS.pack [0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82]
