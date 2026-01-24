module Tox.Android.QoiSpec (spec) where

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS
import           Data.Word            (Word8)
import           Test.Hspec

import           Tox.Android.Qoi

-- | A picture of so many pixels across and down, with its data.
picture :: Int -> Int -> [Word8] -> BS.ByteString
picture w h body = BS.pack $
  [0x71, 0x6f, 0x69, 0x66] ++ be32 w ++ be32 h ++ [4, 0] ++ body ++ [0, 0, 0, 0, 0, 0, 0, 1]
  where be32 n = [fromIntegral (n `div` 2 ^ (s :: Int)) | s <- [24, 16, 8, 0]]

pixels :: BS.ByteString -> Either String [Word8]
pixels = fmap (\(_, _, p) -> VS.toList p) . decodeQoi

spec :: Spec
spec = do
  it "decodes runs, and indexes every pixel" $ do
    -- RGB, a run of one more, RGB, the index of the first colour.
    pixels (picture 4 1 [0xfe, 10, 20, 30, 0xc0, 0xfe, 1, 2, 3, 9])
      `shouldBe` Right (concat [[10, 20, 30, 255], [10, 20, 30, 255], [1, 2, 3, 255], [10, 20, 30, 255]])
    -- A difference and a luma step from black.
    pixels (picture 2 1 [0x40 + 3 * 16 + 2 * 4 + 1, 0x80 + 32 + 4, 0x88])
      `shouldBe` Right ([1, 0, 255, 255] ++ [5, 4, 3, 255])

  it "refuses what isn't all there or is too big" $ do
    pixels (picture 2 1 [0xfe, 1, 2, 3]) `shouldBe` Left "data ends early"
    pixels (picture 1 1 [0xfe, 1, 2, 3, 0xc0]) `shouldBe` Left "data left over"
    pixels (picture 2 1 [0xfe, 1, 2]) `shouldBe` Left "data ends early"
    pixels (picture 300 1 [0xfe, 1, 2, 3]) `shouldBe` Left "too short"
    pixels (picture 8192 8192 []) `shouldBe` Left "bad header"
    pixels (BS.take 20 (picture 1 1 [0xfe, 1, 2, 3])) `shouldBe` Left "too short"
    pixels (BS.snoc (BS.init (picture 1 1 [0xfe, 1, 2, 3])) 2) `shouldBe` Left "bad end marker"

  it "encodes what it decodes, with three channels when opaque" $ do
    let rgbaOf = BS.pack . concat
        opaque = rgbaOf [[10, 20, 30, 255], [10, 20, 30, 255], [11, 19, 31, 255], [200, 0, 7, 255], [10, 20, 30, 255], [40, 50, 60, 255]]
        clear = rgbaOf [[0, 0, 0, 0], [1, 2, 3, 128], [1, 2, 3, 128], [255, 255, 255, 255]]
    fmap (BS.take 1 . BS.drop 12) (encodeQoi 3 2 opaque) `shouldBe` Just (BS.pack [3])
    (pixels =<< maybe (Left "none") Right (encodeQoi 3 2 opaque)) `shouldBe` Right (BS.unpack opaque)
    (pixels =<< maybe (Left "none") Right (encodeQoi 2 2 clear)) `shouldBe` Right (BS.unpack clear)
    let long = rgbaOf (replicate 200 [5, 5, 5, 255] ++ [[9, 9, 9, 255]])
    (pixels =<< maybe (Left "none") Right (encodeQoi 201 1 long)) `shouldBe` Right (BS.unpack long)
    encodeQoi 2 2 opaque `shouldBe` Nothing

  it "makes a bitmap the platform reads" $ do
    bitmap <- either fail pure (qoiBitmap 1 (picture 2 2 [0xfe, 10, 20, 30, 0xc2]))
    BS.take 2 bitmap `shouldBe` BS.pack [0x42, 0x4d]
    -- Shrunk to one pixel, in BGRA.
    BS.drop 122 bitmap `shouldBe` BS.pack [30, 20, 10, 255]
