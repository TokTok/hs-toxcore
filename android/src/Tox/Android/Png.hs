-- | Just enough PNG to show a generated picture: RGB, stored without
-- compression, which keeps zlib out of the Android build.
module Tox.Android.Png
  ( Rgb
  , png
  ) where

import           Data.Bits               (complement, shiftR, xor, (.&.))
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Word               (Word32, Word8)

type Rgb = (Word8, Word8, Word8)

-- | A picture of @width@ by @height@ pixels as a PNG, given each pixel's
-- colour by column and row.
png :: Int -> Int -> (Int -> Int -> Rgb) -> BS.ByteString
png width height pixel = BL.toStrict . B.toLazyByteString $
  B.byteString (BS.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a])
    <> chunk "IHDR" (build $ word width <> word height
                       <> B.word8 8      -- bits per channel
                       <> B.word8 2      -- RGB
                       <> B.word8 0 <> B.word8 0 <> B.word8 0)
    <> chunk "IDAT" (zlib scanlines)
    <> chunk "IEND" BS.empty
  where
    word = B.word32BE . fromIntegral
    scanlines = build $ mconcat
      [ B.word8 0 <> mconcat [rgb (pixel x y) | x <- [0 .. width - 1]]
      | y <- [0 .. height - 1] ]
    rgb (r, g, b) = B.word8 r <> B.word8 g <> B.word8 b

build :: B.Builder -> BS.ByteString
build = BL.toStrict . B.toLazyByteString

chunk :: String -> BS.ByteString -> B.Builder
chunk kind body =
  B.word32BE (fromIntegral (BS.length body)) <> B.byteString typed
    <> B.word32BE (crc32 typed)
  where typed = BS.pack (map (fromIntegral . fromEnum) kind) <> body

-- | Deflate's stored blocks, at most 65535 bytes each, in a zlib stream.
zlib :: BS.ByteString -> BS.ByteString
zlib bytes = build $
  B.word8 0x78 <> B.word8 0x01 <> blocks bytes <> B.word32BE (adler32 bytes)
  where
    blocks rest =
      let (block, more) = BS.splitAt 65535 rest
          size = fromIntegral (BS.length block)
      in B.word8 (if BS.null more then 1 else 0)
           <> B.word16LE size <> B.word16LE (complement size)
           <> B.byteString block
           <> (if BS.null more then mempty else blocks more)

crc32 :: BS.ByteString -> Word32
crc32 = complement . BS.foldl' step 0xffffffff
  where
    step crc byte = iterate shift (crc `xor` fromIntegral byte) !! 8
    shift c = if c .&. 1 == 1 then 0xedb88320 `xor` (c `shiftR` 1) else c `shiftR` 1

adler32 :: BS.ByteString -> Word32
adler32 bytes = b * 65536 + a
  where
    (a, b) = foldl' step (1, 0) (BS.unpack bytes)
    step (x, y) byte = let x' = (x + fromIntegral byte) `mod` 65521
                       in (x', (y + x') `mod` 65521)
