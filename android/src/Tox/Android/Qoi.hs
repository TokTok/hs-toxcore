{-# LANGUAGE BangPatterns #-}
-- | QOI pictures, as a component: some clients send pictures as @.qoi@
-- files, which the platform can't show. They are decoded here, as the spec
-- says, and shown as a bitmap the platform reads.
module Tox.Android.Qoi
  ( isQoi
  , qoiSize
  , decodeQoi
  , qoiBitmap
  , encodeQoi
  ) where

import           Control.Monad                (when)
import           Control.Monad.ST             (ST, runST)
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as Builder
import qualified Data.ByteString.Internal     as BS (fromForeignPtr0)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Unsafe       as BS
import           Data.Char                    (toLower)
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed.Mutable  as VUM
import           Data.Word                    (Word32, Word8)
import           System.FilePath              (takeExtension)

-- | Whether a file is one by its name.
isQoi :: FilePath -> Bool
isQoi path = map toLower (takeExtension path) == ".qoi"

-- | The most pixels taken.
maxPixels :: Int
maxPixels = 2 ^ (25 :: Int)

headerSize, endSize :: Int
headerSize = 14
endSize = 8

-- | Width and height, from the header, if they may be decoded.
qoiSize :: BS.ByteString -> Maybe (Int, Int)
qoiSize bytes
  | BS.length bytes < headerSize = Nothing
  | BS.take 4 bytes /= BS.pack [0x71, 0x6f, 0x69, 0x66] = Nothing
  | width == 0 || height == 0 || width * height > maxPixels = Nothing
  | channels `notElem` [3, 4] || colourSpace > 1 = Nothing
  | otherwise = Just (width, height)
  where
    width = be32 4
    height = be32 8
    channels = BS.index bytes 12
    colourSpace = BS.index bytes 13
    be32 at = foldl (\n i -> n `shiftL` 8 .|. fromIntegral (BS.index bytes (at + i))) 0 [0 .. 3]

-- | Width, height and RGBA pixels, row by row, or why the file is no QOI
-- picture.
decodeQoi :: BS.ByteString -> Either String (Int, Int, VS.Vector Word8)
decodeQoi bytes = do
  (width, height) <- maybe (Left "bad header") Right (qoiSize bytes)
  let pixels = width * height
      end = BS.length bytes - endSize
  -- Each data byte gives at most 62 pixels, and no pixel takes more than 5.
  when (BS.length bytes > headerSize + pixels * 5 + endSize) $ Left "too long"
  when (end < headerSize || (end - headerSize) * 62 < pixels) $ Left "too short"
  when (BS.drop end bytes /= BS.pack [0, 0, 0, 0, 0, 0, 0, 1]) $ Left "bad end marker"
  case runST (decodePixels bytes end pixels) of
    Left why  -> Left why
    Right out -> Right (width, height, out)

decodePixels :: BS.ByteString -> Int -> Int -> ST s (Either String (VS.Vector Word8))
decodePixels bytes end pixels = do
  out <- VSM.new (pixels * 4)
  index <- VUM.replicate 64 (0 :: Word32)
  let byte = BS.unsafeIndex bytes
      go !i !p !run !px
        | i == pixels = pure (if p == end then Right () else Left "data left over")
        | run > 0 = put i p (run - 1) px
        | p >= end = pure (Left "data ends early")
        | otherwise = case byte p of
            0xfe | p + 3 < end ->
              put i (p + 4) 0 (rgba (byte (p + 1)) (byte (p + 2)) (byte (p + 3)) (channel 3 px))
            0xff | p + 4 < end ->
              put i (p + 5) 0 (rgba (byte (p + 1)) (byte (p + 2)) (byte (p + 3)) (byte (p + 4)))
            b | b >= 0xfe -> pure (Left "data ends early")
            b -> case b .&. 0xc0 of
              0x00 -> VUM.unsafeRead index (fromIntegral b) >>= put i (p + 1) 0
              0x40 ->
                let d s = fromIntegral ((b `shiftR` s) .&. 3) - 2
                in put i (p + 1) 0 (rgba (channel 0 px + d 4) (channel 1 px + d 2) (channel 2 px + d 0) (channel 3 px))
              0x80 | p + 1 < end ->
                let b2 = byte (p + 1)
                    vg = (b .&. 0x3f) - 32
                in put i (p + 2) 0 (rgba (channel 0 px + vg - 8 + (b2 `shiftR` 4))
                                         (channel 1 px + vg)
                                         (channel 2 px + vg - 8 + (b2 .&. 0x0f)) (channel 3 px))
              0x80 -> pure (Left "data ends early")
              _ -> put i (p + 1) (fromIntegral (b .&. 0x3f)) px
      -- Every pixel goes into the index, also after a run or an index.
      put i p run px = do
        VUM.unsafeWrite index (hash px) px
        mapM_ (\c -> VSM.unsafeWrite out (i * 4 + c) (channel c px)) [0 .. 3]
        go (i + 1) p (run :: Int) px
  result <- go 0 headerSize 0 (rgba 0 0 0 255)
  case result of
    Left why -> pure (Left why)
    Right () -> Right <$> VS.unsafeFreeze out
  where
    hash px = fromIntegral ((channel 0 px * 3 + channel 1 px * 5 + channel 2 px * 7 + channel 3 px * 11) `mod` 64)

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
rgba r g b a = fromIntegral r `shiftL` 24 .|. fromIntegral g `shiftL` 16 .|. fromIntegral b `shiftL` 8 .|. fromIntegral a

channel :: Int -> Word32 -> Word8
channel c px = fromIntegral (px `shiftR` (24 - 8 * c))

-- | RGBA pixels, row by row, as a QOI file, as the spec says, if there may
-- be so many: every pixel goes into the index, and opaque pictures have
-- three channels.
encodeQoi :: Int -> Int -> BS.ByteString -> Maybe BS.ByteString
encodeQoi width height pixels
  | width <= 0 || height <= 0 || width * height > maxPixels || BS.length pixels /= width * height * 4 = Nothing
  | otherwise = Just (BS.take used (BS.fromForeignPtr0 p n))
  where
    count = width * height
    opaque = all (\i -> BS.unsafeIndex pixels (i * 4 + 3) == 255) [0 .. count - 1]
    (out, used) = runST $ do
      buffer <- VSM.new (headerSize + count * 5 + endSize)
      let header = [0x71, 0x6f, 0x69, 0x66] ++ be32 width ++ be32 height ++ [if opaque then 3 else 4, 0]
      mapM_ (uncurry (VSM.unsafeWrite buffer)) (zip [0 ..] header)
      index <- VUM.replicate 64 (0 :: Word32)
      let at i c = BS.unsafeIndex pixels (i * 4 + c)
          pixel i = rgba (at i 0) (at i 1) (at i 2) (at i 3)
          write o bs = mapM_ (\(k, b) -> VSM.unsafeWrite buffer (o + k) b) (zip [0 ..] bs) >> pure (o + length bs)
          flush o run = if run > 0 then write o [0xc0 .|. fromIntegral (run - 1)] else pure o
          go !i !o !run !prev
            | i == count = flush o run
            | px == prev = do
                VUM.unsafeWrite index (hashOf px) px
                if run < 62 then go (i + 1) o (run + 1) prev else flush o run >>= \o' -> go (i + 1) o' 1 prev
            | otherwise = do
                o' <- flush o run
                let h = hashOf px
                seen <- VUM.unsafeRead index h
                VUM.unsafeWrite index h px
                o'' <- if seen == px then write o' [fromIntegral h] else write o' (step prev px)
                go (i + 1) o'' 0 px
            where px = pixel i
      end <- go 0 headerSize (0 :: Int) (rgba 0 0 0 255)
      end' <- write end [0, 0, 0, 0, 0, 0, 0, 1]
      frozen <- VS.unsafeFreeze buffer
      pure (frozen, end')
    (p, n) = VS.unsafeToForeignPtr0 out
    be32 v = [fromIntegral (v `shiftR` s) | s <- [24, 16, 8, 0]]
    step prev px
      | channel 3 px /= channel 3 prev = [0xff, channel 0 px, channel 1 px, channel 2 px, channel 3 px]
      | dr >= -2 && dr <= 1 && dg >= -2 && dg <= 1 && db >= -2 && db <= 1 =
          [0x40 .|. fromIntegral ((dr + 2) * 16 + (dg + 2) * 4 + (db + 2))]
      | dg >= -32 && dg <= 31 && dr - dg >= -8 && dr - dg <= 7 && db - dg >= -8 && db - dg <= 7 =
          [0x80 .|. fromIntegral (dg + 32), fromIntegral ((dr - dg + 8) * 16 + (db - dg + 8))]
      | otherwise = [0xfe, channel 0 px, channel 1 px, channel 2 px]
      where
        d c = let x = fromIntegral (channel c px) - fromIntegral (channel c prev) :: Int
              in if x > 127 then x - 256 else if x < -128 then x + 256 else x
        dr = d 0
        dg = d 1
        db = d 2
    hashOf px = fromIntegral ((channel 0 px * 3 + channel 1 px * 5 + channel 2 px * 7 + channel 3 px * 11) `mod` 64)

-- | A QOI picture as a bitmap file the platform reads, no larger than so
-- many pixels on its longer side.
qoiBitmap :: Int -> BS.ByteString -> Either String BS.ByteString
qoiBitmap longest bytes = do
  (width, height, pixels) <- decodeQoi bytes
  let k = max 1 ((max width height + longest - 1) `div` longest)
      w = max 1 (width `div` k)
      h = max 1 (height `div` k)
      -- Each pixel the mean of a k by k block, as BGRA.
      shrunk = VS.generate (w * h * 4) $ \n ->
        let (o, c) = n `divMod` 4
            (y, x) = o `divMod` w
            from = if c == 3 then 3 else 2 - c
            rows = min k height
            columns = min k width
            add !total dy dx
              | dy == rows = total
              | dx == columns = add total (dy + 1) 0
              | otherwise = add (total + fromIntegral (VS.unsafeIndex pixels
                  (((y * k + dy) * width + x * k + dx) * 4 + from))) dy (dx + 1)
        in fromIntegral (add (0 :: Int) 0 0 `div` (rows * columns)) :: Word8
  pure $ bmp w h shrunk

-- | 32 bits a pixel with alpha, top row first.
bmp :: Int -> Int -> VS.Vector Word8 -> BS.ByteString
bmp w h bgra = LBS.toStrict (Builder.toLazyByteString header) <> pixelBytes
  where
    size = w * h * 4
    offset = 14 + 108
    header = mconcat
      [ Builder.word8 0x42, Builder.word8 0x4d, w32 (offset + size), w32 0, w32 offset
      , w32 108, i32 w, i32 (negate h), Builder.word16LE 1, Builder.word16LE 32
      , w32 3, w32 size, w32 2835, w32 2835, w32 0, w32 0
      , Builder.word32LE 0x00ff0000, Builder.word32LE 0x0000ff00, Builder.word32LE 0x000000ff
      , Builder.word32LE 0xff000000, Builder.word32LE 0x73524742
      , mconcat (replicate 12 (w32 0))
      ]
    w32 = Builder.word32LE . fromIntegral
    i32 = Builder.int32LE . fromIntegral
    pixelBytes = let (p, n) = VS.unsafeToForeignPtr0 bgra in BS.fromForeignPtr0 p n
