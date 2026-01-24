-- | A key's picture, as Tox clients draw it, so a contact looks the same
-- in each: blocks of two colours from the key's SHA-256, mirrored left to
-- right.
module Tox.Android.Identicon
  ( Identicon (..)
  , identicon
  , identiconPng
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString    as BS
import           Data.Fixed         (mod')
import           Data.Word          (Word8)

import           Tox.Android.Png    (Rgb, png)

rows :: Int
rows = 5

-- | Columns from the middle out, as the rest are mirrored.
activeColumns :: Int
activeColumns = (rows + 1) `div` 2

data Identicon = Identicon
  { identiconColours :: (Rgb, Rgb)
  , identiconBlocks  :: [[Bool]]
    -- ^ By row then column, 'True' for the second colour.
  } deriving (Eq, Show)

identicon :: BS.ByteString -> Identicon
identicon key = Identicon
  { identiconColours = (colour 0 first, colour 1 second)
  , identiconBlocks =
      [ [ odd (BS.index rest ((row * activeColumns + mirrored col) `mod` BS.length rest))
        | col <- [0 .. rows - 1] ]
      | row <- [0 .. rows - 1] ]
  }
  where
    hash = SHA256.hash key
    -- Each colour takes six bytes off the end of the hash.
    (afterFirst, first) = BS.splitAt (BS.length hash - 6) hash
    (rest, second) = BS.splitAt (BS.length afterFirst - 6) afterFirst
    mirrored col = abs (col * 2 - (rows - 1)) `div` 2

-- | The colour of six hash bytes: their value as the hue, lighter for the
-- second colour.
colour :: Int -> BS.ByteString -> Rgb
colour index bytes = hsl hue 0.5 (fromIntegral index / 2 + 0.3)
  where
    value = BS.foldl' (\n b -> n * 256 + fromIntegral b) 0 bytes :: Integer
    hue = fromIntegral value / 0xffffffffffff * 360

-- | As Flutter's HSLColor.toColor converts it.
hsl :: Double -> Double -> Double -> Rgb
hsl hue saturation lightness = (channel r, channel g, channel b)
  where
    chroma = (1 - abs (2 * lightness - 1)) * saturation
    secondary = chroma * (1 - abs ((hue / 60) `mod'` 2 - 1))
    match = lightness - chroma / 2
    (r, g, b)
      | hue < 60  = (chroma, secondary, 0)
      | hue < 120 = (secondary, chroma, 0)
      | hue < 180 = (0, chroma, secondary)
      | hue < 240 = (0, secondary, chroma)
      | hue < 300 = (secondary, 0, chroma)
      | otherwise = (chroma, 0, secondary)
    channel :: Double -> Word8
    channel c = floor ((c + match) * 255 + 0.5)

-- | The identicon as a square PNG of @size@ pixels a side, rounded down to
-- whole blocks.
identiconPng :: Int -> Identicon -> BS.ByteString
identiconPng size (Identicon (one, two) blocks) =
  png side side $ \x y ->
    if blocks !! (y `div` block) !! (x `div` block) then two else one
  where
    block = max 1 (size `div` rows)
    side = block * rows
