-- | Whether a message is a few emoji alone, which show large, as Signal
-- shows them. GHC knows no emoji property, so the ranges are here.
module Tox.Android.Emoji
  ( emojiOnly
  ) where

import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as Text

-- | At most this many emoji show large.
maxEmoji :: Int
maxEmoji = 3

-- | One to 'maxEmoji' emoji, nothing else.
emojiOnly :: Text -> Bool
emojiOnly = go 0 . map ord . Text.unpack
  where
    go n [] = n > 0
    go n cs
      | n >= maxEmoji = False
      | otherwise = maybe False (go (n + 1)) (emoji cs)

-- | What follows one emoji at the start.
emoji :: [Int] -> Maybe [Int]
emoji cs = case cs of
  -- A flag, as a pair of regional indicators.
  a : b : rest | regional a && regional b -> Just rest
  -- A keycap.
  k : rest | k `elem` map ord "0123456789#*" -> case dropWhile (== vs16) rest of
    0x20e3 : rest' -> Just rest'
    _              -> Nothing
  c : rest | pictographic c -> joined (modified c rest)
  _ -> Nothing
  where
    regional c = c >= 0x1f1e6 && c <= 0x1f1ff
    -- A skin tone, the emoji presentation selector or a flag's tags.
    modified c rest =
      let afterVs = case rest of
            v : more | v == vs16 -> Just more
            more | presentsAsEmoji c -> Just more
                 | otherwise -> case more of
                     m : _ | tone m -> Just more
                     _              -> Nothing
      in fmap (dropWhile (\x -> tone x || x == vs16 || (x >= 0xe0020 && x <= 0xe007f))) afterVs
    joined Nothing = Nothing
    joined (Just (0x200d : c : rest)) | pictographic c = joined (modified c rest)
    joined (Just rest) = Just rest
    tone c = c >= 0x1f3fb && c <= 0x1f3ff

vs16 :: Int
vs16 = 0xfe0f

-- | Extended_Pictographic, as far as emoji keyboards offer it.
pictographic :: Int -> Bool
pictographic c = c >= 0x1f000 && c <= 0x1faff && not (c >= 0x1f1e6 && c <= 0x1f1ff) && not (c >= 0x1f3fb && c <= 0x1f3ff)
  || any (\(lo, hi) -> c >= lo && c <= hi)
       [ (0xa9, 0xa9), (0xae, 0xae), (0x203c, 0x203c), (0x2049, 0x2049), (0x2122, 0x2122), (0x2139, 0x2139)
       , (0x2194, 0x2199), (0x21a9, 0x21aa), (0x231a, 0x231b), (0x2328, 0x2328), (0x23cf, 0x23cf)
       , (0x23e9, 0x23f3), (0x23f8, 0x23fa), (0x24c2, 0x24c2), (0x25aa, 0x25ab), (0x25b6, 0x25b6)
       , (0x25c0, 0x25c0), (0x25fb, 0x25fe), (0x2600, 0x27bf), (0x2934, 0x2935), (0x2b05, 0x2b07)
       , (0x2b1b, 0x2b1c), (0x2b50, 0x2b50), (0x2b55, 0x2b55), (0x3030, 0x3030), (0x303d, 0x303d)
       , (0x3297, 0x3297), (0x3299, 0x3299) ]

-- | Whether it shows as an emoji without the presentation selector: all
-- above the basic plane but a few, and some below.
presentsAsEmoji :: Int -> Bool
presentsAsEmoji c
  | c >= 0x1f000 = c `notElem` [0x1f170, 0x1f171, 0x1f17e, 0x1f17f, 0x1f202, 0x1f237, 0x1f321]
  | otherwise = any (\(lo, hi) -> c >= lo && c <= hi)
      [ (0x231a, 0x231b), (0x23e9, 0x23ec), (0x23f0, 0x23f0), (0x23f3, 0x23f3), (0x25fd, 0x25fe)
      , (0x2614, 0x2615), (0x2648, 0x2653), (0x267f, 0x267f), (0x2693, 0x2693), (0x26a1, 0x26a1)
      , (0x26aa, 0x26ab), (0x26bd, 0x26be), (0x26c4, 0x26c5), (0x26ce, 0x26ce), (0x26d4, 0x26d4)
      , (0x26ea, 0x26ea), (0x26f2, 0x26f3), (0x26f5, 0x26f5), (0x26fa, 0x26fa), (0x26fd, 0x26fd)
      , (0x2705, 0x2705), (0x270a, 0x270b), (0x2728, 0x2728), (0x274c, 0x274c), (0x274e, 0x274e)
      , (0x2753, 0x2755), (0x2757, 0x2757), (0x2795, 0x2797), (0x27b0, 0x27b0), (0x27bf, 0x27bf)
      , (0x2b1b, 0x2b1c), (0x2b50, 0x2b50), (0x2b55, 0x2b55) ]
