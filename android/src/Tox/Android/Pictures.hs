{-# LANGUAGE OverloadedStrings #-}
-- | Ways of sending pictures other than as they are, as components: the
-- user picks one per chat, kept by its key, and pictures picked from the
-- gallery or taken with the camera go out as it makes them.
module Tox.Android.Pictures
  ( PictureFormat (..)
  , pictureFormats
  , pictureFormat
  ) where

import qualified Data.ByteString as BS
import           Data.List       (find)
import           Data.Text       (Text)

import           Tox.Android.Qoi (encodeQoi)

data PictureFormat = PictureFormat
  { formatKey       :: Text
  , formatName      :: Text
  , formatExtension :: String
  , formatMaxPixels :: Int
    -- ^ Pictures with more are sent as they are.
  , formatEncode    :: Int -> Int -> BS.ByteString -> Maybe BS.ByteString
    -- ^ RGBA pixels, row by row, as the file.
  }

pictureFormats :: [PictureFormat]
pictureFormats =
  [ PictureFormat "qoi" "QOI" ".qoi" (2 ^ (25 :: Int)) encodeQoi ]

pictureFormat :: Text -> Maybe PictureFormat
pictureFormat key = find ((== key) . formatKey) pictureFormats
