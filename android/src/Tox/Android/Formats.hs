{-# LANGUAGE OverloadedStrings #-}
-- | Ways some clients extend a friend message, as components: the core
-- knows messages only as the bytes Tox carried, and each format here may
-- read them for display. Nothing outside this module names a format.
module Tox.Android.Formats
  ( Format (..)
  , Reading (..)
  , formats
  , readWith
  ) where

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Maybe         (listToMaybe, mapMaybe)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as Text

-- | A way some clients extend a friend message.
data Format = Format
  { formatKey   :: Text
    -- ^ Kept for the setting that switches it off.
  , formatName  :: Text
  , formatAbout :: Text
    -- ^ What it does, by the setting.
  , readMessage :: ByteString -> Maybe Reading
    -- ^ Nothing if the message is not in this format.
  }

data Reading = Reading
  { readText   :: Text
    -- ^ What to show, copy, quote and preview.
  , readRepeat :: Maybe ByteString
    -- ^ A key: an earlier message of the chat with the same key makes this
    -- one a repeat.
  } deriving (Eq, Show)

-- | Every format hTox knows, read only from friends, never in groups.
formats :: [Format]
formats = [trifaV3]

-- | A message as the first of the formats that knows it reads it, or as
-- its text.
readWith :: [Format] -> ByteString -> Reading
readWith known bytes = case listToMaybe (mapMaybe (`readMessage` bytes) known) of
  Just r  -> r
  Nothing -> Reading (Text.decodeUtf8Lenient bytes) Nothing

-- | TRIfA's "message V3": the text, two zero bytes, an ID TRIfA keeps when
-- it sends the message again, and its time as big-endian Unix seconds.
trifaV3 :: Format
trifaV3 = Format "message_v3" "TRIfA message IDs"
  "Hides the ID TRIfA adds to messages, and shows a message it sends again only once" $ \bytes ->
  let end = BS.length bytes - trailer
      (text, rest) = BS.splitAt end bytes
  in if end > 0 && BS.take 2 rest == BS.pack [0, 0]
       then Just (Reading (Text.decodeUtf8Lenient text) (Just (BS.take idLength (BS.drop 2 rest))))
       else Nothing
  where
    idLength = 32
    trailer = 2 + idLength + 4
