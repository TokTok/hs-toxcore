{-# LANGUAGE OverloadedStrings #-}
-- | Adding a device with one action on each side: the home device hands out
-- a link with a one-time code, and the new device's friend request carries
-- it.
module Tox.Sync.Pairing
  ( PairingLink (..)
  , parseLink
  , requestMessage
  ) where

import           Data.Bits              (xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Char              (isHexDigit)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text

-- | @tox:<address>?pair=<code>@: the home's Tox address and the code, both
-- in hex.
data PairingLink = PairingLink
  { linkAddress :: Text
  , linkCode    :: Text
  } deriving (Eq, Show)

-- | A link whose address has 38 bytes, with a checksum that holds, and
-- whose code has 16.
parseLink :: Text -> Maybe PairingLink
parseLink link = do
  rest <- Text.stripPrefix "tox:" (Text.strip link)
  let (address, query) = Text.breakOn "?pair=" rest
  code <- Text.stripPrefix "?pair=" query
  if hex 76 address && hex 32 code && checksummed address
    then Just (PairingLink (Text.toUpper address) (Text.toLower code))
    else Nothing
  where hex n t = Text.length t == n && Text.all isHexDigit t

-- | Whether an address's last two bytes are the XOR of its others in pairs.
checksummed :: Text -> Bool
checksummed address = case Base16.decode (Text.encodeUtf8 address) of
  Right bytes | BS.length bytes == 38 ->
    let (body, sums) = BS.splitAt 36 bytes
        pairs = [(BS.index body i, BS.index body (i + 1)) | i <- [0, 2 .. 34]]
        (a, b) = foldl (\(x, y) (p, q) -> (x `xor` p, y `xor` q)) (0, 0) pairs
    in BS.pack [a, b] == sums
  _ -> False

-- | What the new device's friend request says.
requestMessage :: PairingLink -> Text
requestMessage link = "pair:" <> linkCode link
