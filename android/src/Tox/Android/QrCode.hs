{-# LANGUAGE OverloadedStrings #-}
-- | Tox IDs as QR codes, which other Tox clients scan.
module Tox.Android.QrCode
  ( toxIdQr
  ) where

import           Codec.QRCode              (ErrorLevel (L),
                                            TextEncoding (Iso8859_1),
                                            defaultQRCodeOptions, encodeText)
import           Codec.QRCode.Data.QRImage (QRImage (..))
import qualified Data.ByteString           as BS
import           Data.Text                 (Text)
import qualified Data.Vector.Unboxed       as Vector

import           Tox.Android.Png           (png)

-- | A Tox ID's code as a PNG of @size@ pixels square, dark on white, with
-- a white border a twentieth of it wide.
toxIdQr :: Int -> Text -> BS.ByteString
toxIdQr size address =
  maybe BS.empty draw (encodeText (defaultQRCodeOptions L) Iso8859_1 ("tox:" <> address))
  where
    border = size `div` 20
    draw qr = png size size $ \x y ->
      if dark (cell x) (cell y) then (0, 0, 0) else (255, 255, 255)
      where
        n = qrImageSize qr
        cell p = (p - border) * n `div` (size - 2 * border)
        dark cx cy = cx >= 0 && cy >= 0 && cx < n && cy < n
          && qrImageData qr Vector.! (cy * n + cx)
