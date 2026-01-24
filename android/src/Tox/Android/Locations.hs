{-# LANGUAGE OverloadedStrings #-}
-- | Places on Earth in messages, as a component: sent as the text
-- "geo:<latitude>,<longitude>", which every client can read, and shown as
-- a place. Nothing else knows the convention.
module Tox.Android.Locations
  ( location
  , locationText
  , mapsUri
  ) where

import           Control.Monad (guard)
import           Data.Char     (isDigit)
import           Data.Maybe    (fromMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Numeric       (showFFloat)
import           Text.Read     (readMaybe)

-- | The place a message stands for: exactly
-- "geo:" and two decimal numbers within the Earth's range.
location :: Text -> Maybe (Double, Double)
location text = do
  rest <- Text.stripPrefix "geo:" text
  [lat, lon] <- Just (Text.splitOn "," rest)
  latitude <- number lat
  longitude <- number lon
  if abs latitude <= 90 && abs longitude <= 180 then Just (latitude, longitude) else Nothing
  where
    number t = do
      let (whole, fraction) = Text.breakOn "." (fromMaybe t (Text.stripPrefix "-" t))
      guard (not (Text.null whole) && Text.all isDigit whole)
      guard (Text.null fraction || (Text.length fraction > 1 && Text.all isDigit (Text.tail fraction)))
      readMaybe (Text.unpack t)

-- | A place as the text that sends it: fixed decimals, as an exponent
-- like 5.0e-2 isn't one to those who read it back.
locationText :: Double -> Double -> Text
locationText latitude longitude = "geo:" <> fixed latitude <> "," <> fixed longitude
  where fixed x = Text.pack (showFFloat (Just 6) x "")

-- | What opens a place in the user's maps app, with a pin on it.
mapsUri :: Double -> Double -> Text
mapsUri latitude longitude = "geo:0,0?q=" <> point <> "," <> point'
  where
    point = Text.pack (showFFloat (Just 6) latitude "")
    point' = Text.pack (showFFloat (Just 6) longitude "")
