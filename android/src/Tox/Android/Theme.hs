-- | Sizes and colours, from Material's theme. Text takes the
-- platform theme's colours unless it needs its own.
module Tox.Android.Theme
  ( Theme (..)
  , Palette (..)
  , theme
  , dp
  ) where

import           Data.Word     (Word8)
import           Hatter.Widget (Color (..))

data Theme = Theme
  { thDensity     :: Double
    -- ^ Pixels per density-independent pixel.
  , thScreenWidth :: Double
    -- ^ In pixels.
  , thPalette     :: Palette
  } deriving (Eq, Show)

data Palette = Palette
  { pBackground :: Color
  , pText       :: Color
  , pBar        :: Color
    -- ^ Behind a chat's app bar.
  , pMuted      :: Color
    -- ^ Secondary text.
  , pAccent     :: Color
    -- ^ Field labels, unread counts.
  , pOnAccent   :: Color
  , pMine       :: Color
    -- ^ Our message bubbles, the add and send buttons.
  , pTheirs     :: Color
    -- ^ Their message bubbles, the message field.
  , pError      :: Color
  , pOnline     :: Color
  , pDivider    :: Color
  , pScrim      :: Color
    -- ^ Over the contacts while the drawer is open.
  } deriving (Eq, Show)

theme :: Double -> Double -> Bool -> Theme
theme density width dark = Theme
  { thDensity = if density > 0 then density else 1
  , thScreenWidth = width
  , thPalette = if dark then darkPalette else lightPalette
  }

dp :: Theme -> Double -> Double
dp t = (* thDensity t)

lightPalette :: Palette
lightPalette = Palette
  { pBackground = rgb 0xf9 0xf9 0xff
  , pText = rgb 0x1a 0x1c 0x20
  , pBar = rgb 0xed 0xed 0xf4
  , pMuted = rgb 0x44 0x47 0x4e
  , pAccent = rgb 0x41 0x5f 0x91
  , pOnAccent = rgb 0xff 0xff 0xff
  , pMine = rgb 0xd6 0xe3 0xff
  , pTheirs = rgb 0xe2 0xe2 0xe9
  , pError = rgb 0xba 0x1a 0x1a
  , pOnline = rgb 0x4c 0xaf 0x50
  , pDivider = rgb 0xc4 0xc6 0xd0
  , pScrim = Color 0 0 0 0x52
  }

darkPalette :: Palette
darkPalette = Palette
  { pBackground = rgb 0x11 0x13 0x18
  , pText = rgb 0xe2 0xe2 0xe9
  , pBar = rgb 0x1d 0x20 0x24
  , pMuted = rgb 0xc4 0xc6 0xd0
  , pAccent = rgb 0xa8 0xc8 0xff
  , pOnAccent = rgb 0x0a 0x30 0x5f
  , pMine = rgb 0x1b 0x4a 0x7a
  , pTheirs = rgb 0x33 0x35 0x3a
  , pError = rgb 0xff 0xb4 0xab
  , pOnline = rgb 0x4c 0xaf 0x50
  , pDivider = rgb 0x44 0x47 0x4e
  , pScrim = Color 0 0 0 0x52
  }

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = Color r g b 0xff
