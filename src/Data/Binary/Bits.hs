-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)
--
-- Parse and write bits easily. Parsing can be done either in a monadic style, or more
-- efficiently, using the 'Applicative' style. Writing is monadic style only.
-- See "Data.Binary.Bits.Get" and "Data.Binary.Bits.Put", respectively.
-----------------------------------------------------------------------------

module Data.Binary.Bits where

import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Data.Word

class BinaryBit a where
  putBits :: Int -> a -> BitPut ()
  getBits :: Int -> BitGet a

instance BinaryBit Bool where
  putBits _ = putBool
  getBits _ = getBool

instance BinaryBit Word8 where
  putBits = putWord8
  getBits = getWord8

instance BinaryBit Word16 where
  putBits = putWord16be
  getBits = getWord16be

instance BinaryBit Word32 where
  putBits = putWord32be
  getBits = getWord32be

instance BinaryBit Word64 where
  putBits = putWord64be
  getBits = getWord64be
