{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Encoding where

import           Data.Binary.Bits.Get (BitGet)
import           Data.Binary.Bits.Put (BitPut)
import           Data.Binary.Get      (Get, getWord8)
import           Data.Binary.Put      (Put, putWord8)


class BitEncoding a where
  bitGet :: BitGet a
  bitPut :: a -> BitPut ()


-- TODO: Binary.get :: Get Bool is broken.
getBool :: Get Bool
getBool = getWord8 >>=
  \case
    0x00 -> return False
    0x01 -> return True
    byte -> fail $ "Invalid encoding for Bool (must be 0 or 1): " ++ show byte


putBool :: Bool -> Put
putBool False = putWord8 0x00
putBool True  = putWord8 0x01
