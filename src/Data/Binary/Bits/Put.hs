-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Put
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)
--
-- Put bits easily.
-----------------------------------------------------------------------------

module Data.Binary.Bits.Put
          ( BitPut
          , runBitPut
          , joinPut

          -- * Data types
          -- ** Bool
          , putBool

          -- ** Words
          , putWord8
          , putWord16be
          , putWord32be
          , putWord64be

          -- ** ByteString
          , putByteString
          )
          where

import qualified Data.Binary.Builder as B
import Data.Binary.Builder ( Builder )
import qualified Data.Binary.Put as Put
import Data.Binary.Put ( Put )

import Data.ByteString

import Control.Applicative
import Data.Bits
import Data.Monoid
import Data.Word

data BitPut a = BitPut { run :: (S -> PairS a) }

data PairS a = PairS a {-# UNPACK #-} !S

data S = S !Builder !Word8 !Int

-- | Put a 1 bit 'Bool'.
putBool :: Bool -> BitPut ()
putBool b = putWord8 1 (if b then 0xff else 0x00)

-- | make_mask 3 = 00000111
make_mask :: (Bits a, Num a) => Int -> a
make_mask n = (1 `shiftL` fromIntegral n) - 1
{-# SPECIALIZE make_mask :: Int -> Int #-}
{-# SPECIALIZE make_mask :: Int -> Word #-}
{-# SPECIALIZE make_mask :: Int -> Word8 #-}
{-# SPECIALIZE make_mask :: Int -> Word16 #-}
{-# SPECIALIZE make_mask :: Int -> Word32 #-}
{-# SPECIALIZE make_mask :: Int -> Word64 #-}

-- | Put the @n@ lower bits of a 'Word8'.
putWord8 :: Int -> Word8 -> BitPut ()
putWord8 n w = BitPut $ \s -> PairS () $
  let w' = make_mask n .&. w in
  case s of
                -- a whole word8, no offset
    (S b t o) | n == 8 && o == 0 -> flush $ S b w n
                -- less than a word8, will fit in the current word8
              | n <= 8 - o       -> flush $ S b (t .|. (w' `shiftL` (8 - n - o))) (o+n)
                -- will finish this word8, and spill into the next one
              | otherwise -> flush $
                              let o' = o + n - 8
                                  b' = t .|. (w' `shiftR` o')
                                  t' = w `shiftL` (8 - o')
                              in S (b `mappend` B.singleton b') t' o'

-- | Put the @n@ lower bits of a 'Word16'.
putWord16be :: Int -> Word16 -> BitPut ()
putWord16be n w
  | n <= 8 = putWord8 n (fromIntegral w)
  | otherwise =
      BitPut $ \s -> PairS () $
        let w' = make_mask n .&. w in
        case s of
          -- as n>=9, it's too big to fit into one single byte
          -- it'll either use 2 or 3 bytes
                                     -- it'll fit in 2 bytes
          (S b t o) | o + n <= 16 -> flush $
                        let o' = o + n - 8
                            b' = t .|. fromIntegral (w' `shiftR` o')
                            t' = fromIntegral (w `shiftL` (8-o'))
                        in (S (b `mappend` B.singleton b') t' o')
                                   -- 3 bytes required
                    | otherwise -> flush $
                        let o'  = o + n - 16
                            b'  = t .|. fromIntegral (w' `shiftR` (o' + 8))
                            b'' = fromIntegral ((w `shiftR` o') .&. 0xff)
                            t'  = fromIntegral (w `shiftL` (8-o'))
                        in (S (b `mappend` B.singleton b' `mappend` B.singleton b'') t' o')

-- | Put the @n@ lower bits of a 'Word32'.
putWord32be :: Int -> Word32 -> BitPut ()
putWord32be n w
  | n <= 16 = putWord16be n (fromIntegral w)
  | otherwise = do
      putWord32be (n-16) (w`shiftR`16)
      putWord32be    16  (w .&. 0x0000ffff)

-- | Put the @n@ lower bits of a 'Word64'.
putWord64be :: Int -> Word64 -> BitPut ()
putWord64be n w
  | n <= 32 = putWord32be n (fromIntegral w)
  | otherwise = do
      putWord64be (n-32) (w`shiftR`32)
      putWord64be    32  (w .&. 0xffffffff)

-- | Put a 'ByteString'.
putByteString :: ByteString -> BitPut ()
putByteString bs = do
  offset <- hasOffset
  if offset
    then mapM_ (putWord8 8) (unpack bs) -- naive
    else joinPut (Put.putByteString bs)
  where
    hasOffset = BitPut $ \ s@(S _ _ o) -> PairS (o /= 0) s

-- | Run a 'Put' inside 'BitPut'. Any partially written bytes will be flushed
-- before 'Put' executes to ensure byte alignment.
joinPut :: Put -> BitPut ()
joinPut m = BitPut $ \s0 -> PairS () $
  let (S b0 _ _) = flushIncomplete s0
      b = Put.execPut m
  in (S (b0`mappend`b) 0 0)

flush :: S -> S
flush s@(S b w o)
  | o > 8 = error "flush: offset > 8"
  | o == 8 = S (b `mappend` B.singleton w) 0 0
  | otherwise = s

flushIncomplete :: S -> S
flushIncomplete s@(S b w o)
  | o == 0 = s
  | otherwise = (S (b `mappend` B.singleton w) 0 0)

-- | Run the 'BitPut' monad inside 'Put'.
runBitPut :: BitPut () -> Put.Put
runBitPut m = Put.putBuilder b
  where
  PairS _ s = run m (S mempty 0 0)
  (S b _ _) = flushIncomplete s

instance Functor BitPut where
  fmap f (BitPut k) = BitPut $ \s ->
    let PairS x s' = k s
    in PairS (f x) s'

instance Applicative BitPut where
  pure a = BitPut (\s -> PairS a s)
  (BitPut f) <*> (BitPut g) = BitPut $ \s ->
    let PairS a s' = f s
        PairS b s'' = g s'
    in PairS (a b) s''

instance Monad BitPut where
  m >>= k = BitPut $ \s ->
    let PairS a s'  = run m s
        PairS b s'' = run (k a) s'
    in PairS b s''
  return x = BitPut $ \s -> PairS x s
