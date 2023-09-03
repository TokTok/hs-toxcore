{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE RankNTypes   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Get
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)
--
-- Parse bits easily. Parsing can be done either in a monadic style, or more
-- efficiently, using the 'Applicative' style.
--
-- For the monadic style, write your parser as a 'BitGet' monad using the
--
--   * 'getBool'
--
--   * 'getWord8'
--
--   * 'getWord16be'
--
--   * 'getWord32be'
--
--   * 'getWord64be'
--
--   * 'getByteString'
--
-- functions and run it with 'runBitGet'.
--
-- For the applicative style, compose the fuctions
--
--   * 'bool'
--
--   * 'word8'
--
--   * 'word16be'
--
--   * 'word32be'
--
--   * 'word64be'
--
--   * 'byteString'
--
-- to make a 'Block'.
-- Use 'block' to turn it into the 'BitGet' monad to be able to run it with
-- 'runBitGet'.
-----------------------------------------------------------------------------

module Data.Binary.Bits.Get
            (
            -- * BitGet monad

            -- $bitget

              BitGet
            , runBitGet

            -- ** Get bytes
            , getBool
            , getWord8
            , getWord16be
            , getWord32be
            , getWord64be

            -- * Blocks

            -- $blocks
            , Block
            , block

            -- ** Read in Blocks
            , bool
            , word8
            , word16be
            , word32be
            , word64be
            , byteString
            , Data.Binary.Bits.Get.getByteString
            , Data.Binary.Bits.Get.getLazyByteString
            , Data.Binary.Bits.Get.isEmpty

            ) where

import           Data.Binary.Get          as B (Get, getLazyByteString, isEmpty)
import           Data.Binary.Get.Internal as B (ensureN, get, put)

import           Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import           Data.ByteString.Unsafe

import           Control.Applicative
import           Data.Bits
import           Data.Word

import           Prelude                  as P

#if !defined(__HADDOCK__)
import           GHC.Base
import           GHC.Word
#endif


-- $bitget
-- Parse bits using a monad.
--
-- @
--myBitParser :: 'Get' ('Word8', 'Word8')
--myBitParser = 'runGetBit' parse4by4
--
--parse4by4 :: 'BitGet' ('Word8', 'Word8')
--parse4by4 = do
--   bits <- 'getWord8' 4
--   more <- 'getWord8' 4
--   return (bits,more)
-- @

-- $blocks
-- Parse more efficiently in blocks. Each block is read with only one boundry
-- check (checking that there is enough input) as the size of the block can be
-- calculated statically. This is somewhat limiting as you cannot make the
-- parsing depend on the input being parsed.
--
-- @
--data IPV6Header = IPV6Header {
--     ipv6Version :: 'Word8'
--   , ipv6TrafficClass :: 'Word8'
--   , ipv6FlowLabel :: 'Word32
--   , ipv6PayloadLength :: 'Word16'
--   , ipv6NextHeader :: 'Word8'
--   , ipv6HopLimit :: 'Word8'
--   , ipv6SourceAddress :: 'ByteString'
--   , ipv6DestinationAddress :: 'ByteString'
-- }
--
-- ipv6headerblock =
--         IPV6Header '<$>' 'word8' 4
--                    '<*>' 'word8' 8
--                    '<*>' 'word32be' 24
--                    '<*>' 'word16be' 16
--                    '<*>' 'word8' 8
--                    '<*>' 'word8' 8
--                    '<*>' 'byteString' 16
--                    '<*>' 'byteString' 16
--
--ipv6Header :: 'Get' IPV6Header
--ipv6Header = 'runBitGet' ('block' ipv6headerblock)
-- @

data S = S {-# UNPACK #-} !ByteString -- Input
           {-# UNPACK #-} !Int -- Bit offset (0-7)
          deriving (Show)

-- | A block that will be read with only one boundry check. Needs to know the
-- number of bits in advance.
data Block a = Block Int (S -> a)

instance Functor Block where
  fmap f (Block i p) = Block i (f . p)

instance Applicative Block where
  pure a = Block 0 (const a)
  (Block i p) <*> (Block j q) = Block (i+j) (\s -> p s $ q (incS i s))
  (Block i _)  *> (Block j q) = Block (i+j) (q . incS i)
  (Block i p) <*  (Block j _) = Block (i+j) p

-- | Get a block. Will be read with one single boundry check, and
-- therefore requires a statically known number of bits.
-- Build blocks using 'bool', 'word8', 'word16be', 'word32be', 'word64be',
-- 'byteString' and 'Applicative'.
block :: Block a -> BitGet a
block (Block i p) = do
  ensureBits i
  s <- getState
  putState $! incS i s
  return $! p s

incS :: Int -> S -> S
incS o (S bs n) =
  let !o' = (n+o)
      !d = o' `shiftR` 3
      !n' = o' .&. makeMask 3
  in S (unsafeDrop d bs) n'

-- | makeMask 3 = 00000111
makeMask :: (Bits a, Num a) => Int -> a
makeMask n = (1 `shiftL` fromIntegral n) - 1
{-# SPECIALIZE makeMask :: Int -> Int #-}
{-# SPECIALIZE makeMask :: Int -> Word #-}
{-# SPECIALIZE makeMask :: Int -> Word8 #-}
{-# SPECIALIZE makeMask :: Int -> Word16 #-}
{-# SPECIALIZE makeMask :: Int -> Word32 #-}
{-# SPECIALIZE makeMask :: Int -> Word64 #-}

bitOffset :: Int -> Int
bitOffset n = makeMask 3 .&. n

byteOffset :: Int -> Int
byteOffset n = n `shiftR` 3

readBool :: S -> Bool
readBool (S bs n) = testBit (unsafeHead bs) (7-n)

{-# INLINE readWord8 #-}
readWord8 :: Int -> S -> Word8
readWord8 n (S bs o)
  -- no bits at all, return 0
  | n == 0 = 0

  -- all bits are in the same byte
  -- we just need to shift and mask them right
  | n <= 8 - o = let w = unsafeHead bs
                     m = makeMask n
                     w' = (w `shiftr_w8` (8 - o - n)) .&. m
                 in w'

  -- the bits are in two different bytes
  -- make a word16 using both bytes, and then shift and mask
  | n <= 8 = let w = (fromIntegral (unsafeHead bs) `shiftl_w16` 8) .|.
                     fromIntegral (unsafeIndex bs 1)
                 m = makeMask n
                 w' = (w `shiftr_w16` (16 - o - n)) .&. m
             in fromIntegral w'

{-# INLINE readWord16be #-}
readWord16be :: Int -> S -> Word16
readWord16be n s@(S bs o)

  -- 8 or fewer bits, use readWord8
  | n <= 8 = fromIntegral (readWord8 n s)

  -- handle 9 or more bits, stored in two bytes

  -- no offset, plain and simple 16 bytes
  | o == 0 && n == 16 = let msb = fromIntegral (unsafeHead bs)
                            lsb = fromIntegral (unsafeIndex bs 1)
                            w = (msb `shiftl_w16` 8) .|. lsb
                        in w

  -- no offset, but not full 16 bytes
  | o == 0 = let msb = fromIntegral (unsafeHead bs)
                 lsb = fromIntegral (unsafeIndex bs 1)
                 w = (msb `shiftl_w16` (n-8)) .|. (lsb `shiftr_w16` (16-n))
             in w

  -- with offset, and n=9-16
  | n <= 16 = readWithOffset s shiftl_w16 shiftr_w16 n

  | otherwise = error "readWord16be: tried to read more than 16 bits"

{-# INLINE readWord32be #-}
readWord32be :: Int -> S -> Word32
readWord32be n s@(S _ o)
  -- 8 or fewer bits, use readWord8
  | n <= 8 = fromIntegral (readWord8 n s)

  -- 16 or fewer bits, use readWord16be
  | n <= 16 = fromIntegral (readWord16be n s)

  | o == 0 = readWithoutOffset s shiftl_w32 shiftr_w32 n

  | n <= 32 = readWithOffset s shiftl_w32 shiftr_w32 n

  | otherwise = error "readWord32be: tried to read more than 32 bits"


{-# INLINE readWord64be #-}
readWord64be :: Int -> S -> Word64
readWord64be n s@(S _ o)
  -- 8 or fewer bits, use readWord8
  | n <= 8 = fromIntegral (readWord8 n s)

  -- 16 or fewer bits, use readWord16be
  | n <= 16 = fromIntegral (readWord16be n s)

  | o == 0 = readWithoutOffset s shiftl_w64 shiftr_w64 n

  | n <= 64 = readWithOffset s shiftl_w64 shiftr_w64 n

  | otherwise = error "readWord64be: tried to read more than 64 bits"


readByteString :: Int -> S -> ByteString
readByteString n s@(S bs o)
  -- no offset, easy.
  | o == 0 = unsafeTake n bs
  -- offset. ugg. this is really naive and slow. but also pretty easy :)
  | otherwise = B.pack (P.map (readWord8 8) (P.take n (iterate (incS 8) s)))

readWithoutOffset :: (Bits a, Num a)
                  => S -> (a -> Int -> a) -> (a -> Int -> a) -> Int -> a
readWithoutOffset (S bs o) shifterL shifterR n
  | o /= 0 = error "readWithoutOffset: there is an offset"

  | bitOffset n == 0 && byteOffset n <= 4 =
              let segs = byteOffset n
                  bn 0 = fromIntegral (unsafeHead bs)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

              in bn (segs-1)

  | n <= 64 = let segs = byteOffset n
                  o' = bitOffset (n - 8 + o)

                  bn 0 = fromIntegral (unsafeHead bs)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

                  msegs = bn (segs-1) `shifterL` o'

                  last = fromIntegral (unsafeIndex bs segs) `shifterR` (8 - o')

                  w = msegs .|. last
              in w

readWithOffset :: (Bits a, Num a)
         => S -> (a -> Int -> a) -> (a -> Int -> a) -> Int -> a
readWithOffset (S bs o) shifterL shifterR n
  | n <= 64 = let bits_in_msb = 8 - o
                  (n',top) = (n - bits_in_msb
                             , (fromIntegral (unsafeHead bs) .&. makeMask bits_in_msb) `shifterL` n')

                  segs = byteOffset n'

                  bn 0 = 0
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

                  o' = bitOffset n'

                  mseg = bn segs `shifterL` o'

                  last | o' > 0 = fromIntegral (unsafeIndex bs (segs + 1)) `shifterR` (8 - o')
                       | otherwise = 0

                  w = top .|. mseg .|. last
              in w

------------------------------------------------------------------------
-- | 'BitGet' is a monad, applicative and a functor. See 'runBitGet'
-- for how to run it.
newtype BitGet a = B { runState :: S -> Get (S,a) }

instance Monad BitGet where
  return x = B $ \s -> return (s,x)
  (B f) >>= g = B $ \s -> do (s',a) <- f s
                             runState (g a) s'

instance MonadFail BitGet where
  fail str = B $ \(S inp n) -> putBackState inp n >> fail str

instance Functor BitGet where
  fmap f m = m >>= \a -> return (f a)

instance Applicative BitGet where
  pure = return
  fm <*> m = fm >>= \f -> m >>= \v -> return (f v)

-- | Run a 'BitGet' within the Binary packages 'Get' monad. If a byte has
-- been partially consumed it will be discarded once 'runBitGet' is finished.
runBitGet :: BitGet a -> Get a
runBitGet bg = do
  s <- mkInitState
  (S str' n,a) <- runState bg s
  putBackState str' n
  return a

mkInitState :: Get S
mkInitState = do
  str <- get
  put B.empty
  return (S str 0)

putBackState :: B.ByteString -> Int -> Get ()
putBackState bs n = do
 remaining <- get
 put (B.drop (if n==0 then 0 else 1) bs `B.append` remaining)

getState :: BitGet S
getState = B $ \s -> return (s,s)

putState :: S -> BitGet ()
putState s = B $ \_ -> return (s,())

-- | Make sure there are at least @n@ bits.
ensureBits :: Int -> BitGet ()
ensureBits n = do
  (S bs o) <- getState
  if n <= (B.length bs * 8 - o)
    then return ()
    else do let currentBits = B.length bs * 8 - o
            let byteCount = (n - currentBits + 7) `div` 8
            B $ \_ -> do B.ensureN byteCount
                         bs' <- B.get
                         put B.empty
                         return (S (bs`append`bs') o, ())

-- | Get 1 bit as a 'Bool'.
getBool :: BitGet Bool
getBool = block bool

-- | Get @n@ bits as a 'Word8'. @n@ must be within @[0..8]@.
getWord8 :: Int -> BitGet Word8
getWord8 n = block (word8 n)

-- | Get @n@ bits as a 'Word16'. @n@ must be within @[0..16]@.
getWord16be :: Int -> BitGet Word16
getWord16be n = block (word16be n)

-- | Get @n@ bits as a 'Word32'. @n@ must be within @[0..32]@.
getWord32be :: Int -> BitGet Word32
getWord32be n = block (word32be n)

-- | Get @n@ bits as a 'Word64'. @n@ must be within @[0..64]@.
getWord64be :: Int -> BitGet Word64
getWord64be n = block (word64be n)

-- | Get @n@ bytes as a 'ByteString'.
getByteString :: Int -> BitGet ByteString
getByteString n = block (byteString n)

-- | Get @n@ bytes as a lazy ByteString.
getLazyByteString :: Int -> BitGet L.ByteString
getLazyByteString n = do
  (S _ o) <- getState
  case o of
    0 -> B $ \ (S bs o') -> do
            putBackState bs o'
            lbs <- B.getLazyByteString (fromIntegral n)
            return (S B.empty 0, lbs)
    _ -> L.fromChunks . (:[]) <$> Data.Binary.Bits.Get.getByteString n

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
isEmpty :: BitGet Bool
isEmpty = B $ \ (S bs o) -> if B.null bs
                               then B.isEmpty >>= \e -> return (S bs o, e)
                               else return (S bs o, False)

-- | Read a 1 bit 'Bool'.
bool :: Block Bool
bool = Block 1 readBool

-- | Read @n@ bits as a 'Word8'. @n@ must be within @[0..8]@.
word8 :: Int -> Block Word8
word8 n = Block n (readWord8 n)

-- | Read @n@ bits as a 'Word16'. @n@ must be within @[0..16]@.
word16be :: Int -> Block Word16
word16be n = Block n (readWord16be n)

-- | Read @n@ bits as a 'Word32'. @n@ must be within @[0..32]@.
word32be :: Int -> Block Word32
word32be n = Block n (readWord32be n)

-- | Read @n@ bits as a 'Word64'. @n@ must be within @[0..64]@.
word64be :: Int -> Block Word64
word64be n = Block n (readWord64be n)

-- | Read @n@ bytes as a 'ByteString'.
byteString :: Int -> Block ByteString
byteString n | n > 0 = Block (n*8) (readByteString n)
             | otherwise = Block 0 (const B.empty)

------------------------------------------------------------------------
-- Unchecked shifts, from the package binary

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if __GLASGOW_HASKELL__ < 900
uncheckedShiftLWord8# = uncheckedShiftL#
uncheckedShiftLWord16# = uncheckedShiftL#
uncheckedShiftLWord32# = uncheckedShiftL#

uncheckedShiftRLWord8# = uncheckedShiftRL#
uncheckedShiftRLWord16# = uncheckedShiftRL#
uncheckedShiftRLWord32# = uncheckedShiftRL#
#endif

#if !defined(__HADDOCK__)
shiftl_w8  (W8#  w) (I# i) = W8# (w `uncheckedShiftLWord8#`     i)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftLWord16#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftLWord32#`   i)

shiftr_w8  (W8#  w) (I# i) = W8# (w `uncheckedShiftRLWord8#`    i)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRLWord16#`  i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRLWord32#`  i)


#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#`  i)
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
foreign import ccall unsafe "stg_uncheckedShiftRL64"
    uncheckedShiftRL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#`  i)
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftl_w8 = shiftL
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL

shiftr_w8 = shiftR
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif
