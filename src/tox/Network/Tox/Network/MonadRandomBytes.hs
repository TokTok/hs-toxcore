{-# LANGUAGE Trustworthy #-}

module Network.Tox.Network.MonadRandomBytes where

import           Control.Monad.Random              (RandT, getRandoms)
import           Control.Monad.Reader              (ReaderT)
import           Control.Monad.RWS                 (MonadReader, MonadState,
                                                    RWST, execRWST, local,
                                                    state)
import           Control.Monad.State               (StateT)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Writer              (WriterT)
import qualified Crypto.Saltine.Class              as Sodium (decode)
import qualified Crypto.Saltine.Internal.ByteSizes as Sodium (boxNonce)
import           Data.Binary.Get                   (Get, getWord64be, runGet)
import           Data.Bits                         (FiniteBits, finiteBitSize)
import qualified Data.Bits                         as Bits
import           Data.ByteString                   (ByteString, pack, unpack)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.Maybe                        (fromJust)
import           Data.Word                         (Word64, Word8)
import           System.Entropy                    (getEntropy)
import           System.Random                     (RandomGen)


import           Network.Tox.Crypto.Key            (Key (..), Nonce, PublicKey)
import qualified Network.Tox.Crypto.Nonce          as Nonce

class Monad m => MonadRandomBytes m where
  randomBytes :: Int -> m ByteString

instance (Monad m, RandomGen s) => MonadRandomBytes (RandT s m) where
  randomBytes n = pack . take n <$> getRandoms

-- | cryptographically secure random bytes from system source
instance MonadRandomBytes IO where
  randomBytes = getEntropy

instance MonadRandomBytes m => MonadRandomBytes (ReaderT r m) where
  randomBytes = lift . randomBytes
instance (Monoid w, MonadRandomBytes m) => MonadRandomBytes (WriterT w m) where
  randomBytes = lift . randomBytes
instance MonadRandomBytes m => MonadRandomBytes (StateT s m) where
  randomBytes = lift . randomBytes
instance (Monoid w, MonadRandomBytes m) => MonadRandomBytes (RWST r w s m) where
  randomBytes = lift . randomBytes

newNonce :: MonadRandomBytes m => m Nonce
newNonce = Key . fromJust . Sodium.decode <$> randomBytes Sodium.boxNonce

randomBinary :: MonadRandomBytes m => Get a -> Int -> m a
randomBinary g len = runGet g . fromStrict <$> randomBytes len

randomWord64 :: MonadRandomBytes m => m Word64
randomWord64 = randomBinary getWord64be 8

-- produces Int uniformly distributed in range [0,bound)
randomInt :: MonadRandomBytes m => Int -> m Int
randomInt bound | bound <= 1 = return 0
randomInt bound =
  let
    numBits = log2 bound
    numBytes = 1 + (numBits - 1 `div` 8)
  in do
    r <- (`mod` 2^numBits) . makeInt . unpack <$> randomBytes numBytes
    if r >= bound
      then randomInt bound
      else return r
  where
    log2 :: Int -> Int
    log2 = ceiling . logBase 2 . (fromIntegral :: Int -> Double)
    makeInt :: [Word8] -> Int
    makeInt = foldr (\w -> (fromIntegral w +) . (256*)) 0

-- produces Int uniformly distributed in range [low,high]
randomIntR :: MonadRandomBytes m => (Int,Int) -> m Int
randomIntR (low,high) = (low +) <$> randomInt (1 + high - low)

-- | produces uniformly random element of a list
uniform :: MonadRandomBytes m => [a] -> m a
uniform [] = error "empty list in uniform"
uniform as = (as!!) <$> randomInt (length as)

uniformSafe :: MonadRandomBytes m => [a] -> m (Maybe a)
uniformSafe [] = return Nothing
uniformSafe as = Just <$> uniform as

