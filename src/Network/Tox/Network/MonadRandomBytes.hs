{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}

module Network.Tox.Network.MonadRandomBytes where

import           Control.Monad.RWS          (RWST)
import           Control.Monad.Random       (RandT, getRandoms)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.State        (StateT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Writer       (WriterT)
import           Data.Binary                (get)
import           Data.Binary.Get            (Get, getWord16be, getWord32be,
                                             getWord64be, getWord8, runGet)
import           Data.ByteString            (ByteString, pack, unpack)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Proxy                 (Proxy (..))
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           System.Entropy             (getEntropy)
import           System.Random              (RandomGen)


import           Network.Tox.Crypto.Key     (Key)
import qualified Network.Tox.Crypto.Key     as Key
import           Network.Tox.Crypto.KeyPair (KeyPair)
import qualified Network.Tox.Crypto.KeyPair as KeyPair

class (Monad m, Applicative m) => MonadRandomBytes m where
  randomBytes :: Int -> m ByteString

  newKeyPair :: m KeyPair
  newKeyPair = KeyPair.fromSecretKey <$> randomKey

instance (Monad m, Applicative m, RandomGen s) => MonadRandomBytes (RandT s m) where
  randomBytes n = pack . take n <$> getRandoms

-- | cryptographically secure random bytes from system source
instance MonadRandomBytes IO where
  randomBytes = getEntropy
  newKeyPair = KeyPair.newKeyPair

instance MonadRandomBytes m => MonadRandomBytes (ReaderT r m) where
  randomBytes = lift . randomBytes
  newKeyPair = lift newKeyPair
instance (Monoid w, MonadRandomBytes m) => MonadRandomBytes (WriterT w m) where
  randomBytes = lift . randomBytes
  newKeyPair = lift newKeyPair
instance MonadRandomBytes m => MonadRandomBytes (StateT s m) where
  randomBytes = lift . randomBytes
  newKeyPair = lift newKeyPair
instance (Monoid w, MonadRandomBytes m) => MonadRandomBytes (RWST r w s m) where
  randomBytes = lift . randomBytes
  newKeyPair = lift newKeyPair

randomBinary :: MonadRandomBytes m => Get a -> Int -> m a
randomBinary g len = runGet g . fromStrict <$> randomBytes len

randomKey :: forall m a. (MonadRandomBytes m, Key.CryptoNumber a) => m (Key a)
randomKey = randomBinary get $ Key.encodedByteSize (Proxy :: Proxy a)

randomNonce :: MonadRandomBytes m => m Key.Nonce
randomNonce = randomKey

randomWord64 :: MonadRandomBytes m => m Word64
randomWord64 = randomBinary getWord64be 8
randomWord32 :: MonadRandomBytes m => m Word32
randomWord32 = randomBinary getWord32be 4
randomWord16 :: MonadRandomBytes m => m Word16
randomWord16 = randomBinary getWord16be 2
randomWord8 :: MonadRandomBytes m => m Word8
randomWord8 = randomBinary getWord8 1

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
