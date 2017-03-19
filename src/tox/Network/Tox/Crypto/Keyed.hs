{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Monad class for caching of combined keys
module Network.Tox.Crypto.Keyed where

import           Control.Applicative            (Applicative, pure, (<*>))
import           Control.Monad                  (Monad)
import           Control.Monad.Random           (RandT, mapRandT)
import           Control.Monad.Reader           (ReaderT, mapReaderT)
import           Control.Monad.RWS              (RWST, mapRWST)
import           Control.Monad.State            (StateT, mapStateT)
import           Control.Monad.Trans            (lift)
import           Control.Monad.Writer           (WriterT, mapWriterT)
import           Data.Monoid                    (Monoid)

import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.Key         (CombinedKey, PublicKey,
                                                 SecretKey)
import           Network.Tox.Time               (Timestamp)

class (Monad m, Applicative m) => Keyed m where
  getCombinedKey :: SecretKey -> PublicKey -> m CombinedKey

instance Keyed m => Keyed (ReaderT r m) where
  getCombinedKey = (lift .) . getCombinedKey
instance (Monoid w, Keyed m) => Keyed (WriterT w m) where
  getCombinedKey = (lift .) . getCombinedKey
instance Keyed m => Keyed (StateT s m) where
  getCombinedKey = (lift .) . getCombinedKey
instance (Monoid w, Keyed m) => Keyed (RWST r w s m) where
  getCombinedKey = (lift .) . getCombinedKey
instance Keyed m => Keyed (RandT s m) where
  getCombinedKey = (lift .) . getCombinedKey

-- | trivial instance: the trivial monad, with no caching of keys
newtype NullKeyed a = NullKeyed { runNullKeyed :: a }
instance Functor NullKeyed where
  fmap f (NullKeyed x) = NullKeyed (f x)
instance Applicative NullKeyed where
  pure = NullKeyed
  (NullKeyed f) <*> (NullKeyed x) = NullKeyed (f x)
instance Monad NullKeyed where
  return = NullKeyed
  NullKeyed x >>= f = f x
instance Keyed NullKeyed where
  getCombinedKey = (NullKeyed .) . CombinedKey.precompute
