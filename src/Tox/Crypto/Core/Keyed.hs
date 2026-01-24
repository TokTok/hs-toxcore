{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Monad class for caching of combined keys
module Tox.Crypto.Core.Keyed where

import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Random             (RandT)
import           Control.Monad.Reader             (ReaderT)
import           Control.Monad.RWS                (RWST)
import           Control.Monad.State              (MonadState, StateT (..),
                                                   evalStateT, gets, modify,
                                                   runStateT, state)
import           Control.Monad.Trans              (MonadTrans, lift)
import           Control.Monad.Writer             (MonadWriter, WriterT)

import           Control.Monad.Logger             (LoggingT, MonadLogger (..))
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Tox.Core.Timed                   (Timed)
import qualified Tox.Crypto.Core.CombinedKey      as CombinedKey
import           Tox.Crypto.Core.Key              (CombinedKey, PublicKey,
                                                   SecretKey)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)

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
instance Keyed m => Keyed (LoggingT m) where
  getCombinedKey sk pk = lift $ getCombinedKey sk pk

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

type KeyRing = Map (SecretKey, PublicKey) CombinedKey

-- | caches computations of combined keys. Makes no attempt to delete old keys.
newtype KeyedT m a = KeyedT (StateT KeyRing m a)
  deriving (Monad, Applicative, Functor, MonadWriter w
    , MonadRandomBytes, MonadTrans, MonadIO, Timed, MonadLogger)

runKeyedT :: Monad m => KeyedT m a -> KeyRing -> m (a, KeyRing)
runKeyedT (KeyedT m) = runStateT m

evalKeyedT :: Monad m => KeyedT m a -> KeyRing -> m a
evalKeyedT (KeyedT m) = evalStateT m

instance (MonadState s m, Applicative m) => MonadState s (KeyedT m) where
  state f = KeyedT . StateT $ \s -> (, s) <$> state f

instance (Monad m, Applicative m) => Keyed (KeyedT m) where
  getCombinedKey secretKey publicKey =
    let keys = (secretKey, publicKey)
    in KeyedT $ gets (Map.lookup keys) >>= \case
      Nothing ->
        let shared = CombinedKey.precompute secretKey publicKey
        in modify (Map.insert keys shared) >> return shared
      Just shared -> return shared
