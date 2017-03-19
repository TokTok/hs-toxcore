{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Tox.Crypto.KeyedT where

import           Control.Applicative                  (Applicative, (<$>))
import           Control.Monad                        (Monad)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Reader                 (ReaderT, ask, local,
                                                       runReaderT)
import           Control.Monad.State                  (MonadState, StateT,
                                                       StateT (..), evalStateT,
                                                       gets, modify, runStateT,
                                                       state)
import           Control.Monad.Trans                  (MonadTrans)
import           Control.Monad.Writer                 (MonadWriter)

import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import qualified Network.Tox.Crypto.CombinedKey       as CombinedKey
import           Network.Tox.Crypto.Key               (CombinedKey, PublicKey,
                                                       SecretKey)
import           Network.Tox.Crypto.Keyed             (Keyed (..))
import           Network.Tox.Network.MonadRandomBytes (MonadRandomBytes)
import           Network.Tox.Network.Networked        (Networked)
import           Network.Tox.Timed                    (Timed)

type KeyRing = Map (SecretKey, PublicKey) CombinedKey

-- | caches computations of combined keys. Makes no attempt to delete old keys.
newtype KeyedT m a = KeyedT (StateT KeyRing m a)
  deriving (Monad, Applicative, Functor, MonadWriter w
    , MonadRandomBytes, MonadTrans, MonadIO, Networked, Timed)

runKeyedT :: Monad m => KeyedT m a -> KeyRing -> m (a, KeyRing)
runKeyedT (KeyedT m) = runStateT m

evalKeyedT :: Monad m => KeyedT m a -> KeyRing -> m a
evalKeyedT (KeyedT m) = evalStateT m

instance (MonadState s m, Applicative m) => MonadState s (KeyedT m) where
  state f = KeyedT . StateT $ \s -> flip (,) s <$> state f

instance (Monad m, Applicative m) => Keyed (KeyedT m) where
  getCombinedKey secretKey publicKey =
    let keys = (secretKey, publicKey)
    in KeyedT $ gets (Map.lookup keys) >>= \case
      Nothing ->
        let shared = CombinedKey.precompute secretKey publicKey
        in modify (Map.insert keys shared) >> return shared
      Just shared -> return shared
