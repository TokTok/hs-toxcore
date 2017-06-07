{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Trustworthy                #-}

module Network.Tox.TimedT where

import           Control.Applicative                  (Applicative)
import           Control.Monad                        (Monad)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Reader                 (ReaderT, ask, local,
                                                       runReaderT)
import           Control.Monad.State                  (MonadState)
import           Control.Monad.Trans                  (MonadTrans)
import           Control.Monad.Writer                 (MonadWriter)

import           Network.Tox.Crypto.Keyed             (Keyed)
import           Network.Tox.Network.MonadRandomBytes (MonadRandomBytes)
import           Network.Tox.Network.Networked        (Networked)
import           Network.Tox.Time                     (Timestamp)
import           Network.Tox.Timed                    (Timed (..))

newtype TimedT m a = TimedT (ReaderT Timestamp m a)
  deriving (Monad, Applicative, Functor, MonadState s, MonadWriter w
    , MonadRandomBytes, MonadTrans, MonadIO, Networked, Keyed)

runTimedT :: TimedT m a -> Timestamp -> m a
runTimedT (TimedT m) = runReaderT m

instance Monad m => Timed (TimedT m) where
  askTime = TimedT ask
