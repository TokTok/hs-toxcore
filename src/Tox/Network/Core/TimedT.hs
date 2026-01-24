{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StrictData                 #-}

module Tox.Network.Core.TimedT where

import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (ReaderT, ask, runReaderT)
import           Control.Monad.State              (MonadState)
import           Control.Monad.Trans              (MonadTrans)
import           Control.Monad.Writer             (MonadWriter)

import           Control.Monad.Logger             (MonadLogger)
import           Tox.Core.Time                    (Timestamp)
import           Tox.Core.Timed                   (Timed (..))
import           Tox.Crypto.Core.Keyed            (Keyed)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)
import           Tox.Network.Core.Networked       (Networked)

newtype TimedT m a = TimedT (ReaderT Timestamp m a)
  deriving (Monad, Applicative, Functor, MonadState s, MonadWriter w
    , MonadRandomBytes, MonadTrans, MonadIO, Networked, Keyed, MonadLogger)

runTimedT :: TimedT m a -> Timestamp -> m a
runTimedT (TimedT m) = runReaderT m

instance Monad m => Timed (TimedT m) where
  askTime = TimedT ask
