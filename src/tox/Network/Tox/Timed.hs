{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Tox.Timed where

import           Control.Monad                        (Monad)
import           Control.Monad.Random                 (RandT, mapRandT)
import           Control.Monad.Reader                 (ReaderT, ask, local,
                                                       mapReaderT, runReaderT)
import           Control.Monad.RWS                    (RWST, mapRWST)
import           Control.Monad.State                  (MonadState, StateT,
                                                       mapStateT)
import           Control.Monad.Trans                  (MonadTrans, lift)
import           Control.Monad.Writer                 (WriterT, mapWriterT)
import           Data.Monoid                          (Monoid)

import           Network.Tox.Network.MonadRandomBytes (MonadRandomBytes,
                                                       randomBytes)
import           Network.Tox.Time                     (Timestamp)

class Monad m => Timed m where
  askTime :: m Timestamp
  adjustTime :: (Timestamp -> Timestamp) -> m a -> m a

newtype TimedT m a = TimedT (ReaderT Timestamp m a)
  deriving (Monad, Functor, Applicative, MonadState s, MonadRandomBytes, MonadTrans)

runTimedT :: Monad m => TimedT m a -> Timestamp -> m a
runTimedT (TimedT m) = runReaderT m


instance Monad m => Timed (TimedT m) where
  askTime = TimedT ask
  adjustTime f (TimedT m) = TimedT $ local f m

instance Timed m => Timed (ReaderT r m) where
  askTime = lift askTime
  adjustTime = mapReaderT . adjustTime
instance (Monoid w, Timed m) => Timed (WriterT w m) where
  askTime = lift askTime
  adjustTime = mapWriterT . adjustTime
instance (Monoid w, Timed m) => Timed (RWST r w s m) where
  askTime = lift askTime
  adjustTime = mapRWST . adjustTime
instance Timed m => Timed (RandT s m) where
  askTime = lift askTime
  adjustTime = mapRandT . adjustTime
instance Timed m => Timed (StateT s m) where
  askTime = lift askTime
  adjustTime = mapStateT . adjustTime
