{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Tox.Timed where

import           Control.Monad        (Monad)
import           Control.Monad.Random (RandT, mapRandT)
import           Control.Monad.Reader (ReaderT, mapReaderT)
import           Control.Monad.RWS    (RWST, mapRWST)
import           Control.Monad.State  (StateT, mapStateT)
import           Control.Monad.Trans  (lift)
import           Control.Monad.Writer (WriterT, mapWriterT)
import           Data.Monoid          (Monoid)

import           Network.Tox.Time     (Timestamp)

-- |Essentially a synonym for MonadReader Timestamp
class Monad m => Timed m where
  askTime :: m Timestamp
  adjustTime :: (Timestamp -> Timestamp) -> m a -> m a

instance Timed m => Timed (ReaderT r m) where
  askTime = lift askTime
  adjustTime = mapReaderT . adjustTime
instance (Monoid w, Timed m) => Timed (WriterT w m) where
  askTime = lift askTime
  adjustTime = mapWriterT . adjustTime
instance Timed m => Timed (StateT s m) where
  askTime = lift askTime
  adjustTime = mapStateT . adjustTime
instance (Monoid w, Timed m) => Timed (RWST r w s m) where
  askTime = lift askTime
  adjustTime = mapRWST . adjustTime
instance Timed m => Timed (RandT s m) where
  askTime = lift askTime
  adjustTime = mapRandT . adjustTime
