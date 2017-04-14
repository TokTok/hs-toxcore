{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Tox.Timed where

import           Control.Monad        (Monad)
import           Control.Monad.Random (RandT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.RWS    (RWST)
import           Control.Monad.State  (StateT)
import           Control.Monad.Trans  (lift)
import           Control.Monad.Writer (WriterT)
import           Data.Monoid          (Monoid)

import           Network.Tox.Time     (Timestamp)

class Monad m => Timed m where
  askTime :: m Timestamp

instance Timed m => Timed (ReaderT r m) where
  askTime = lift askTime
instance (Monoid w, Timed m) => Timed (WriterT w m) where
  askTime = lift askTime
instance Timed m => Timed (StateT s m) where
  askTime = lift askTime
instance (Monoid w, Timed m) => Timed (RWST r w s m) where
  askTime = lift askTime
instance Timed m => Timed (RandT s m) where
  askTime = lift askTime
