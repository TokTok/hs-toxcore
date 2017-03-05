\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

-- | abstraction layer for network functionality.
-- The intention is to
--   (i) separate the logic of the protocol from its binary encoding, and
--   (ii) allow a simulated network in place of actual network IO.
module Network.Tox.Network.Networked where

import           Control.Monad                 (guard, replicateM, void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Random          (RandT)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.State           (State, StateT, execStateT, gets,
                                                modify)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe     (MaybeT (..), runMaybeT)
import           Control.Monad.Writer          (WriterT, tell)
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           System.Random                 (Random, StdGen, randomIO)

import           Network.Tox.DHT.DhtState      (DhtState)
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import           Network.Tox.Protocol.Packet   (Packet (..))

class Monad m => Networked m where
  sendPacket :: (Binary payload, Show payload) => NodeInfo -> Packet payload -> m ()
-- | actual network IO
instance Networked (StateT NetworkState IO) where
  -- | TODO
  sendPacket _ _ = return ()

-- | TODO: sockets etc
type NetworkState = ()

type NetworkEvent = String
type NetworkLogged m = RandT StdGen (WriterT [NetworkEvent] m)
-- | just log network events
instance Monad m => Networked (NetworkLogged m) where
  sendPacket to packet = tell [">>> " ++ show to ++ " : " ++ show packet]

instance Networked m => Networked (ReaderT r m) where
  sendPacket = (lift .) . sendPacket
instance (Monoid w, Networked m) => Networked (WriterT w m) where
  sendPacket = (lift .) . sendPacket
instance Networked m => Networked (RandT s m) where
  sendPacket = (lift .) . sendPacket
instance Networked m => Networked (StateT s m) where
  sendPacket = (lift .) . sendPacket

{- Earlier attempt:
newtype NetworkIOT m a = NetworkIOT { runNetworkIOT :: m a }
instance MonadTrans NetworkIOT where
  lift = NetworkIOT
instance Monad m => Monad (NetworkIOT m) where
  (NetworkIOT m) >>= f = m >>= runNetworkIOT . f
instance Monad m => Applicative (NetworkIOT m) where
  pure = return
  (<*>) = ap
instance Monad m => Functor (NetworkIOT m) where
  fmap f x = pure f <*> x
instance MonadIO m => MonadIO (NetworkIOT m) where
  liftIO = lift . liftIO
instance MonadIO m => Networked (NetworkIOT m) where
  sendPacket _ = liftIO $ return () -- TODO
-}

\end{code}
