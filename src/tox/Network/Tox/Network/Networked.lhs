\begin{code}
{-# LANGUAGE FlexibleInstances #-}

-- | abstraction layer for network functionality.
-- The intention is to
--   (i) separate the logic of the protocol from its binary encoding, and
--   (ii) allow a simulated network in place of actual network IO.
module Network.Tox.Network.Networked where

import           Control.Monad.Random          (RandT)
import           Control.Monad.Reader          (ReaderT)
import           Control.Monad.State           (StateT)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Writer          (WriterT, tell)
import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)

-- | Information required, along with KeyPairs and NodeInfos for the
-- communicating nodes, to build a packet.
-- TODO: should be a class? With e.g. NodesRequest and NodesResponse as instances?
data PacketInfo
  = NodesRequestInfo
    { requestSearch :: PublicKey
    }
  | NodesResponseInfo
    { responseNodes :: [NodeInfo]
    }
  deriving (Eq, Ord, Show)

class Monad m => Networked m where
  sendPacket :: NodeInfo -> PacketInfo -> m ()

-- | actual network IO
instance Networked (StateT NetworkState IO) where
  -- | TODO: build packet from info and send
  sendPacket _ _ = return ()
-- | TODO: sockets etc
type NetworkState = ()

type NetworkEvent = String
type NetworkLogged = WriterT [NetworkEvent]
-- | just log network events
instance Monad m => Networked (NetworkLogged m) where
  sendPacket to packet = tell [">>> " + to ++ " : " ++ show request]

{- TODO: simulated network with many nodes, with no IO and no actual encryption.
Something like:
instance Networked SimulatedNetwork where
  sendPacket to packet = do
    randomUDPLoss >>= guard . not
    randomUDPDelay >>= pause
    us <- tellCurrentNode
    atNode to $ handlePacket us packet
-}

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
