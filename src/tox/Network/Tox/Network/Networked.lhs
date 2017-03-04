\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}

-- | abstraction layer for network functionality.
-- The intention is to
--   (i) separate the logic of the protocol from its binary encoding, and
--   (ii) allow a simulated network in place of actual network IO.
module Network.Tox.Network.Networked where

import           Control.Monad                     (guard, replicateM, void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Random              (RandT, getRandom)
import           Control.Monad.Reader              (ReaderT, ask, runReaderT)
import           Control.Monad.State               (State, StateT, execStateT,
                                                    gets, modify)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)
import           Control.Monad.Writer              (WriterT, tell)
import qualified Crypto.Saltine.Class              as Sodium (decode)
import qualified Crypto.Saltine.Internal.ByteSizes as Sodium (boxNonce)
import           Data.Binary                       (Binary)
import           Data.ByteString                   (ByteString, pack)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromJust)
import           System.Random                     (Random, StdGen, randomIO)

import           Network.Tox.Crypto.Key            (Key (..), Nonce, PublicKey)
import qualified Network.Tox.Crypto.Nonce          as Nonce
import           Network.Tox.DHT.DhtState          (DhtState)
import           Network.Tox.NodeInfo.NodeInfo     (NodeInfo)
import           Network.Tox.Protocol.Packet       (Packet (..))

class Monad m => Networked m where
  sendPacket :: NodeInfo -> Packet -> m ()

  randomBytes :: Int -> m ByteString

  newNonce :: m Nonce
  newNonce = Nonce <$> randomBytes Sodium.boxNonce

-- | actual network IO
instance Networked (StateT NetworkState IO) where
  -- | TODO
  sendPacket _ _ = return ()

  newNonce = Nonce.newNonce

  -- | FIXME TODO XXX TODO FIXME: not cryptographically secure!
  -- c-toxcore uses sodium for this, but annoyingly the saltine package
  -- doesn't expose the corresponding function.
  randomBytes n = pack <$> replicateM n randomIO

-- | TODO: sockets etc
type NetworkState = ()

type NetworkEvent = String
type NetworkLogged m = RandT StdGen (WriterT [NetworkEvent] m)
-- | just log network events
instance Monad m => Networked (NetworkLogged m) where
  sendPacket to packet = tell [">>> " + to ++ " : " ++ show packet]
  randomBytes n = pack <$> replicateM n getRandom

{- TODO: simulated network with many nodes, with no IO.
Note: would be nice if we could also avoid expensive encryption, but I don't see
how to arrange this without unduely complicating the architecture.

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
  randomBytes = lift . randomBytes
  newNonce = lift randomBytes
instance (Monoid w, Networked m) => Networked (WriterT w m) where
  sendPacket = (lift .) . sendPacket
  randomBytes = lift . randomBytes
  newNonce = lift newNonce
instance Networked m => Networked (RandT s m) where
  sendPacket = (lift .) . sendPacket
  randomBytes = lift . randomBytes
  newNonce = lift newNonce
instance Networked m => Networked (StateT s m) where
  sendPacket = (lift .) . sendPacket
  randomBytes = lift . randomBytes
  newNonce = lift newNonce

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
