{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

-- | Abstraction layer for network functionality.
--
-- The intention is to
--   (i) separate the logic of the protocol from its binary encoding, and
--   (ii) allow a simulated network in place of actual network IO.
module Tox.Network.Core.Networked where

import           Control.Monad.Random             (RandT)
import           Control.Monad.Reader             (ReaderT)
import           Control.Monad.State              (MonadState, StateT)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Writer             (WriterT, execWriterT,
                                                   runWriterT, tell)
import           Data.Binary                      (Binary, encode)
import qualified Data.ByteString.Lazy             as LBS

import           Control.Monad.Logger             (LoggingT, MonadLogger (..),
                                                   NoLoggingT)
import           Tox.Core.Timed                   (Timed)
import           Tox.Crypto.Core.Keyed            (KeyedT)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)
import           Tox.Network.Core.NodeInfo        (NodeInfo)
import           Tox.Network.Core.Packet          (Packet (..), RawPayload (..))

class Monad m => Networked m where
  sendPacket :: (Binary payload, Show payload) => NodeInfo -> Packet payload -> m ()
  -- | Whether the link to the node holds more than it can take now, so data
  -- that can wait should.
  linkBusy :: NodeInfo -> m Bool
  linkBusy _ = return False

instance Networked m => Networked (KeyedT m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy

instance Networked m => Networked (LoggingT m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy

instance Networked m => Networked (NoLoggingT m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy

instance MonadLogger m => MonadLogger (RandT s m) where
  monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

-- | actual network IO
instance Networked (StateT NetworkState IO) where
  -- | TODO
  sendPacket _ _ = return ()
  linkBusy _ = return False

-- | TODO: sockets etc
type NetworkState = ()

data NetworkAction = SendPacket NodeInfo (Packet RawPayload)
  deriving (Show, Eq)

newtype NetworkLogged m a = NetworkLogged (WriterT [NetworkAction] m a)
  deriving (Monad, Applicative, Functor, MonadState s, MonadRandomBytes, Timed, MonadLogger)

runNetworkLogged :: Monad m => NetworkLogged m a -> m (a, [NetworkAction])
runNetworkLogged (NetworkLogged m) = runWriterT m
evalNetworkLogged :: (Monad m, Applicative m) => NetworkLogged m a -> m a
evalNetworkLogged = (fst <$>) . runNetworkLogged
execNetworkLogged :: Monad m => NetworkLogged m a -> m [NetworkAction]
execNetworkLogged (NetworkLogged m) = execWriterT m

-- | just log network events
instance Monad m => Networked (NetworkLogged m) where
  sendPacket to packet = NetworkLogged $
    let payloadBS = LBS.toStrict $ encode (packetPayload packet)
        rawPkt = Packet (packetKind packet) (RawPayload payloadBS)
    in tell [SendPacket to rawPkt]

instance Networked m => Networked (ReaderT r m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy
instance (Monoid w, Networked m) => Networked (WriterT w m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy
instance Networked m => Networked (RandT s m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy
instance Networked m => Networked (StateT s m) where
  sendPacket = (lift .) . sendPacket
  linkBusy = lift . linkBusy
