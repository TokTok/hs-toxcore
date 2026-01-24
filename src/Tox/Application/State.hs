{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StrictData                 #-}
module Tox.Application.State where

import           Control.Monad.State              (MonadState (..), gets,
                                                   modify)
import           Tox.Core.Time                    (Timestamp, getTime)
import qualified Tox.Crypto.Core.KeyPair          as KeyPair
import           Tox.Crypto.Core.KeyPair          (KeyPair)
import qualified Tox.DHT.DhtState                 as DhtState
import           Tox.DHT.DhtState                 (DhtState)
import qualified Tox.Onion.Operation              as Onion
import           Tox.Onion.Operation              (OnionState)
import qualified Tox.Session.Connection           as Connection
import           Tox.Session.Connection           (ConnectionManager)
import qualified Tox.Session.Friend               as Messenger
import           Tox.Session.Friend               (Messenger)

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader, ReaderT, ask,
                                                   runReaderT)
import qualified Data.Binary                      as Binary
import qualified Data.ByteString.Lazy             as LBS
import           Data.IORef                       (IORef, readIORef, writeIORef)
import qualified Network.Socket                   as Socket
import qualified Network.Socket.ByteString        as SocketBS
import           Tox.Conduit.Network              (toSockAddr)
import           Tox.Core.Timed                   (Timed (..))
import           Tox.Crypto.Core.Keyed            (Keyed (..), KeyedT)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes (..))
import           Tox.DHT.Operation                (DhtNodeMonad (..))
import           Tox.Network.Core.Networked       (Networked (..))
import           Tox.Network.Core.NodeInfo        (NodeInfo (..))
import           Tox.Network.Core.Packet          (Packet (..))
import qualified Tox.Onion.Operation              as Onion
import qualified Tox.Session.Connection           as Connection
import qualified Tox.Session.Friend               as Messenger

data GlobalState = GlobalState
  { dhtState    :: DhtState
  , onionState  :: OnionState
  , connManager :: ConnectionManager
  , messenger   :: Messenger
  , udpSocket   :: Socket.Socket
  }

initState :: Timestamp -> KeyPair -> KeyPair -> Socket.Socket -> GlobalState
initState now dhtKeys realKeys sock = GlobalState
  { dhtState    = DhtState.empty now dhtKeys
  , onionState  = Onion.initState realKeys
  , connManager = Connection.initManager realKeys dhtKeys
  , messenger   = Messenger.initMessenger
  , udpSocket   = sock
  }

-- | Unified application monad.
newtype AppMonad m a = AppMonad { unAppMonad :: ReaderT (IORef GlobalState) (KeyedT m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadRandomBytes, Keyed, MonadReader (IORef GlobalState))

instance MonadIO m => Timed (AppMonad m) where
    askTime = liftIO getTime

-- | Use the underlying IORef for the main MonadState.
instance (MonadIO m, MonadRandomBytes m) => MonadState GlobalState (AppMonad m) where
    get = ask >>= liftIO . readIORef
    put s = ask >>= liftIO . flip writeIORef s

-- | Instance for DHT layer.
instance (MonadIO m, MonadRandomBytes m) => DhtNodeMonad (AppMonad m) where
    getDhtState = gets dhtState
    putDhtState s = modify $ \gs -> gs { dhtState = s }
    handleDhtRequestPayload from dhtPkt = do
        Onion.onDhtRequestPayload from dhtPkt

-- | Instance for Onion layer.
instance (MonadIO m, MonadRandomBytes m) => Onion.OnionNodeMonad (AppMonad m) where
    getOnionState = gets onionState
    putOnionState s = modify $ \gs -> gs { onionState = s }
    getDhtPublicKey = gets (KeyPair.publicKey . DhtState.dhtKeyPair . dhtState)

-- | Instance for Connection layer.
instance (MonadIO m, MonadRandomBytes m) => Connection.ConnectionMonad (AppMonad m) where
    getConnManager = gets connManager
    putConnManager s = modify $ \gs -> gs { connManager = s }

-- | Instance for Messenger layer.
instance (MonadIO m, MonadRandomBytes m) => Messenger.MessengerMonad (AppMonad m) where
    getMessenger = gets messenger
    putMessenger s = modify $ \gs -> gs { messenger = s }

-- | Implement Networked to actually send UDP packets.
instance (MonadIO m, MonadRandomBytes m) => Networked (AppMonad m) where
    sendPacket ni packet = do
        sock <- gets udpSocket
        let addr = toSockAddr (address ni)
            bs = LBS.toStrict $ Binary.encode packet
        liftIO $ void $ SocketBS.sendTo sock bs addr
        where
          void = fmap (const ())
