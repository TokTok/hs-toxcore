{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Tox.Conduit.DHT where

import           Control.Monad                    (forever)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.State              (MonadState (..))
import           Control.Monad.Trans              (MonadTrans, lift)
import           Data.Binary                      (Binary)
import qualified Data.ByteString                  as BS
import           Data.Conduit                     (ConduitT, await, yield)

import           Tox.Core.Timed                   (Timed (..))
import           Tox.Crypto.Core.Keyed            (Keyed (..))
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes (..))
import           Tox.DHT.DhtState                 (DhtState)
import           Tox.DHT.Node                     (handleBootstrap,
                                                   handleIncomingPacket,
                                                   handleMaintenance)
import           Tox.DHT.Operation                (DhtNodeMonad)
import qualified Tox.Network.Core.Encoding        as Encoding
import           Tox.Network.Core.Networked       (Networked (..))
import           Tox.Network.Core.NodeInfo        (NodeInfo)
import           Tox.Network.Core.Packet          (Packet (..))

-- | A wrapper around 'ConduitT' to provide the necessary instances for DHT logic
-- without requiring orphan instances.
newtype DhtConduit i o m a = DhtConduit { unDhtConduit :: ConduitT i o m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadState s)

instance Timed m => Timed (DhtConduit i o m) where
    askTime = lift askTime

instance MonadRandomBytes m => MonadRandomBytes (DhtConduit i o m) where
    randomBytes = lift . randomBytes
    newKeyPair = lift newKeyPair

instance Keyed m => Keyed (DhtConduit i o m) where
    getCombinedKey sk pk = lift $ getCombinedKey sk pk

-- | The 'Networked' instance for 'DhtConduit' yields outgoing packets.
instance (Monad m) => Networked (DhtConduit i (NodeInfo, Packet BS.ByteString) m) where
    sendPacket to packet = DhtConduit $ yield (to, fmap Encoding.encode packet)

-- | 'DhtConduit' is a 'DhtNodeMonad' if the underlying monad 'm' provides state and other effects.
instance (Timed m, MonadRandomBytes m, MonadState DhtState m, Keyed m)
    => DhtNodeMonad (DhtConduit i (NodeInfo, Packet BS.ByteString) m) where
    getDhtState = get
    putDhtState = put
    handleDhtRequestPayload _ _ = return ()

-- | Conduit that handles incoming DHT packets.
dhtPacketHandler :: forall m. (Timed m, MonadRandomBytes m, MonadState DhtState m, Keyed m)
                 => ConduitT (NodeInfo, Packet BS.ByteString) (NodeInfo, Packet BS.ByteString) m ()
dhtPacketHandler = unDhtConduit $ forever $ do
    mInp <- DhtConduit await
    case mInp of
        Nothing          -> return ()
        Just (from, pkt) -> handleIncomingPacket from pkt

-- | Run maintenance operations within a conduit.
dhtMaintenanceLoop :: forall i m. (Timed m, MonadRandomBytes m, MonadState DhtState m, Keyed m)
                   => ConduitT i (NodeInfo, Packet BS.ByteString) m ()
dhtMaintenanceLoop = unDhtConduit handleMaintenance

-- | Bootstrap from a node within a conduit.
dhtBootstrapFrom :: forall i m. (Timed m, MonadRandomBytes m, MonadState DhtState m, Keyed m)
                 => NodeInfo -> ConduitT i (NodeInfo, Packet BS.ByteString) m ()
dhtBootstrapFrom node = unDhtConduit $ handleBootstrap node
