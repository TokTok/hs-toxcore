{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tox.DHT.Node where

import           Control.Monad.State         (gets)
import           Data.Binary                 (Binary)
import qualified Data.ByteString             as BS
import           Data.Foldable               (forM_)
import           Tox.DHT.DhtPacket           as DhtPacket
import           Tox.DHT.DhtRequestPacket    (DhtRequestPacket)
import           Tox.DHT.DhtState            as DhtState
import           Tox.DHT.Operation           (DhtNodeMonad (..), bootstrapNode,
                                              doDHT, getsDht,
                                              handleDhtRequestPacket,
                                              handleNodesRequest,
                                              handleNodesResponse,
                                              handlePingRequest,
                                              handlePingResponse)
import qualified Tox.Network.Core.Encoding   as Encoding
import           Tox.Network.Core.NodeInfo   (NodeInfo (..))
import           Tox.Network.Core.Packet     (Packet (..))
import           Tox.Network.Core.PacketKind as PacketKind

-- | A unified packet handler for DHT protocol packets.
-- Decrypts the DHT envelope and dispatches to the appropriate handler.
handleIncomingPacket :: forall m. DhtNodeMonad m => NodeInfo -> Packet BS.ByteString -> m ()
handleIncomingPacket from (Packet kind payload) = do
    kp <- getsDht DhtState.dhtKeyPair
    let decodeDht :: forall a. Binary a => (NodeInfo -> a -> m ()) -> m ()
        decodeDht handler = case Encoding.decode payload of
            Nothing -> return ()
            Just dhtPacket -> do
                mDecoded <- DhtPacket.decodeKeyed kp dhtPacket
                forM_ mDecoded (handler from)
    case kind of
        PacketKind.PingRequest   -> decodeDht handlePingRequest
        PacketKind.PingResponse  -> decodeDht handlePingResponse
        PacketKind.NodesRequest  -> decodeDht handleNodesRequest
        PacketKind.NodesResponse -> decodeDht handleNodesResponse
        PacketKind.Crypto        -> case Encoding.decode payload of
            Nothing -> return ()
            Just (dhtReq :: DhtRequestPacket) -> handleDhtRequestPacket from dhtReq
        _ -> return ()

-- | Periodic maintenance for the DHT node.
handleMaintenance :: DhtNodeMonad m => m ()
handleMaintenance = doDHT

-- | Bootstrap from a known node.
handleBootstrap :: DhtNodeMonad m => NodeInfo -> m ()
handleBootstrap = bootstrapNode
