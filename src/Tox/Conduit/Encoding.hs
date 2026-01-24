{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Tox.Conduit.Encoding where

import           Control.Monad             (forever)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Binary               (encode, get)
import           Data.Binary.Get           (runGetOrFail)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Conduit              (ConduitT, await, yield)
import qualified Network.Socket            as Socket

import           Tox.Conduit.Network       (toSockAddr)
import           Tox.Network.Core.NodeInfo (NodeInfo (..))
import           Tox.Network.Core.Packet   (Packet (..), RawPayload (..))

-- | Decodes raw UDP packets into Tox packets, keeping the sender's address.
decodePacket :: MonadIO m
             => ConduitT (Socket.SockAddr, BS.ByteString) (Socket.SockAddr, Packet RawPayload) m ()
decodePacket = forever $ do
    mInp <- await
    case mInp of
        Nothing -> return ()
        Just (addr, bs) -> do
            liftIO $ putStrLn $ "Raw packet from " ++ show addr ++ " (" ++ show (BS.length bs) ++ " bytes)"
            case runGetOrFail get (LBS.fromStrict bs) of
                Right (_, _, pkt) -> yield (addr, pkt)
                Left _ -> liftIO $ putStrLn "Failed to decode packet kind"

-- | Encodes Tox packets into raw UDP packets.
encodePacket :: MonadIO m
             => ConduitT (NodeInfo, Packet RawPayload) (Socket.SockAddr, BS.ByteString) m ()
encodePacket = forever $ do
    mInp <- await
    case mInp of
        Nothing -> return ()
        Just (ni, pkt) -> do
            let addr = toSockAddr (address ni)
            let bs = LBS.toStrict $ encode pkt
            liftIO $ putStrLn $ "Sending packet to " ++ show addr ++ " (" ++ show (BS.length bs) ++ " bytes)"
            yield (addr, bs)
