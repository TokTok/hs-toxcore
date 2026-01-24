{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Tox.Conduit.Network where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import qualified Data.ByteString                as BS
import           Data.Conduit                   (ConduitT, yield)
import qualified Data.Conduit.List              as CL
import qualified Network.Socket                 as Socket
import qualified Network.Socket.ByteString      as SocketBS

import           Tox.Network.Core.HostAddress   (HostAddress (..))
import           Tox.Network.Core.PortNumber    (PortNumber (..))
import           Tox.Network.Core.SocketAddress (SocketAddress (..))

-- | Convert Tox 'SocketAddress' to 'Socket.SockAddr'.
toSockAddr :: SocketAddress -> Socket.SockAddr
toSockAddr (SocketAddress (IPv4 addr) (PortNumber port)) =
    Socket.SockAddrInet (fromIntegral port) addr
toSockAddr (SocketAddress (IPv6 addr) (PortNumber port)) =
    Socket.SockAddrInet6 (fromIntegral port) 0 addr 0

-- | An address for a dual-stack IPv6 socket: IPv4 addresses are mapped.
toSockAddr6 :: SocketAddress -> Socket.SockAddr
toSockAddr6 (SocketAddress (IPv4 addr) (PortNumber port)) =
    Socket.SockAddrInet6 (fromIntegral port) 0 (0, 0, 0xffff, v4) 0
  where
    (a, b, c, d) = Socket.hostAddressToTuple addr
    v4 = foldl (\acc o -> acc * 256 + fromIntegral o) 0 [a, b, c, d]
toSockAddr6 sa = toSockAddr sa

-- | Convert 'Socket.SockAddr' to Tox 'SocketAddress'.  IPv4 addresses mapped
-- into IPv6 become IPv4 addresses.
fromSockAddr :: Socket.SockAddr -> Maybe SocketAddress
fromSockAddr (Socket.SockAddrInet port addr) =
    Just $ SocketAddress (IPv4 addr) (fromIntegral port)
fromSockAddr (Socket.SockAddrInet6 port _ (0, 0, 0xffff, v4) _) =
    Just $ SocketAddress (IPv4 $ Socket.tupleToHostAddress (byte 24, byte 16, byte 8, byte 0)) (fromIntegral port)
  where
    byte n = fromIntegral (v4 `div` (2 ^ (n :: Int)))
fromSockAddr (Socket.SockAddrInet6 port _ addr _) =
    Just $ SocketAddress (IPv6 addr) (fromIntegral port)
fromSockAddr _ = Nothing

-- | A conduit source that reads from a UDP socket.
udpSource :: MonadIO m => Socket.Socket -> ConduitT i (Socket.SockAddr, BS.ByteString) m ()
udpSource sock = do
    (bs, addr) <- liftIO $ SocketBS.recvFrom sock 4096
    yield (addr, bs)
    udpSource sock

-- | A conduit sink that writes to a UDP socket.
udpSink :: MonadIO m => Socket.Socket -> ConduitT (Socket.SockAddr, BS.ByteString) o m ()
udpSink sock = CL.mapM_ $ \(addr, bs) ->
    liftIO $ void $ SocketBS.sendTo sock bs addr
