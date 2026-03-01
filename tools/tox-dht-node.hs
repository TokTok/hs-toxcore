{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import           Control.Concurrent                 (forkIO, threadDelay)
import           Control.Monad                      (forM, forever, void, when)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (MonadReader, ReaderT, ask,
                                                     runReaderT)
import           Control.Monad.State                (MonadState (..))
import           Control.Monad.Trans                (lift)
import           Control.Monad.Trans.Resource       (runResourceT)
import           Data.Aeson                         (FromJSON, decode,
                                                     parseJSON, withObject,
                                                     (.:), (.:?))
import qualified Data.Binary                        as Binary
import           Data.Binary.Get                    (runGetOrFail)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base16             as Base16
import qualified Data.ByteString.Char8              as BS8
import qualified Data.ByteString.Lazy               as LBS
import           Data.Conduit                       (ConduitT, await,
                                                     awaitForever, runConduit,
                                                     yield, (.|))
import qualified Data.Conduit.List                  as CL
import           Data.IORef                         (IORef, newIORef, readIORef,
                                                     writeIORef)
import qualified Data.IP                            as IP
import           Data.Maybe                         (catMaybes)
import           GHC.Generics                       (Generic)
import           Network.HTTP.Simple                (getResponseBody, httpLBS,
                                                     parseRequest)
import qualified Network.Socket                     as Socket
import           System.IO                          (BufferMode (..),
                                                     hSetBuffering, stdout)
import           Text.Read                          (readMaybe)

import qualified Crypto.Saltine.Class               as Sodium
import qualified Data.Map                           as Map
import           Tox.Conduit.DHT                    (DhtConduit (..),
                                                     dhtBootstrapFrom,
                                                     dhtMaintenanceLoop,
                                                     dhtPacketHandler)
import           Tox.Conduit.Encoding               (decodePacket, encodePacket)
import           Tox.Conduit.Network                (fromSockAddr, udpSink,
                                                     udpSource)
import           Tox.Core.Time                      (getTime)
import           Tox.Core.Timed                     (Timed (..))
import qualified Tox.Crypto.Core.Key                as Key
import           Tox.Crypto.Core.Key                (PublicKey)
import           Tox.Crypto.Core.Keyed              (Keyed (..), KeyedT,
                                                     evalKeyedT)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..))
import           Tox.DHT.DhtPacket                  (DhtPacket (..))
import qualified Tox.DHT.DhtState                   as DhtState
import           Tox.DHT.DhtState                   (DhtState)
import           Tox.DHT.Operation                  (DhtNodeMonad (..), initDht)
import           Tox.Network.Core.HostAddress       (HostAddress (..))

import           Tox.Network.Core.Networked         (Networked (..))
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..))
import           Tox.Network.Core.PortNumber        (PortNumber (..))
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TimedT            (runTimedT)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))

data ToxNodeJSON = ToxNodeJSON
    { jsonIpv4      :: Maybe String
    , jsonIpv6      :: Maybe String
    , jsonPort      :: Int
    , jsonPublicKey :: String
    } deriving (Show, Generic)

instance FromJSON ToxNodeJSON where
    parseJSON = withObject "ToxNodeJSON" $ \v -> ToxNodeJSON
        <$> v .:? "ipv4"
        <*> v .:? "ipv6"
        <*> v .: "port"
        <*> v .: "public_key"

data NodesList = NodesList
    { nodes :: [ToxNodeJSON]
    } deriving (Show, Generic)

instance FromJSON NodesList

parsePublicKey :: String -> Maybe PublicKey
parsePublicKey s = do
    let bs = BS8.pack s
    case Base16.decode bs of
        Left _        -> Nothing
        Right decoded -> Key.Key <$> Sodium.decode decoded

resolveNode :: ToxNodeJSON -> IO [NodeInfo]
resolveNode j = do
    let mPk = parsePublicKey (jsonPublicKey j)
        port = fromIntegral (jsonPort j)
    case mPk of
        Nothing -> return []
        Just pk -> do
            let addrs = catMaybes [jsonIpv4 j]
            fmap concat $ forM addrs $ \addrStr -> do
                if addrStr == "-" then return []
                else do
                    case (readMaybe addrStr :: Maybe IP.IPv4) of
                        Just ip -> return [NodeInfo UDP (SocketAddress (IPv4 (IP.toHostAddress ip)) port) pk]
                        Nothing -> do
                            let hints = Socket.defaultHints { Socket.addrSocketType = Socket.Datagram, Socket.addrFamily = Socket.AF_INET }
                            res <- Socket.getAddrInfo (Just hints) (Just addrStr) (Just $ show (jsonPort j))
                            return $ catMaybes $ flip map res $ \ai -> do
                                sa <- fromSockAddr (Socket.addrAddress ai)
                                return $ NodeInfo UDP sa pk

newtype DhtApp m a = DhtApp { unDhtApp :: ReaderT (IORef DhtState) (KeyedT m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadRandomBytes, Keyed, MonadReader (IORef DhtState))

instance (MonadIO m, MonadRandomBytes m) => MonadState DhtState (DhtApp m) where
    get = ask >>= liftIO . readIORef
    put s = do
        old <- get
        when (DhtState.size old /= DhtState.size s) $
            liftIO $ putStrLn $ "DHT Size changed: " ++ show (DhtState.size s)
        ask >>= liftIO . flip writeIORef s

instance MonadIO m => Timed (DhtApp m) where
    askTime = liftIO getTime

instance (MonadIO m, MonadRandomBytes m) => DhtNodeMonad (DhtApp m) where
    getDhtState = get
    putDhtState = put
    handleDhtRequestPayload _ _ = return ()

instance (Monad m) => Networked (DhtApp m) where
    sendPacket _ _ = return ()

main :: IO ()
main = runResourceT $ do
    liftIO $ hSetBuffering stdout LineBuffering
    liftIO $ putStrLn "Fetching nodes from nodes.tox.chat..."
    req <- parseRequest "https://nodes.tox.chat/json"
    res <- httpLBS req
    let mNodes = decode (getResponseBody res) :: Maybe NodesList
    allNodes <- case mNodes of
        Nothing -> do
            liftIO $ putStrLn "Failed to parse nodes JSON"
            return []
        Just nl -> liftIO $ fmap concat $ mapM resolveNode (take 30 $ nodes nl)

    liftIO $ putStrLn $ "Resolved " ++ show (length allNodes) ++ " nodes"

    sock <- liftIO $ Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    liftIO $ Socket.bind sock (Socket.SockAddrInet 0 0)

    now <- liftIO getTime
    initialState <- liftIO $ runTimedT initDht now
    stateRef <- liftIO $ newIORef initialState

    liftIO $ putStrLn "Starting DHT node..."

    let runStack m = evalKeyedT (runReaderT (unDhtApp m) stateRef) Map.empty

    -- Bridge conduit to extract sender public key from DhtPacket
    let dhtBridge :: MonadIO m => ConduitT (Socket.SockAddr, Packet BS8.ByteString) (NodeInfo, Packet BS8.ByteString) m ()
        dhtBridge = forever $ do
            mInp <- await
            case mInp of
                Nothing -> return ()
                Just (addr, pkt) -> do
                    case runGetOrFail Binary.get (LBS.fromStrict (packetPayload pkt)) of
                        Right (_, _, dhtPkt :: DhtPacket) -> do
                            case fromSockAddr addr of
                                Just sa -> yield (NodeInfo UDP sa (senderPublicKey dhtPkt), pkt)
                                Nothing -> return ()
                        Left _ -> return ()

    let encodePkt :: MonadIO m => ConduitT (NodeInfo, Packet BS8.ByteString) (Socket.SockAddr, BS8.ByteString) m ()
        encodePkt = encodePacket

    -- Start packet handler in background
    void $ liftIO $ forkIO $ runStack $ runConduit $
        udpSource sock .| decodePacket .| dhtBridge .| dhtPacketHandler .| encodePkt .| udpSink sock

    -- Maintenance thread
    void $ liftIO $ forkIO $ runStack $ forever $ do
        runConduit $ dhtMaintenanceLoop .| encodePkt .| udpSink sock
        liftIO $ threadDelay 20000000 -- 20 seconds

    -- Initial bootstrap
    liftIO $ runStack $ runConduit $
        CL.sourceList allNodes
        .| CL.mapM (\n -> liftIO (putStrLn $ "Bootstrapping from " ++ show n) >> return n)
        .| awaitForever dhtBootstrapFrom .| encodePkt .| udpSink sock

    liftIO $ putStrLn "Entering main loop..."

    liftIO $ forever $ do
        s <- readIORef stateRef
        putStrLn $ "DHT Size: " ++ show (DhtState.size s)
        threadDelay 1000000 -- 1 second
