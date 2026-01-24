{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
module Tox.Application.State where

import           Control.Exception                  (IOException, try)
import           Control.Monad                      (forM_, when)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Logger               (LoggingT, MonadLogger,
                                                     logDebugN)
import           Control.Monad.Reader               (MonadReader, ReaderT, ask,
                                                     asks, runReaderT)
import           Control.Monad.State                (MonadState (..), gets,
                                                     modify)
import           Control.Monad.Trans                (lift)
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Put                    as Put
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.IORef                         (IORef, readIORef,
                                                     writeIORef)
import           Data.List                          (partition)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Text                          (pack)
import           GHC.IO.Exception                   (IOErrorType (..), ioe_type)
import qualified Network.Socket                     as Socket
import qualified Network.Socket.ByteString          as SocketBS

import qualified Tox.Application.GroupChat          as Group
import qualified Tox.Application.LegacyGroup        as Conference
import           Tox.Conduit.Network                (toSockAddr, toSockAddr6)
import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (Timestamp, getTime)
import           Tox.Core.Timed                     (Timed (..))
import           Tox.Crypto.Core.Box                (PlainText (..))
import           Tox.Crypto.Core.Key                (PublicKey)
import           Tox.Crypto.Core.Keyed              (Keyed (..), KeyedT)
import           Tox.Crypto.Core.KeyPair            (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..),
                                                     randomKey, randomWord32)
import qualified Tox.DHT.DhtPacket                  as DhtPacket
import qualified Tox.DHT.DhtState                   as DhtState
import           Tox.DHT.DhtState                   (DhtState)
import qualified Tox.DHT.Nat                        as Nat
import           Tox.DHT.Operation                  (DhtNodeMonad (..))
import           Tox.Network.Core.HostAddress       (HostAddress (..))
import           Tox.Network.Core.Networked         (Networked (..))
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..))
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Network.TCP.Connections        as Tcp
import qualified Tox.Network.TCP.Server             as Server
import qualified Tox.Onion.Operation                as Onion
import           Tox.Onion.Operation                (OnionState)
import qualified Tox.Session.Connection             as Connection
import           Tox.Session.Connection             (ConnectionManager)
import qualified Tox.Session.Friend                 as Messenger
import           Tox.Session.Friend                 (Messenger)

data GlobalState = GlobalState
  { dhtState     :: DhtState
  , onionState   :: OnionState
  , connManager  :: ConnectionManager
  , messenger    :: Messenger
  , udpSocket    :: Socket.Socket
  , udpEnabled   :: Bool
  , udpIpv6      :: Bool -- ^ The socket is dual-stack IPv6.
  , tcpState     :: Tcp.TcpState
  , tcpIo        :: TcpIo
  , tcpServer    :: Server.TcpServer
  , tcpServerIo  :: TcpServerIo
  , conferences  :: Conference.Conferences
  , groupChats   :: Group.Groups
  , groupTcp     :: Map BS.ByteString GroupTcp -- ^ By chat id.
  , groupTcpLeft :: [(Timestamp, BS.ByteString, [PublicKey])]
    -- ^ The relays of groups we left, by chat id, to close a while after
    -- then: a toxcore relay drops what comes with a closing connection
    -- unread, such as our parting message.
  , friendLinks  :: Map PublicKey TransportProtocol -- ^ How we last told the client each online friend is connected.
  }

-- | A group's own TCP relay connections, made with its keys.
data GroupTcp = GroupTcp
  { gtKeys  :: KeyPair
  , gtState :: Tcp.TcpState
  }

-- | Whose connection to a relay a socket is: ours, or a group's by chat id.
type RelayOwner = Maybe BS.ByteString

-- | Socket operations for TCP relay connections, run by the application.
data TcpIo = TcpIo
  { tcpIoConnect :: RelayOwner -> NodeInfo -> IO ()
  , tcpIoSend    :: RelayOwner -> PublicKey -> BS.ByteString -> IO ()
  , tcpIoClose   :: RelayOwner -> PublicKey -> IO ()
  , tcpIoBusy    :: RelayOwner -> PublicKey -> IO Bool
  }

noTcpIo :: TcpIo
noTcpIo = TcpIo (\_ _ -> return ()) (\_ _ _ -> return ()) (\_ _ -> return ()) (\_ _ -> return False)

-- | Socket operations for the clients of our TCP relay.
data TcpServerIo = TcpServerIo
  { serverIoSend  :: Server.ClientId -> BS.ByteString -> IO ()
  , serverIoClose :: Server.ClientId -> IO ()
  }

noTcpServerIo :: TcpServerIo
noTcpServerIo = TcpServerIo (\_ _ -> return ()) (const $ return ())

initState :: MonadRandomBytes m => Timestamp -> KeyPair -> KeyPair -> Socket.Socket -> m GlobalState
initState now dhtKeys realKeys sock = do
  cKey <- randomKey
  nospam <- randomWord32
  onion <- Onion.initState realKeys
  return GlobalState
    { dhtState    = DhtState.empty now dhtKeys
    , onionState  = onion
    , connManager = Connection.initManager realKeys dhtKeys cKey
    , messenger   = Messenger.initMessenger nospam
    , udpSocket   = sock
    , udpEnabled  = True
    , udpIpv6     = False
    , tcpState    = Tcp.emptyTcpState
    , tcpIo       = noTcpIo
    , tcpServer   = Server.newTcpServer dhtKeys
    , tcpServerIo = noTcpServerIo
    , conferences = Conference.emptyConferences
    , groupChats  = Group.emptyGroups
    , groupTcp    = Map.empty
    , groupTcpLeft = []
    , friendLinks = Map.empty
    }

-- | Unified application monad.
newtype AppMonad m a = AppMonad { unAppMonad :: ReaderT (IORef GlobalState) (LoggingT (KeyedT m)) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadRandomBytes, Keyed, MonadReader (IORef GlobalState), MonadLogger)

instance MonadIO m => Timed (AppMonad m) where
    askTime = liftIO getTime

-- | Use the underlying IORef for the main MonadState.
instance (MonadIO m, MonadRandomBytes m) => MonadState GlobalState (AppMonad m) where
    get = ask >>= liftIO . readIORef
    put s = ask >>= liftIO . flip writeIORef s

instance (MonadIO m, MonadRandomBytes m) => DhtNodeMonad (AppMonad m) where
    getDhtState = gets dhtState
    putDhtState s = modify $ \gs -> gs { dhtState = s }
    handleDhtRequestPayload _ pkt = do
        keys <- gets (DhtState.dhtKeyPair . dhtState)
        plain <- DhtPacket.decryptKeyed keys pkt
        case BS.uncons . unPlainText <$> plain of
            Just (Just (ident, rest)) | ident == Nat.packetIdNatPing -> Nat.handleNatPing (DhtPacket.senderPublicKey pkt) rest
            _ -> return ()

instance (MonadIO m, MonadRandomBytes m) => Onion.OnionNodeMonad (AppMonad m) where
    getOnionState = gets onionState
    putOnionState s = modify $ \gs -> gs { onionState = s }
    getDhtKeyPair = gets (DhtState.dhtKeyPair . dhtState)
    getCloseNodes n key = gets (DhtState.takeClosestNodesTo n key . dhtState)
    getCloseNodesFor from n key = gets (DhtState.takeClosestNodesFor from n key . dhtState)
    getTcpRelays = Tcp.connectedRelays
    sendOnionViaRelay = Tcp.sendOnionRequest
    sendToTcpClient = Server.sendOnionResponse
    sendDhtRequest to plain = Nat.dhtRequest to plain >>= Nat.routeToFriend to

instance (MonadIO m, MonadRandomBytes m) => Tcp.TcpNetworked (AppMonad m) where
    tcpConnect relay = gets tcpIo >>= \io -> liftIO (tcpIoConnect io Nothing relay)
    tcpSend pk bytes = gets tcpIo >>= \io -> liftIO (tcpIoSend io Nothing pk bytes)
    tcpClose pk = gets tcpIo >>= \io -> liftIO (tcpIoClose io Nothing pk)
    tcpBusy pk = gets tcpIo >>= \io -> liftIO (tcpIoBusy io Nothing pk)

instance (MonadIO m, MonadRandomBytes m) => Tcp.TcpMonad (AppMonad m) where
    getTcpState = gets tcpState
    putTcpState s = modify $ \gs -> gs { tcpState = s }
    getTcpDhtKeys = gets (DhtState.dhtKeyPair . dhtState)

instance (MonadIO m, MonadRandomBytes m) => Server.TcpServerMonad (AppMonad m) where
    getTcpServer = gets tcpServer
    putTcpServer s = modify $ \gs -> gs { tcpServer = s }
    serverSend client bytes = gets tcpServerIo >>= \io -> liftIO (serverIoSend io client bytes)
    serverClose client = gets tcpServerIo >>= \io -> liftIO (serverIoClose io client)

instance (MonadIO m, MonadRandomBytes m) => Connection.ConnectionMonad (AppMonad m) where
    getConnManager = gets connManager
    putConnManager s = modify $ \gs -> gs { connManager = s }

instance (MonadIO m, MonadRandomBytes m) => Messenger.MessengerMonad (AppMonad m) where
    getMessenger = gets messenger
    putMessenger s = modify $ \gs -> gs { messenger = s }

instance (MonadIO m, MonadRandomBytes m) => Conference.ConferenceMonad (AppMonad m) where
    getConferences = gets conferences
    putConferences s = modify $ \gs -> gs { conferences = s }

instance (MonadIO m, MonadRandomBytes m) => Group.GroupMonad (AppMonad m) where
    getGroups = gets groupChats
    putGroups s = modify $ \gs -> gs { groupChats = s }
    withGroupTcp = runGroupTcp
    groupUdp = gets udpEnabled
    dropGroupTcp chatId = do
        relays <- gets (maybe [] (Map.keys . Tcp.tsRelays . gtState) . Map.lookup chatId . groupTcp)
        now <- askTime
        modify $ \gs -> gs { groupTcp = Map.delete chatId (groupTcp gs)
                           , groupTcpLeft = (now, chatId, relays) : groupTcpLeft gs }

-- | Close the relays of groups left a while ago, but those a group we are in
-- again uses.
closeLeftGroupTcp :: (MonadIO m, MonadRandomBytes m) => Timestamp -> AppMonad m ()
closeLeftGroupTcp now = do
    (due, waiting) <- gets (span' . groupTcpLeft)
    io <- gets tcpIo
    forM_ due $ \(_, chatId, relays) -> do
        used <- gets (maybe [] (Map.keys . Tcp.tsRelays . gtState) . Map.lookup chatId . groupTcp)
        liftIO $ mapM_ (tcpIoClose io (Just chatId)) (filter (`notElem` used) relays)
    modify $ \gs -> gs { groupTcpLeft = waiting }
  where
    span' = partition (\(left, _, _) -> Time.addTime left leftGroupTcpDelay <= now)

-- | Long enough for a relay to read what we sent before we closed.
leftGroupTcpDelay :: Time.TimeDiff
leftGroupTcpDelay = Time.seconds 2

-- | The TCP layer working on one group's connections.
newtype GroupTcpM m a = GroupTcpM { unGroupTcpM :: ReaderT (BS.ByteString, KeyPair) (AppMonad m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadRandomBytes)

runGroupTcp :: BS.ByteString -> KeyPair -> GroupTcpM m a -> AppMonad m a
runGroupTcp chatId keys act = runReaderT (unGroupTcpM act) (chatId, keys)

instance MonadIO m => Timed (GroupTcpM m) where
    askTime = liftIO getTime

instance (MonadIO m, MonadRandomBytes m) => Tcp.TcpNetworked (GroupTcpM m) where
    tcpConnect relay = withIo $ \io chatId -> tcpIoConnect io (Just chatId) relay
    tcpSend pk bytes = withIo $ \io chatId -> tcpIoSend io (Just chatId) pk bytes
    tcpClose pk = withIo $ \io chatId -> tcpIoClose io (Just chatId) pk

withIo :: (MonadIO m, MonadRandomBytes m) => (TcpIo -> BS.ByteString -> IO ()) -> GroupTcpM m ()
withIo f = GroupTcpM $ do
    (chatId, _) <- ask
    io <- lift (gets tcpIo)
    liftIO $ f io chatId

instance (MonadIO m, MonadRandomBytes m) => Tcp.TcpMonad (GroupTcpM m) where
    getTcpState = GroupTcpM $ do
        (chatId, _) <- ask
        lift $ gets (maybe Tcp.emptyTcpState gtState . Map.lookup chatId . groupTcp)
    putTcpState st = GroupTcpM $ do
        (chatId, keys) <- ask
        lift . modify $ \gs -> gs { groupTcp = Map.insert chatId (GroupTcp keys st) (groupTcp gs) }
    getTcpDhtKeys = GroupTcpM $ asks snd

instance (MonadIO m, MonadRandomBytes m) => Networked (AppMonad m) where
    linkBusy ni = if protocol ni == TCP then Tcp.peerBusy ni else return False
    sendPacket ni packet = do
        sock <- gets udpSocket
        ipv6 <- gets udpIpv6
        udp <- gets ((&& (ipv6 || not (isIpv6 $ address ni))) . udpEnabled)
        let addr = (if ipv6 then toSockAddr6 else toSockAddr) (address ni)
            bs = LBS.toStrict $ Put.runPut $ do
                Binary.put (packetKind packet)
                Binary.put (packetPayload packet)
        logDebugN $ "Sending packet to " <> pack (show addr) <> ": " <> pack (show $ packetKind packet) <> " (" <> pack (show $ BS.length bs) <> " bytes)"
        if protocol ni == TCP then Tcp.sendToPeer ni bs else when udp $ do
            res <- liftIO . try $ SocketBS.sendTo sock bs addr
            case res of
                -- Safely ignore "Address family not supported"
                Left (e :: IOException) | ioe_type e /= UnsupportedOperation ->
                    logDebugN $ "Sending to " <> pack (show addr) <> " failed: " <> pack (show e)
                _ -> return ()

isIpv6 :: SocketAddress -> Bool
isIpv6 (SocketAddress (IPv6 _) _) = True
isIpv6 _                          = False
