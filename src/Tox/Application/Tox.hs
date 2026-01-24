{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
-- | A Tox instance, like toxcore's tox.h: it runs the network on its own
-- threads, takes commands and reports events.  Friends are named by their
-- public keys.
module Tox.Application.Tox
    ( Tox
    , Options (..)
    , Proxy (..)
    , ProxyType (..)
    , SaveData (..)
    , defaultOptions
    , Event (..)
    , ConnectionStatus (..)
    , FriendInfo (..)
    , Messenger.MessageType (..)
    , Messenger.UserStatus (..)
    , Messenger.AddFriendError (..)
    , Messenger.SendMessageError (..)
    , Messenger.FileRef (..)
    , Messenger.fileKindData
    , Messenger.fileKindAvatar
    , Messenger.FileControl (..)
    , Messenger.FileError (..)
    , NewError (..)
    , ToxStopped (..)
    , new
    , kill
    , nextEvent
      -- * Network
    , bootstrap
    , addTcpRelay
    , resolveNode
    , selfConnectionStatus
    , selfUdpPort
    , selfDhtId
      -- * Profile
    , saveData
    , selfPublicKey
    , selfSecretKey
    , selfAddress
    , selfGetNospam
    , selfSetNospam
    , maxNameLength
    , maxStatusMessageLength
    , selfGetName
    , selfSetName
    , selfGetStatusMessage
    , selfSetStatusMessage
    , selfGetStatus
    , selfSetStatus
      -- * Friends
    , parseAddress
    , friendAdd
    , friendAddNoRequest
    , maxFriendRequestLength
    , friendDelete
    , friendList
    , friendInfo
    , friendSendMessage
    , maxMessageLength
    , friendSetTyping
    , Messenger.CustomPacketError (..)
    , friendSendLosslessPacket
    , friendSendLossyPacket
    , friendQueueLosslessPacket
    , friendQueueLossyPacket
      -- * Conferences
    , Conference.GroupNumber
    , Conference.PeerNumber
    , Conference.JoinError (..)
    , ConferencePeer (..)
    , conferenceNew
    , conferenceInvite
    , conferenceJoin
    , conferenceDelete
    , conferenceSendMessage
    , conferenceSetTitle
    , conferenceTitle
    , conferencePeers
    , conferenceList
    , conferenceId
    , conferenceSave
    , conferenceRestore
      -- * Groups
    , Group.PeerId
    , Group.ExitType (..)
    , Group.JoinFail (..)
    , Group.AcceptError (..)
    , Group.GroupSendError (..)
    , Group.Role (..)
    , Group.Moderation (..)
    , Group.Voice (..)
    , Group.ModError (..)
    , GroupPeer (..)
    , GroupSettings (..)
    , GroupPackets.maxGroupNameSize
    , GroupPackets.maxPasswordSize
    , groupNew
    , groupInviteFriend
    , groupJoin
    , groupChatId
    , groupInviteAccept
    , groupSendMessage
    , groupSendPrivateMessage
    , groupSendCustomPacket
    , groupSendCustomPrivatePacket
    , groupSetTopic
    , groupMayTopic
    , groupSelfRole
    , groupSettings
    , groupTopic
    , groupName
    , groupSetNick
    , groupPeers
    , groupLeave
    , groupList
    , groupSave
    , groupRestore
    , groupKickPeer
    , groupSetRole
    , groupSetPassword
    , groupSetPeerLimit
    , groupSetPrivacy
    , groupSetTopicLock
    , groupSetVoice
      -- * Files
    , fileSend
    , fileControl
    , fileSeek
    , fileSendChunk
    ) where

import           Control.Concurrent                 (ThreadId, forkFinally,
                                                     forkIO, killThread,
                                                     threadDelay)
import           Control.Concurrent.Chan            (Chan, newChan, readChan,
                                                     writeChan)
import           Control.Concurrent.MVar            (MVar, modifyMVar,
                                                     modifyMVar_, newEmptyMVar,
                                                     newMVar, readMVar,
                                                     swapMVar, takeMVar,
                                                     tryPutMVar, withMVar)
import           Control.Concurrent.STM             (TBQueue, TMVar, atomically,
                                                     isFullTBQueue,
                                                     newEmptyTMVarIO,
                                                     newTBQueueIO, orElse,
                                                     putTMVar, readTBQueue,
                                                     readTMVar, writeTBQueue)
import           Control.Exception                  (AsyncException (..),
                                                     Exception, IOException,
                                                     finally, fromException,
                                                     handle, onException,
                                                     throwIO, try)
import           Control.Monad                      (forM, forM_, forever,
                                                     unless, void, when, (<=<),
                                                     (>=>))
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Logger               (LogLevel (..), fromLogStr,
                                                     runLoggingT)
import           Control.Monad.Random               (evalRandT)
import           Control.Monad.Reader               (runReaderT)
import           Control.Monad.State                (gets, modify)
import qualified Crypto.Saltine.Class               as Sodium
import qualified Data.Binary                        as Binary
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Functor.Identity              (runIdentity)
import           Data.IORef                         (atomicModifyIORef',
                                                     newIORef, readIORef)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust, mapMaybe,
                                                     maybeToList)
import           Data.Text                          (Text, pack)
import qualified Data.Text.Encoding                 as Text
import qualified Data.Text.Encoding.Error           as Text
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8, byteSwap32)
import qualified Network.Socket                     as Socket
import qualified Network.Socket.ByteString          as SocketBS
import qualified System.Clock                       as Clock
import           System.Random                      (mkStdGen)

import qualified Tox.Application.GroupChat          as Group
import qualified Tox.Application.GroupChatPackets   as GroupPackets
import qualified Tox.Application.LegacyGroup        as Conference
import           Tox.Application.Proxy              (Proxy (..), ProxyType (..),
                                                     connectVia)
import qualified Tox.Application.Save               as Save
import           Tox.Application.Save               (Profile (..),
                                                     SavedFriend (..))
import           Tox.Application.State
import           Tox.Conduit.Network                (fromSockAddr, toSockAddr)
import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (Timestamp, getTime)
import           Tox.Core.Timed                     (Timed (..))
import qualified Tox.Crypto.Core.Key                as Key
import           Tox.Crypto.Core.Key                (PublicKey)
import           Tox.Crypto.Core.Keyed              (evalKeyedT)
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..))
import           Tox.DHT.DhtPacket                  (DhtPacket (..))
import qualified Tox.DHT.DhtState                   as DhtState
import qualified Tox.DHT.KBuckets                   as KBuckets
import qualified Tox.DHT.Nat                        as Nat
import qualified Tox.DHT.Node                       as DHTNode
import qualified Tox.Network.Core.HostAddress       as HostAddress
import           Tox.Network.Core.HostAddress       (HostAddress (..))
import           Tox.Network.Core.Networked         (Networked (..))
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..),
                                                     RawPayload (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import qualified Tox.Network.Core.SocketAddress     as SocketAddress
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Network.TCP.Connections        as Tcp
import qualified Tox.Network.TCP.Server             as Server
import qualified Tox.Onion.Operation                as Onion
import qualified Tox.Persistence.Groups             as PG
import qualified Tox.Session.Connection             as Connection
import qualified Tox.Session.Friend                 as Messenger


{-------------------------------------------------------------------------------
 -
 - :: Types.
 -
 ------------------------------------------------------------------------------}

data Options = Options
  { optUdpEnabled     :: Bool
  , optIpv6Enabled    :: Bool         -- ^ Use IPv6 as well as IPv4 where the system has it.
  , optUdpPorts       :: (Word16, Word16) -- ^ Bind the first free port in this range, (0, 0) for any.
  , optTcpPort        :: Maybe Word16  -- ^ Run a TCP relay on this port.
  , optSaveData       :: SaveData
  , optSeed           :: Maybe Int     -- ^ Make new keys from this seed, for tests.
  , optLocalDiscovery :: Bool      -- ^ Find peers on the local network.
  , optProxy          :: Maybe Proxy -- ^ Reach TCP relays through it.
  , optLogger         :: LogLevel -> Text -> IO ()
    -- ^ Runs on the main loop's thread, so it must not wait for the
    -- instance, for example by calling it.
  }

defaultOptions :: Options
defaultOptions = Options True True (lanPortFrom, lanPortTo) Nothing NoSaveData Nothing True Nothing (\_ _ -> return ())

-- | What a new Tox starts with, like toxcore's savedata types.
data SaveData
  = NoSaveData
  | ToxSave BS.ByteString
    -- ^ A profile in toxcore's save format.
  | SecretKey Key.SecretKey
    -- ^ Only our key, for clients that keep the rest themselves.

data NewError
  = BadSaveData
  | PortInUse
  deriving (Eq, Show)

data ConnectionStatus = NotConnected | ConnectedTcp | ConnectedUdp
  deriving (Eq, Show)

data Event
  = SelfConnectionChanged ConnectionStatus
  | FriendConnectionChanged PublicKey ConnectionStatus
  | FriendRequest PublicKey BS.ByteString
  | FriendMessage PublicKey Messenger.MessageType BS.ByteString
  | FriendReadReceipt PublicKey Word32
  | FriendName PublicKey BS.ByteString
  | FriendStatusMessage PublicKey BS.ByteString
  | FriendStatus PublicKey Messenger.UserStatus
  | FriendTyping PublicKey Bool
  | FileReceive PublicKey Word8 Word32 Word64 BS.ByteString BS.ByteString
    -- ^ A friend offers a file: its number, kind, size, id and name.  Accept
    -- it with 'fileControl'.
  | FileReceiveChunk PublicKey Word8 Word64 BS.ByteString
    -- ^ File data at a position.  No data means the file is complete.
  | FileChunkRequest PublicKey Word8 Word64 Int
    -- ^ Send this much of a file from this position with 'fileSendChunk'.
    -- A length of 0 means the friend has the whole file.
  | FileControlChanged PublicKey Messenger.FileRef Messenger.FileControl
  | FriendLosslessPacket PublicKey BS.ByteString
    -- ^ A packet with an id for clients, the id included.
  | FriendLossyPacket PublicKey BS.ByteString
  | ConferenceInvite PublicKey Word8 BS.ByteString
    -- ^ A friend invites us to a conference of a type.  Join it with the
    -- cookie and 'conferenceJoin'.
  | ConferenceConnected Conference.GroupNumber
  | ConferenceMessage Conference.GroupNumber Conference.PeerNumber Messenger.MessageType BS.ByteString
  | ConferenceTitle Conference.GroupNumber (Maybe Conference.PeerNumber) BS.ByteString
  | ConferencePeerName Conference.GroupNumber Conference.PeerNumber BS.ByteString
  | ConferencePeerListChanged Conference.GroupNumber
  | GroupInvite PublicKey BS.ByteString BS.ByteString
    -- ^ A friend invites us to a group: the invite data for
    -- 'groupInviteAccept', and the group name.
  | GroupSelfJoin Group.GroupNumber
  | GroupJoinFail Group.GroupNumber Group.JoinFail
  | GroupPeerJoin Group.GroupNumber Group.PeerId PublicKey
  | GroupPeerExit Group.GroupNumber Group.PeerId Group.ExitType BS.ByteString BS.ByteString
  | GroupPeerName Group.GroupNumber Group.PeerId BS.ByteString
  | GroupPeerStatus Group.GroupNumber Group.PeerId Word8
  | GroupMessage Group.GroupNumber Group.PeerId Messenger.MessageType BS.ByteString Word32
  | GroupPrivateMessage Group.GroupNumber Group.PeerId Messenger.MessageType BS.ByteString Word32
  | GroupTopic Group.GroupNumber (Maybe Group.PeerId) BS.ByteString
  | GroupModeration Group.GroupNumber (Maybe Group.PeerId) (Maybe Group.PeerId) Group.Moderation -- ^ Setter and target, 'Nothing' for us.
  | GroupPrivacy Group.GroupNumber Bool -- ^ Whether it is private now.
  | GroupPeerLimit Group.GroupNumber Word16
  | GroupPassword Group.GroupNumber BS.ByteString
  | GroupTopicLock Group.GroupNumber Bool
  | GroupVoiceState Group.GroupNumber Group.Voice
  | GroupCustomPacket Group.GroupNumber Group.PeerId BS.ByteString
  | GroupCustomPrivatePacket Group.GroupNumber Group.PeerId BS.ByteString
  deriving (Eq, Show)

data FriendInfo = FriendInfo
  { friendPublicKey     :: PublicKey
  , friendName          :: BS.ByteString
  , friendStatusMessage :: BS.ByteString
  , friendStatus        :: Messenger.UserStatus
  , friendTyping        :: Bool
  , friendConnection    :: ConnectionStatus
  , friendLastSeen      :: Word64 -- ^ Unix time we last saw the friend online, 0 for never.
  } deriving (Eq, Show)

-- | What the main loop handles, in order.  Relay sockets are numbered, so
-- what an old socket to a relay reports is told apart from the new one's.
data Input
  = Datagram Socket.SockAddr BS.ByteString
  | RelayBytes RelayOwner PublicKey Int BS.ByteString
  | RelayClosed RelayOwner PublicKey Int
  | ClientAccepted Server.ClientId
  | ClientBytes Server.ClientId BS.ByteString
  | ClientClosed Server.ClientId
  | Tick
  | Command (AppMonad IO ())

-- | Inputs waiting for the main loop, and whether it has ended.  While the
-- queue is full, datagrams and ticks are dropped and the rest wait.
data Inputs = Inputs
  { inputQueue :: TBQueue Input
  , loopEnded  :: TMVar ()
  }

maxWaitingInputs :: Int
maxWaitingInputs = 4096

pushInput, offerInput :: Inputs -> Input -> IO ()
pushInput inputs input = atomically $ readTMVar (loopEnded inputs) `orElse` writeTBQueue (inputQueue inputs) input
offerInput inputs input = atomically $ isFullTBQueue (inputQueue inputs) >>= \full -> unless full (writeTBQueue (inputQueue inputs) input)

data Tox = Tox
  { toxInputs    :: Inputs
  , toxEvents    :: Chan Event
  , toxPublicKey :: PublicKey
  , toxSecretKey :: Key.SecretKey
  , toxDhtId     :: PublicKey
  , toxUdpPort   :: Word16
  , toxThreads   :: MVar [ThreadId]
  , toxShutdown  :: IO ()
  }

-- | The instance was killed, or its main loop died.
data ToxStopped = ToxStopped
  deriving (Eq, Show)

instance Exception ToxStopped


{-------------------------------------------------------------------------------
 -
 - :: Lifecycle.
 -
 ------------------------------------------------------------------------------}

-- | Interval of the main loop's periodic work.
tickInterval :: Int
tickInterval = 50000

-- | Interval of DHT and onion maintenance.
dhtInterval :: Time.TimeDiff
dhtInterval = Time.seconds 2

-- | Bytes read from a socket at once.
readSize :: Int
readSize = 4096

-- | Waits after a failed read or accept, so a lasting failure does not spin.
readRetryDelay, acceptRetryDelay :: Int
readRetryDelay = 10000
acceptRetryDelay = 1000000

new :: Options -> IO (Either NewError Tox)
new opts = case optSaveData opts of
  NoSaveData -> startWith opts Nothing Nothing
  ToxSave bytes -> case Binary.decodeOrFail (LBS.fromStrict bytes) of
    Right (_, _, sd) | Just profile <- Save.fromSaveData sd ->
      startWith opts (Just (profileKeys profile)) (Just profile)
    _ -> return $ Left BadSaveData
  SecretKey sk -> startWith opts (Just (KeyPair.fromSecretKey sk)) Nothing

startWith :: Options -> Maybe KeyPair.KeyPair -> Maybe Profile -> IO (Either NewError Tox)
startWith opts keys profile = openSockets >>= \case
  Nothing -> return $ Left PortInUse
  Just (sock, ipv6, inputs, server) -> fmap Right . (`onException` (Socket.close sock >> mapM_ snd server)) $ do
    now <- getTime
    initial <- case optSeed opts of
      Just seed -> return . runIdentity . evalRandT (initial0 now sock) $ mkStdGen seed
      Nothing   -> initial0 now sock
    Socket.setSocketOption sock Socket.Broadcast 1
    port <- Socket.socketPort sock
    events <- newChan
    (tcp, isCurrent, stopTcp) <- makeTcpIo (optProxy opts) inputs
    stateRef <- newIORef initial
      { udpEnabled = optUdpEnabled opts
      , udpIpv6 = ipv6
      , tcpIo = tcp
      , tcpServerIo = maybe noTcpServerIo fst server
      , onionState = (onionState initial) { Onion.onionTcpOnly = not (optUdpEnabled opts) }
      }
    threads <- newMVar []
    let fork action = forkIO action >>= \t -> modifyMVar_ threads (return . (t :))
        logger _ _ level str = optLogger opts level (Text.decodeUtf8With Text.lenientDecode $ fromLogStr str)
    -- A failed read, which some systems report for an unreachable peer, must
    -- not stop us listening.
    fork $ forever $ try (SocketBS.recvFrom sock readSize) >>= \case
      Right (bs, addr)        -> offerInput inputs (Datagram addr bs)
      Left (e :: IOException) -> optLogger opts LevelDebug (pack $ "Receiving failed: " ++ show e) >> threadDelay readRetryDelay
    fork $ forever $ threadDelay tickInterval >> offerInput inputs Tick
    let ended = \case
          Left e | fromException e /= Just ThreadKilled -> optLogger opts LevelError . pack $ "Main loop stopped: " ++ show e
          _ -> return ()
        loop = flip evalKeyedT Map.empty $ flip runLoggingT logger $ flip runReaderT stateRef $ unAppMonad $ do
          restore profile
          forM_ (optTcpPort opts) $ \tcpPort -> do
            -- Like toxcore, we use our own relay as a client too.
            dhtPk <- gets (KeyPair.publicKey . DhtState.dhtKeyPair . dhtState)
            Tcp.addRelay $ NodeInfo TCP (SocketAddress (IPv4 $ Socket.tupleToHostAddress (127, 0, 0, 1)) (fromIntegral tcpPort)) dhtPk
          mainLoop inputs events isCurrent (optUdpEnabled opts && optLocalDiscovery opts) (LoopState Nothing NotConnected Nothing (lanPortFrom + 1))
    loopThread <- forkFinally loop (\r -> ended r >> atomically (putTMVar (loopEnded inputs) ()))
    modifyMVar_ threads (return . (loopThread :))
    return Tox
      { toxInputs    = inputs
      , toxEvents    = events
      , toxPublicKey = KeyPair.publicKey . Connection.ourRealKeys $ connManager initial
      , toxSecretKey = KeyPair.secretKey . Connection.ourRealKeys $ connManager initial
      , toxDhtId     = KeyPair.publicKey . DhtState.dhtKeyPair $ dhtState initial
      , toxUdpPort   = fromIntegral port
      , toxThreads   = threads
      , toxShutdown  = Socket.close sock >> stopTcp >> mapM_ snd server
      }
  where
    -- The UDP socket on the first free port, and our relay's if we run one.
    openSockets = do
      (sock, ipv6) <- if optIpv6Enabled opts
        then try (dualStackSocket Socket.Datagram) >>= \case
          Right sock              -> return (sock, True)
          Left (_ :: IOException) -> ipv4Socket
        else ipv4Socket
      let (from, to) = optUdpPorts opts
          bindFirst [] = return False
          bindFirst (port : ports) = try (Socket.bind sock (anyAddress ipv6 port)) >>= \case
            Left (_ :: IOException) -> bindFirst ports
            Right ()                -> return True
      bound <- bindFirst [from .. to]
      inputs <- Inputs <$> newTBQueueIO (fromIntegral maxWaitingInputs) <*> newEmptyTMVarIO
      server <- if not bound then return Nothing else
        either (\(_ :: IOException) -> Nothing) Just <$> try (mapM (makeTcpServerIo (optLogger opts) inputs ipv6) (optTcpPort opts))
      case server of
        Just s  -> return $ Just (sock, ipv6, inputs, s)
        Nothing -> Nothing <$ Socket.close sock
    ipv4Socket = (, False) <$> Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    anyAddress True port  = Socket.SockAddrInet6 (fromIntegral port) 0 (0, 0, 0, 0) 0
    anyAddress False port = Socket.SockAddrInet (fromIntegral port) 0
    initial0 :: MonadRandomBytes m => Timestamp -> Socket.Socket -> m GlobalState
    initial0 now sock = do
      realKeys <- maybe newKeyPair return keys
      dhtKeys <- newKeyPair
      gs <- initState now dhtKeys realKeys sock
      return $ case profile of
        Nothing -> gs
        Just p -> gs { messenger = (messenger gs)
          { Messenger.selfNospam = profileNospam p
          , Messenger.selfName   = profileName p
          , Messenger.selfMsg    = profileStatusMessage p
          , Messenger.selfStatus = profileStatus p
          } }

-- | What a save file brings back: friends, nodes, relays, conferences and
-- groups.
restore :: Maybe Profile -> AppMonad IO ()
restore Nothing = return ()
restore (Just p) = do
  forM_ (profileFriends p) $ \f -> do
    case sfRequest f of
      Nothing                -> Messenger.addFriend (sfPublicKey f)
      Just (nospam, message) -> void $ Messenger.addFriendWithRequest (sfPublicKey f) nospam message
    Messenger.modifyFriend (sfPublicKey f) $ \friend -> friend
      { Messenger.friendName = sfName f
      , Messenger.friendMsg = sfStatusMessage f
      , Messenger.friendStatus = sfStatus f
      , Messenger.friendLastSeen = sfLastSeen f
      }
  mapM_ bootstrapNode (profileDhtNodes p)
  Onion.addBootstrapNodes (profilePathNodes p)
  mapM_ Tcp.addRelay (profileTcpRelays p)
  Conference.restore (profileConferences p)
  Group.restore (profileGroups p)

-- | Stop the instance: friends see us go offline.
kill :: Tox -> IO ()
kill tox = do
  handle (\ToxStopped -> return ()) . call tox $ do
    friends <- Connection.getsConn (Map.keys . Connection.friends)
    mapM_ Connection.killSession friends
  -- The loop goes first, so it opens no sockets once they are closed.
  mapM_ killThread =<< swapMVar (toxThreads tox) []
  toxShutdown tox

nextEvent :: Tox -> IO Event
nextEvent = readChan . toxEvents

-- | Run an action on the main loop and wait for its result. Throws
-- 'ToxStopped' if the loop is not running.
call :: Tox -> AppMonad IO a -> IO a
call tox action = do
  result <- newEmptyMVar
  -- A full queue makes us wait, unless the loop has ended.
  let Inputs queue ended = toxInputs tox
  sent <- atomically $ (False <$ readTMVar ended)
    `orElse` (True <$ writeTBQueue queue (Command (action >>= liftIO . void . tryPutMVar result . Just)))
  unless sent $ throwIO ToxStopped
  watcher <- forkIO $ atomically (readTMVar ended) >> void (tryPutMVar result Nothing)
  takeMVar result `finally` killThread watcher >>= maybe (throwIO ToxStopped) return

-- | Queue an action for the main loop without waiting for it, as long as the
-- loop runs.
cast :: Tox -> AppMonad IO () -> IO ()
cast tox = pushInput (toxInputs tox) . Command


{-------------------------------------------------------------------------------
 -
 - :: Main loop.
 -
 ------------------------------------------------------------------------------}

data LoopState = LoopState
  { lsLastDht     :: Maybe Timestamp
  , lsSelfConn    :: ConnectionStatus
  , lsLastLan     :: Maybe Timestamp
  , lsNextLanPort :: Word16
  }

-- | The ports toxcore uses.  LAN discovery always goes to the first and to
-- a few more each time.
lanPortFrom, lanPortTo, lanPortsPerDiscovery :: Word16
lanPortFrom = 33445
lanPortTo = 33545
lanPortsPerDiscovery = 10

lanDiscoveryInterval :: Time.TimeDiff
lanDiscoveryInterval = Time.seconds 10

-- | Tell peers on the local network about us, by broadcast and, with IPv6,
-- to all nodes on the link.
sendLanDiscovery :: Word16 -> AppMonad IO Word16
sendLanDiscovery first = do
  dhtPk <- gets (KeyPair.publicKey . DhtState.dhtKeyPair . dhtState)
  ipv6 <- gets udpIpv6
  let lastPort = min lanPortTo (first + lanPortsPerDiscovery)
      broadcast = IPv4 $ Socket.tupleToHostAddress (255, 255, 255, 255)
      allNodes = IPv6 (0xff020000, 0, 0, 1)
      packet = Packet PacketKind.LanDiscovery (RawPayload $ Sodium.encode $ Key.unKey dhtPk)
  forM_ (lanPortFrom : [first .. lastPort - 1]) $ \port ->
    forM_ (broadcast : [allNodes | ipv6]) $ \host ->
      sendPacket (NodeInfo UDP (SocketAddress host (fromIntegral port)) dhtPk) packet
  return $ if lastPort == lanPortTo then lanPortFrom + 1 else lastPort

mainLoop :: Inputs -> Chan Event -> RelaySockets -> Bool -> LoopState -> AppMonad IO ()
mainLoop inputs events isCurrent lan ls = do
  input <- liftIO . atomically . readTBQueue $ inputQueue inputs
  let emit = mapM_ (liftIO . writeChan events)
      whenCurrent owner pk gen act = liftIO (isCurrent owner pk gen) >>= \current -> when current act
  ls' <- case input of
    Datagram addr bs -> do
      forM_ (fromSockAddr addr) $ \sa -> forM_ (decodePacket bs) $ handleDatagram lan sa >=> emit
      return ls
    RelayBytes owner pk gen bytes -> ls <$ whenCurrent owner pk gen (relayBytes owner pk bytes >>= emit)
    RelayClosed owner pk gen -> ls <$ whenCurrent owner pk gen (relayClosed owner pk)
    ClientAccepted client -> ls <$ Server.clientAccepted client
    ClientBytes client bytes -> do
      serverEvents <- Server.clientBytes client bytes
      forM_ serverEvents $ \(Server.ServerOnionRequest from request) -> Onion.handleTcpOnionRequest from request
      return ls
    ClientClosed client -> ls <$ Server.killClient client
    Command action -> ls <$ action
    Tick -> do
      now <- askTime
      lastDht <- if Time.due now dhtInterval (lsLastDht ls)
        then do
          DHTNode.handleMaintenance
          Onion.doOnion
          return $ Just now
        else return $ lsLastDht ls
      Nat.doNat
      Tcp.doTcp
      Server.doTcpServer
      Messenger.doMessenger >>= concatMapM toEvent >>= emit
      Connection.doFriendConnections >>= connectionEvents >>= emit
      Conference.doConferences >>= emit . map conferenceEvent
      Group.doGroups >>= emit . map groupEvent
      closeLeftGroupTcp now
      linkChanges >>= emit
      conn <- getSelfConnection
      when (conn /= lsSelfConn ls) $ liftIO $ writeChan events (SelfConnectionChanged conn)
      (lastLan, nextLanPort) <- if lan && Time.due now lanDiscoveryInterval (lsLastLan ls)
        then (,) (Just now) <$> sendLanDiscovery (lsNextLanPort ls)
        else return (lsLastLan ls, lsNextLanPort ls)
      return $ LoopState lastDht conn lastLan nextLanPort
  mainLoop inputs events isCurrent lan ls'

relayBytes :: RelayOwner -> PublicKey -> BS.ByteString -> AppMonad IO [Event]
relayBytes (Just chatId) pk bytes = do
  keys <- gets (fmap gtKeys . Map.lookup chatId . groupTcp)
  relayEvents <- maybe (return []) (\k -> runGroupTcp chatId k (Tcp.handleRelayBytes pk bytes)) keys
  flip concatMapM relayEvents $ \case
    Tcp.TcpPacket _ packet -> case decodePacket packet of
      Just (Packet kind (RawPayload payload)) | kind `elem` groupKinds -> map groupEvent <$> Group.handlePacket Nothing kind payload
      _ -> return []
    Tcp.TcpOnionResponse _ -> return []
relayBytes Nothing pk bytes = do
  relayEvents <- Tcp.handleRelayBytes pk bytes
  flip concatMapM relayEvents $ \case
    Tcp.TcpPacket link packet -> flip concatMapM (maybeToList $ decodePacket packet) $ \pkt ->
      Connection.handlePacket link (fmap unRawPayload pkt) >>= connectionEvents
    Tcp.TcpOnionResponse packet -> flip concatMapM (maybeToList $ decodePacket packet) $ \pkt ->
      Onion.handleOnionPacket noAddress (fmap unRawPayload pkt) >>= onionEvents

relayClosed :: RelayOwner -> PublicKey -> AppMonad IO ()
relayClosed (Just chatId) pk = do
  keys <- gets (fmap gtKeys . Map.lookup chatId . groupTcp)
  forM_ keys $ \k -> runGroupTcp chatId k (Tcp.relayClosed pk)
relayClosed Nothing pk = Tcp.relayClosed pk

-- | Online friends whose connection moved between UDP and TCP, as toxcore
-- reports them.
linkChanges :: AppMonad IO [Event]
linkChanges = do
  links <- gets friendLinks
  fmap concat . forM (Map.toList links) $ \(pk, reported) -> Connection.friendLink pk >>= \case
    Just link | NodeInfo.protocol link /= reported -> do
      modify $ \gs -> gs { friendLinks = Map.insert pk (NodeInfo.protocol link) (friendLinks gs) }
      return [FriendConnectionChanged pk (linkStatus link)]
    _ -> return []

onionEvents :: [Onion.OnionEvent] -> AppMonad IO [Event]
onionEvents events = do
  fromGroups <- concatMapM Group.handleOnionEvent events
  (map groupEvent fromGroups ++) <$> (concatMapM Connection.handleOnionEvent >=> connectionEvents) events

-- | What friend connections report goes to Messenger, conferences, groups
-- and the client.
connectionEvents :: [Connection.ConnectionEvent] -> AppMonad IO [Event]
connectionEvents events = do
  fromMessenger <- concatMapM Messenger.handleConnectionEvent events >>= concatMapM toEvent
  fromConferences <- concatMapM Conference.handleConnectionEvent events
  fromGroups <- concatMapM Group.handleConnectionEvent events
  fromClient <- concatMapM customPacket events
  return $ fromMessenger ++ map conferenceEvent fromConferences ++ map groupEvent fromGroups ++ fromClient

-- | Lossless ids toxcore sends friends itself.
ownLosslessIds :: [Word8]
ownLosslessIds = [0 .. 15] ++ Connection.packetIds ++ Messenger.packetIds ++ Conference.packetIds
  ++ [GroupPackets.packetIdGroupInvite]

-- | Clients have the lossless ids toxcore doesn't use, and every lossy one.
clientLossless, clientLossy :: Word8 -> Bool
clientLossless ident = ident < 192 && ident `notElem` ownLosslessIds
clientLossy ident = ident >= 192 && ident < 255

customPacket :: Connection.ConnectionEvent -> AppMonad IO [Event]
customPacket = \case
  Connection.FriendData pk bytes | Just (ident, _) <- BS.uncons bytes, clientLossless ident -> do
    online <- Messenger.isFriendOnline pk
    return [FriendLosslessPacket pk bytes | online]
  Connection.FriendLossyData pk bytes | Just (ident, _) <- BS.uncons bytes, clientLossy ident -> do
    online <- Messenger.isFriendOnline pk
    return [FriendLossyPacket pk bytes | online]
  _ -> return []

groupEvent :: Group.GroupEvent -> Event
groupEvent = \case
  Group.GroupInvite friend invite name      -> GroupInvite friend invite name
  Group.GroupSelfJoin g                     -> GroupSelfJoin g
  Group.GroupJoinFail g reason              -> GroupJoinFail g reason
  Group.GroupPeerJoin g n pk                -> GroupPeerJoin g n pk
  Group.GroupPeerExit g n exit name part    -> GroupPeerExit g n exit name part
  Group.GroupPeerName g n name              -> GroupPeerName g n name
  Group.GroupPeerStatus g n status          -> GroupPeerStatus g n status
  Group.GroupMessage g n ty msg msgId       -> GroupMessage g n ty msg msgId
  Group.GroupPrivateMessage g n ty msg msgId -> GroupPrivateMessage g n ty msg msgId
  Group.GroupTopic g n topic                -> GroupTopic g n topic
  Group.GroupModeration g setter target m   -> GroupModeration g setter target m
  Group.GroupPrivacy g private              -> GroupPrivacy g private
  Group.GroupPeerLimit g n                  -> GroupPeerLimit g n
  Group.GroupPassword g pw                  -> GroupPassword g pw
  Group.GroupTopicLock g locked             -> GroupTopicLock g locked
  Group.GroupVoiceState g v                 -> GroupVoiceState g v
  Group.GroupCustomPacket g n bytes         -> GroupCustomPacket g n bytes
  Group.GroupCustomPrivatePacket g n bytes  -> GroupCustomPrivatePacket g n bytes

conferenceEvent :: Conference.ConferenceEvent -> Event
conferenceEvent = \case
  Conference.ConferenceInvited pk ty cookie -> ConferenceInvite pk ty cookie
  Conference.ConferenceConnected g          -> ConferenceConnected g
  Conference.ConferenceMessage g n ty msg   -> ConferenceMessage g n ty msg
  Conference.ConferenceTitle g n title      -> ConferenceTitle g n title
  Conference.ConferencePeerName g n name    -> ConferencePeerName g n name
  Conference.ConferencePeerListChanged g    -> ConferencePeerListChanged g

toEvent :: Messenger.MessengerEvent -> AppMonad IO [Event]
toEvent = \case
  Messenger.FriendConnectionStatus pk online -> do
    link <- if online then Connection.friendLink pk else return Nothing
    modify $ \gs -> gs { friendLinks = Map.alter (const $ NodeInfo.protocol <$> link) pk (friendLinks gs) }
    touchLastSeen pk
    return [FriendConnectionChanged pk (maybe NotConnected linkStatus link)]
  Messenger.FriendMessage pk ty msg          -> return [FriendMessage pk ty msg]
  Messenger.FriendNameChanged pk name        -> return [FriendName pk name]
  Messenger.FriendStatusMessageChanged pk m  -> return [FriendStatusMessage pk m]
  Messenger.FriendUserStatusChanged pk s     -> return [FriendStatus pk s]
  Messenger.FriendTypingChanged pk t         -> return [FriendTyping pk t]
  Messenger.FriendRequestReceived pk msg     -> return [FriendRequest pk msg]
  Messenger.FriendReadReceipt pk msgId       -> return [FriendReadReceipt pk msgId]
  Messenger.FileReceived pk n kind size fileId name -> return [FileReceive pk n kind size fileId name]
  Messenger.FileChunkReceived pk n pos bytes -> return [FileReceiveChunk pk n pos bytes]
  Messenger.FileChunkRequested pk n pos len  -> return [FileChunkRequest pk n pos len]
  Messenger.FileControlReceived pk ref c     -> return [FileControlChanged pk ref c]

getFriendConnection :: PublicKey -> AppMonad IO ConnectionStatus
getFriendConnection pk = maybe NotConnected linkStatus <$> Connection.friendLink pk

linkStatus :: NodeInfo -> ConnectionStatus
linkStatus link = if NodeInfo.protocol link == TCP then ConnectedTcp else ConnectedUdp

-- | As in toxcore, UDP only counts with DHT nodes outside the LAN.
getSelfConnection :: AppMonad IO ConnectionStatus
getSelfConnection = do
  connected <- gets (Onion.isConnected . onionState)
  udp <- gets $ \gs -> udpEnabled gs && not (all (SocketAddress.isLan . NodeInfo.address)
    (KBuckets.getAllNodes . DhtState.dhtCloseList $ dhtState gs))
  return $ if not connected then NotConnected else if udp then ConnectedUdp else ConnectedTcp

unixTime :: IO Word64
unixTime = fromIntegral . Clock.sec <$> Clock.getTime Clock.Realtime

-- | The friend is online, or was until now.
touchLastSeen :: PublicKey -> AppMonad IO ()
touchLastSeen pk = do
  now <- liftIO unixTime
  Messenger.modifyFriend pk $ \f -> f { Messenger.friendLastSeen = now }

-- | Onion responses from a relay have no address.  Only requests use it.
noAddress :: SocketAddress
noAddress = SocketAddress (IPv4 0) 0

groupKinds :: [PacketKind.PacketKind]
groupKinds = [PacketKind.GroupHandshake, PacketKind.GroupLossless, PacketKind.GroupLossy]

decodePacket :: BS.ByteString -> Maybe (Packet RawPayload)
decodePacket bytes = case Binary.decodeOrFail (LBS.fromStrict bytes) of
  Right (_, _, pkt) -> Just pkt
  Left _            -> Nothing

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

handleDatagram :: Bool -> SocketAddress -> Packet RawPayload -> AppMonad IO [Event]
handleDatagram lan sa@(SocketAddress host _) pktRaw
  | kind == PacketKind.LanDiscovery = do
      ourPk <- gets (KeyPair.publicKey . DhtState.dhtKeyPair . dhtState)
      case Key.Key <$> Sodium.decode payload of
        Just pk | lan, HostAddress.isLan host, pk /= ourPk -> bootstrapNode (NodeInfo UDP sa pk)
        _ -> return ()
      return []
  | kind `elem` groupKinds =
      map groupEvent <$> Group.handlePacket (Just sa) kind payload
  | kind == PacketKind.Crypto = do
      DHTNode.handleIncomingPacket from pkt
      Onion.handleDhtRequest payload >>= onionEvents
  | kind `elem` [PacketKind.PingRequest, PacketKind.PingResponse, PacketKind.NodesRequest, PacketKind.NodesResponse] =
      [] <$ DHTNode.handleIncomingPacket from pkt
  | kind `elem` [ PacketKind.OnionRequest0, PacketKind.OnionRequest1, PacketKind.OnionRequest2
                , PacketKind.OnionResponse1, PacketKind.OnionResponse2, PacketKind.OnionResponse3
                , PacketKind.AnnounceRequest, PacketKind.AnnounceResponse
                , PacketKind.OnionDataRequest, PacketKind.OnionDataResponse ] =
      Onion.handleOnionPacket sa pkt >>= onionEvents
  | kind `elem` [PacketKind.CookieRequest, PacketKind.CookieResponse, PacketKind.CryptoHandshake, PacketKind.CryptoData] =
      Connection.handlePacket from pkt >>= connectionEvents
  | otherwise = return []
  where
    pkt = fmap unRawPayload pktRaw
    kind = packetKind pkt
    payload = packetPayload pkt
    nullPk = Key.Key (fromJust $ Sodium.decode $ BS.replicate 32 0)
    keyAt offset = maybe nullPk Key.Key $ Sodium.decode (BS.take 32 $ BS.drop offset payload)
    -- The sender's DHT key, where the packet carries it.
    senderPk
      | kind `elem` [PacketKind.PingRequest, PacketKind.PingResponse, PacketKind.NodesRequest, PacketKind.NodesResponse] =
          case Binary.decodeOrFail (LBS.fromStrict payload) of
            Right (_, _, dhtPkt :: DhtPacket) -> senderPublicKey dhtPkt
            _                                 -> nullPk
      | kind == PacketKind.Crypto = keyAt 32
      | kind == PacketKind.CookieRequest = keyAt 0
      | otherwise = nullPk
    from = NodeInfo UDP sa senderPk


{-------------------------------------------------------------------------------
 -
 - :: Sockets.
 -
 ------------------------------------------------------------------------------}

-- | Whether a numbered socket is the current one to a relay.
type RelaySockets = RelayOwner -> PublicKey -> Int -> IO Bool

-- | TCP relay connections: each has a writer thread with a queue, so data
-- sent while connecting waits, and a reader thread feeding the main loop.
-- Also returns an action closing them all.
makeTcpIo :: Maybe Proxy -> Inputs -> IO (TcpIo, RelaySockets, IO ())
makeTcpIo proxy inputs = do
  sockets <- newMVar (0 :: Int, Map.empty)
  let connect owner relay = do
        let pk = NodeInfo.publicKey relay
        queue <- newChan
        pending <- newIORef 0
        gen <- modifyMVar sockets $ \(n, m) -> do
          mapM_ (\(_, q, _) -> writeChan q Nothing) (Map.lookup (owner, pk) m)
          return ((n + 1, Map.insert (owner, pk) (n, queue, pending) m), n)
        void $ forkIO $ do
          result <- try $ do
            sock <- connectVia proxy (toSockAddr $ NodeInfo.address relay)
            -- A small system buffer keeps what waits behind data short, so
            -- the relay's pings are answered in time on a slow link.
            Socket.setSocketOption sock Socket.SendBuffer relaySendBuffer
            void $ forkIO $ readStream (RelayBytes owner pk gen) (RelayClosed owner pk gen) inputs sock
            writeStream (\n -> atomicModifyIORef' pending (\p -> (p - n, ()))) sock queue
          case result of
            Left (_ :: IOException) -> pushInput inputs (RelayClosed owner pk gen)
            Right ()                -> return ()
      current owner pk gen = (== Just gen) . fmap (\(g, _, _) -> g) . Map.lookup (owner, pk) . snd <$> readMVar sockets
      send owner pk bytes = withMVar sockets $ \(_, m) -> forM_ (Map.lookup (owner, pk) m) $ \(_, q, pending) -> do
        atomicModifyIORef' pending (\p -> (p + BS.length bytes, ()))
        writeChan q (Just bytes)
      close owner pk = modifyMVar_ sockets $ \(n, m) -> do
        mapM_ (\(_, q, _) -> writeChan q Nothing) (Map.lookup (owner, pk) m)
        return (n, Map.delete (owner, pk) m)
      busy owner pk = withMVar sockets $ \(_, m) -> case Map.lookup (owner, pk) m of
        Just (_, _, pending) -> (> maxPendingRelayBytes) <$> readIORef pending
        Nothing              -> return False
      closeAll = readMVar sockets >>= mapM_ (\(_, q, _) -> writeChan q Nothing) . Map.elems . snd
  return (TcpIo connect send close busy, current, closeAll)

-- | Bytes a relay connection may hold that the system has not taken yet,
-- before data that can wait does.  toxcore waits whenever the system takes
-- no more.
maxPendingRelayBytes :: Int
maxPendingRelayBytes = 32768

relaySendBuffer :: Int
relaySendBuffer = 65536

-- | Our TCP relay: accepts clients on the port, each with a writer thread
-- and a reader thread feeding the main loop.  Also returns an action
-- stopping it.
makeTcpServerIo :: (LogLevel -> Text -> IO ()) -> Inputs -> Bool -> Word16 -> IO (TcpServerIo, IO ())
makeTcpServerIo logger inputs ipv6 port = do
  listener <- if ipv6 then dualStackSocket Socket.Stream else Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  flip onException (Socket.close listener) $ do
    Socket.setSocketOption listener Socket.ReuseAddr 1
    Socket.bind listener $ if ipv6
      then Socket.SockAddrInet6 (fromIntegral port) 0 (0, 0, 0, 0) 0
      else Socket.SockAddrInet (fromIntegral port) 0
    Socket.listen listener Server.maxIncoming
  queues <- newMVar Map.empty
  acceptor <- forkIO $ forM_ [0 ..] $ \client -> try (Socket.accept listener) >>= \case
    Left (e :: IOException) -> logger LevelWarn (pack $ "Accepting failed: " ++ show e) >> threadDelay acceptRetryDelay
    Right (sock, _) -> do
      queue <- newChan
      modifyMVar_ queues (return . Map.insert client queue)
      -- The writer closes the socket when we stop, even while we wait here.
      void $ forkIO $ writeStream (const $ return ()) sock queue
      pushInput inputs (ClientAccepted client)
      void $ forkIO $ readStream (ClientBytes client) (ClientClosed client) inputs sock
  let send client bytes = withMVar queues $ \m -> mapM_ (`writeChan` Just bytes) (Map.lookup client m)
      close client = modifyMVar_ queues $ \m -> do
        mapM_ (`writeChan` Nothing) (Map.lookup client m)
        return $ Map.delete client m
      stop = do
        killThread acceptor
        Socket.close listener
        readMVar queues >>= mapM_ (`writeChan` Nothing) . Map.elems
  return (TcpServerIo send close, stop)

-- | An IPv6 socket that also takes IPv4, as mapped addresses.
dualStackSocket :: Socket.SocketType -> IO Socket.Socket
dualStackSocket ty = do
  sock <- Socket.socket Socket.AF_INET6 ty Socket.defaultProtocol
  Socket.setSocketOption sock Socket.IPv6Only 0
  return sock

readStream :: (BS.ByteString -> Input) -> Input -> Inputs -> Socket.Socket -> IO ()
readStream received closed inputs sock = do
  result <- try $ SocketBS.recv sock readSize
  case result of
    Right bytes | not (BS.null bytes) -> pushInput inputs (received bytes) >> readStream received closed inputs sock
    Right _                           -> pushInput inputs closed
    Left (_ :: IOException)           -> pushInput inputs closed

-- | Write what comes on the queue to the socket, saying how much went.
writeStream :: (Int -> IO ()) -> Socket.Socket -> Chan (Maybe BS.ByteString) -> IO ()
writeStream written sock queue = readChan queue >>= \case
  Just bytes -> do
    result <- try $ SocketBS.sendAll sock bytes
    case result of
      Right ()                -> written (BS.length bytes) >> writeStream written sock queue
      Left (_ :: IOException) -> Socket.close sock
  Nothing -> Socket.close sock


{-------------------------------------------------------------------------------
 -
 - :: Network.
 -
 ------------------------------------------------------------------------------}

bootstrapNode :: NodeInfo -> AppMonad IO ()
bootstrapNode node = do
  DHTNode.handleBootstrap node
  Onion.addBootstrapNodes [node]

bootstrap :: Tox -> NodeInfo -> IO ()
bootstrap tox = call tox . bootstrapNode

addTcpRelay :: Tox -> NodeInfo -> IO ()
addTcpRelay tox = call tox . Tcp.addRelay . (\n -> n { protocol = TCP })

-- | The addresses of a host, as nodes with the given key.
resolveNode :: String -> Word16 -> PublicKey -> IO [NodeInfo]
resolveNode host port pk = do
  let hints = Socket.defaultHints { Socket.addrSocketType = Socket.Datagram }
  result <- try $ Socket.getAddrInfo (Just hints) (Just host) (Just $ show port)
  return $ case result of
    Left (_ :: IOException) -> []
    Right infos -> [ NodeInfo UDP sa pk | sa <- mapMaybe (fromSockAddr . Socket.addrAddress) infos ]

selfConnectionStatus :: Tox -> IO ConnectionStatus
selfConnectionStatus tox = call tox getSelfConnection

selfUdpPort :: Tox -> Word16
selfUdpPort = toxUdpPort

selfDhtId :: Tox -> PublicKey
selfDhtId = toxDhtId


{-------------------------------------------------------------------------------
 -
 - :: Profile.
 -
 ------------------------------------------------------------------------------}

-- | Our profile in toxcore's save format.
saveData :: Tox -> IO BS.ByteString
saveData tox = call tox $ do
  gets (Map.keys . friendLinks) >>= mapM_ touchLastSeen
  unixAt <- unixClock
  gets (LBS.toStrict . Binary.encode . Save.toSaveData . Save.profileOf unixAt)

-- | Turns the times we keep into unix time.
unixClock :: AppMonad IO (Timestamp -> Word64)
unixClock = do
  unixNow <- liftIO unixTime
  now <- askTime
  return $ \t -> unixNow - fromIntegral (Time.toMilliseconds (Time.diffTime now t) `div` 1000)

selfPublicKey :: Tox -> PublicKey
selfPublicKey = toxPublicKey

selfSecretKey :: Tox -> Key.SecretKey
selfSecretKey = toxSecretKey

-- | Our Tox ID, which others add us with.
selfAddress :: Tox -> IO BS.ByteString
selfAddress tox = call tox $ gets (Messenger.toxId (toxPublicKey tox) . messenger)

-- | As in toxcore's API, the bytes in the Tox ID read big-endian.
selfGetNospam :: Tox -> IO Word32
selfGetNospam tox = call tox $ gets (byteSwap32 . Messenger.selfNospam . messenger)

-- | A new nospam changes our Tox ID, so requests to the old one fail.
selfSetNospam :: Tox -> Word32 -> IO ()
selfSetNospam tox nospam = call tox $ modify $ \gs -> gs { messenger = (messenger gs) { Messenger.selfNospam = byteSwap32 nospam } }

selfGetName :: Tox -> IO BS.ByteString
selfGetName tox = call tox $ gets (Messenger.selfName . messenger)

-- | 'False' if the name is too long.
selfSetName :: Tox -> BS.ByteString -> IO Bool
selfSetName tox name = call tox $ do
  ok <- Messenger.setSelfName name
  when ok Conference.nameChanged
  return ok

selfGetStatusMessage :: Tox -> IO BS.ByteString
selfGetStatusMessage tox = call tox $ gets (Messenger.selfMsg . messenger)

-- | The longest name and status message, in bytes.
maxNameLength, maxStatusMessageLength :: Int
maxNameLength = Messenger.maxNameLength
maxStatusMessageLength = Messenger.maxStatusMessageLength

-- | 'False' if the status message is too long.
selfSetStatusMessage :: Tox -> BS.ByteString -> IO Bool
selfSetStatusMessage tox = call tox . Messenger.setSelfStatusMessage

selfGetStatus :: Tox -> IO Messenger.UserStatus
selfGetStatus tox = call tox $ gets (Messenger.selfStatus . messenger)

selfSetStatus :: Tox -> Messenger.UserStatus -> IO ()
selfSetStatus tox = call tox . Messenger.setSelfStatus


{-------------------------------------------------------------------------------
 -
 - :: Friends.
 -
 ------------------------------------------------------------------------------}

-- | The public key and nospam in a Tox ID, if its checksum is right.
parseAddress :: BS.ByteString -> Maybe (PublicKey, BS.ByteString)
parseAddress = Messenger.parseToxId

-- | Add a friend by Tox ID and send them a friend request.
friendAdd :: Tox -> BS.ByteString -> BS.ByteString -> IO (Either (Maybe Messenger.AddFriendError) PublicKey)
friendAdd tox toxId message = case parseAddress toxId of
  Nothing -> return $ Left Nothing
  Just (pk, nospam) -> call tox $ either (Left . Just) (const $ Right pk) <$> Messenger.addFriendWithRequest pk nospam message

-- | The longest friend request message, in bytes.
maxFriendRequestLength :: Int
maxFriendRequestLength = Messenger.maxFriendRequestLength

-- | Add a friend who added us, or accept their friend request.
friendAddNoRequest :: Tox -> PublicKey -> IO (Either Messenger.AddFriendError ())
friendAddNoRequest tox pk = call tox $ do
  known <- Messenger.getsMessenger (Map.member pk . Messenger.messengerFriends)
  ourPk <- KeyPair.publicKey <$> Connection.getsConn Connection.ourRealKeys
  if | known       -> return $ Left Messenger.AlreadyFriend
     | pk == ourPk -> return $ Left Messenger.OwnKey
     | otherwise   -> Right <$> Messenger.addFriend pk

friendDelete :: Tox -> PublicKey -> IO Bool
friendDelete tox pk = call tox $ do
  modify $ \gs -> gs { friendLinks = Map.delete pk (friendLinks gs) }
  Messenger.deleteFriend pk <* Conference.releaseFriendConnection pk

friendList :: Tox -> IO [PublicKey]
friendList tox = call tox $ Messenger.getsMessenger (Map.keys . Messenger.messengerFriends)

friendInfo :: Tox -> PublicKey -> IO (Maybe FriendInfo)
friendInfo tox pk = call tox $ do
  mFriend <- Messenger.getsMessenger (Map.lookup pk . Messenger.messengerFriends)
  forM mFriend $ \f -> do
    conn <- if Messenger.friendOnline f then getFriendConnection pk else return NotConnected
    now <- liftIO unixTime
    return FriendInfo
      { friendPublicKey     = pk
      , friendName          = Messenger.friendName f
      , friendStatusMessage = Messenger.friendMsg f
      , friendStatus        = Messenger.friendStatus f
      , friendTyping        = Messenger.friendTyping f
      , friendConnection    = conn
      , friendLastSeen      = if conn == NotConnected then Messenger.friendLastSeen f else now
      }

-- | Send a message to an online friend: its id for 'FriendReadReceipt'.
friendSendMessage :: Tox -> PublicKey -> Messenger.MessageType -> BS.ByteString -> IO (Either Messenger.SendMessageError Word32)
friendSendMessage tox pk ty = call tox . Messenger.sendMessage pk ty

-- | The longest message, in bytes.
maxMessageLength :: Int
maxMessageLength = Messenger.maxMessageLength

friendSetTyping :: Tox -> PublicKey -> Bool -> IO Bool
friendSetTyping tox pk = call tox . Messenger.setTyping pk

-- | Packets of the client's own protocols, their id included: a lossless id
-- toxcore doesn't use, or a lossy id from 192 to 254.
friendSendLosslessPacket, friendSendLossyPacket :: Tox -> PublicKey -> BS.ByteString -> IO (Either Messenger.CustomPacketError ())
friendSendLosslessPacket tox pk = call tox . Messenger.sendCustomPacket clientLossless True pk
friendSendLossyPacket tox pk = call tox . Messenger.sendCustomPacket clientLossy False pk

-- | The same without waiting for the main loop: a packet it refuses is
-- dropped. Packets go out in the order they were queued.
friendQueueLosslessPacket, friendQueueLossyPacket :: Tox -> PublicKey -> BS.ByteString -> IO ()
friendQueueLosslessPacket tox pk = cast tox . void . Messenger.sendCustomPacket clientLossless True pk
friendQueueLossyPacket tox pk = cast tox . void . Messenger.sendCustomPacket clientLossy False pk


{-------------------------------------------------------------------------------
 -
 - :: Files.
 -
 ------------------------------------------------------------------------------}

-- | Offer a friend a file of a kind and size, with a name.  Without an id
-- the file gets a random one.  The friend's 'FileControlChanged' says when
-- it accepts, and 'FileChunkRequest' asks for the data.
fileSend :: Tox -> PublicKey -> Word32 -> Word64 -> Maybe BS.ByteString -> BS.ByteString
         -> IO (Either Messenger.FileError Word8)
fileSend tox pk kind size fileId name = call tox $ do
  fid <- maybe (randomBytes Messenger.fileIdLength) return fileId
  Messenger.sendFile pk kind size fid name

-- | Accept, resume, pause or cancel a transfer.
fileControl :: Tox -> PublicKey -> Messenger.FileRef -> Messenger.FileControl -> IO (Either Messenger.FileError ())
fileControl tox pk ref = call tox . Messenger.controlFile pk ref

-- | Ask for an offered file from a position, before accepting it.
fileSeek :: Tox -> PublicKey -> Word8 -> Word64 -> IO (Either Messenger.FileError ())
fileSeek tox pk n = call tox . Messenger.seekFile pk n

fileSendChunk :: Tox -> PublicKey -> Word8 -> Word64 -> BS.ByteString -> IO (Either Messenger.FileError ())
fileSendChunk tox pk n position = call tox . Messenger.sendFileChunk pk n position

{-------------------------------------------------------------------------------
 -
 - :: Conferences.
 -
 ------------------------------------------------------------------------------}

data ConferencePeer = ConferencePeer
  { conferencePeerNumber    :: Conference.PeerNumber
  , conferencePeerPublicKey :: PublicKey
  , conferencePeerName      :: BS.ByteString
  } deriving (Eq, Show)

-- | A new text conference.  'Nothing' if we are in too many.
conferenceNew :: Tox -> IO (Maybe Conference.GroupNumber)
conferenceNew tox = call tox Conference.newConference

conferenceInvite :: Tox -> PublicKey -> Conference.GroupNumber -> IO Bool
conferenceInvite tox friend = call tox . Conference.invite friend

-- | Join with the cookie of a 'ConferenceInvite'.
conferenceJoin :: Tox -> PublicKey -> BS.ByteString -> IO (Either Conference.JoinError Conference.GroupNumber)
conferenceJoin tox friend = call tox . Conference.join friend

-- | Leave a conference for good.
conferenceDelete :: Tox -> Conference.GroupNumber -> IO Bool
conferenceDelete tox g = call tox $ Conference.leave g True

conferenceSendMessage :: Tox -> Conference.GroupNumber -> Messenger.MessageType -> BS.ByteString -> IO Bool
conferenceSendMessage tox g ty = call tox . Conference.sendText g ty

conferenceSetTitle :: Tox -> Conference.GroupNumber -> BS.ByteString -> IO Bool
conferenceSetTitle tox g = call tox . Conference.changeTitle g

conferenceTitle :: Tox -> Conference.GroupNumber -> IO (Maybe BS.ByteString)
conferenceTitle tox g = call tox $ fmap Conference.cfTitle <$> Conference.getConference g

conferencePeers :: Tox -> Conference.GroupNumber -> IO [ConferencePeer]
conferencePeers tox g = call tox $ maybe [] peers <$> Conference.getConference g
  where
    peers c = [ ConferencePeer n (Conference.peerRealPk p) (Conference.peerName p) | (n, p) <- Map.toList (Conference.cfPeers c) ]

conferenceList :: Tox -> IO [Conference.GroupNumber]
conferenceList tox = call tox $ Map.keys . Conference.conferences <$> Conference.getConferences

-- | The conference's type and id, which stay the same between runs.
conferenceId :: Tox -> Conference.GroupNumber -> IO (Maybe BS.ByteString)
conferenceId tox g = call tox $ fmap Conference.identifier <$> Conference.getConference g

-- | One conference as toxcore's save file holds it, for clients that keep
-- their own state.
conferenceSave :: Tox -> Conference.GroupNumber -> IO (Maybe BS.ByteString)
conferenceSave tox g = call tox $ do
  unixAt <- unixClock
  (>>= fmap (LBS.toStrict . Binary.encode) . Save.savedConference unixAt) <$> Conference.getConference g

-- | Rejoin a conference from 'conferenceSave', unless we are in it.
conferenceRestore :: Tox -> BS.ByteString -> IO (Maybe Conference.GroupNumber)
conferenceRestore tox bytes = case Binary.decodeOrFail (LBS.fromStrict bytes) of
  Right (rest, _, saved) | LBS.null rest -> call tox $ Conference.restoreConference saved
  _ -> return Nothing

{-------------------------------------------------------------------------------
 -
 - :: Groups.
 -
 ------------------------------------------------------------------------------}

data GroupPeer = GroupPeer
  { groupPeerId     :: Group.PeerId
  , groupPeerKey    :: PublicKey
  , groupPeerName   :: BS.ByteString
  , groupPeerStatus :: Word8
  , groupPeerRole   :: Group.Role
  } deriving (Eq, Show)

-- | Found a group, private (joined by invite) or public, with its name and
-- our nick in it.
groupNew :: Tox -> Bool -> BS.ByteString -> BS.ByteString -> IO (Maybe Group.GroupNumber)
groupNew tox private name = call tox . Group.createGroup private name

-- | Join a public group by its chat id, with our nick and its password if
-- it has one.
groupJoin :: Tox -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either Group.AcceptError Group.GroupNumber)
groupJoin tox chatId nick = call tox . Group.joinPublicGroup chatId nick

groupChatId :: Tox -> Group.GroupNumber -> IO (Maybe BS.ByteString)
groupChatId tox g = call tox $ fmap Group.grChatId <$> Group.getGroup g

groupInviteFriend :: Tox -> Group.GroupNumber -> PublicKey -> IO Bool
groupInviteFriend tox g = call tox . Group.inviteFriend g

-- | Join a group with the invite data of a 'GroupInvite', our nick and the
-- group's password if it has one.
groupInviteAccept :: Tox -> PublicKey -> BS.ByteString -> BS.ByteString -> BS.ByteString
                  -> IO (Either Group.AcceptError Group.GroupNumber)
groupInviteAccept tox friend invite nick = call tox . Group.acceptInvite friend invite nick

-- | Send a message to the group: its id, as peers see it.
groupSendMessage :: Tox -> Group.GroupNumber -> Messenger.MessageType -> BS.ByteString -> IO (Either Group.GroupSendError Word32)
groupSendMessage tox g ty = call tox . Group.sendMessage g ty

groupSendPrivateMessage :: Tox -> Group.GroupNumber -> Group.PeerId -> Messenger.MessageType -> BS.ByteString
                        -> IO (Either Group.GroupSendError Word32)
groupSendPrivateMessage tox g n ty = call tox . Group.sendPrivateMessage g n ty

-- | The client's own data for everyone in the group, lossless or not.
groupSendCustomPacket :: Tox -> Group.GroupNumber -> Bool -> BS.ByteString -> IO (Either Group.GroupSendError ())
groupSendCustomPacket tox g lossless = call tox . Group.sendCustomPacket g Nothing lossless

groupSendCustomPrivatePacket :: Tox -> Group.GroupNumber -> Group.PeerId -> Bool -> BS.ByteString -> IO (Either Group.GroupSendError ())
groupSendCustomPrivatePacket tox g n lossless = call tox . Group.sendCustomPacket g (Just n) lossless

groupSetTopic :: Tox -> Group.GroupNumber -> BS.ByteString -> IO Bool
groupSetTopic tox g = call tox . Group.setTopic g

-- | Our role in the group.
groupSelfRole :: Tox -> Group.GroupNumber -> IO (Maybe Group.Role)
groupSelfRole tox g = call tox $ fmap Group.selfRole <$> Group.getGroup g

-- | What the founder sets for everyone, as the group's shared state has it.
data GroupSettings = GroupSettings
  { groupPrivate   :: Bool
  , groupPassword  :: BS.ByteString
  , groupPeerLimit :: Word16
  , groupTopicLock :: Bool
  , groupVoice     :: Group.Voice
  } deriving (Eq, Show)

-- | The settings once we have the group's shared state.
groupSettings :: Tox -> Group.GroupNumber -> IO (Maybe GroupSettings)
groupSettings tox g = call tox $ (>>= (settings . fst <=< Group.grSharedState)) <$> Group.getGroup g
  where
    settings s = GroupSettings (GroupPackets.ssPrivacy s == 1) (GroupPackets.ssPassword s) (GroupPackets.ssMaxPeers s)
      (GroupPackets.ssTopicLock s == 0) <$> Group.voiceOf (GroupPackets.ssVoice s)

-- | Whether 'groupSetTopic' would be taken, as our role and the topic lock
-- stand now.
groupMayTopic :: Tox -> Group.GroupNumber -> IO Bool
groupMayTopic tox g = call tox $ maybe False Group.mayTopic <$> Group.getGroup g

groupTopic :: Tox -> Group.GroupNumber -> IO (Maybe BS.ByteString)
groupTopic tox g = call tox $ (>>= fmap (GroupPackets.tiTopic . fst) . Group.grTopic) <$> Group.getGroup g

groupName :: Tox -> Group.GroupNumber -> IO (Maybe BS.ByteString)
groupName tox g = call tox $ (>>= fmap (GroupPackets.ssGroupName . fst) . Group.grSharedState) <$> Group.getGroup g

groupSetNick :: Tox -> Group.GroupNumber -> BS.ByteString -> IO Bool
groupSetNick tox g = call tox . Group.setNick g

-- | The peers that joined, without us.
groupPeers :: Tox -> Group.GroupNumber -> IO [GroupPeer]
groupPeers tox g = call tox $ maybe [] peers <$> Group.getGroup g
  where
    peers gr = [ GroupPeer (Group.peerId p) (Group.peerEncPk p) (Group.peerNick p) (Group.peerStatus p) (Group.peerRole gr p)
               | p <- Group.confirmedPeers gr ]

-- | Leave a group with a parting message.
groupLeave :: Tox -> Group.GroupNumber -> BS.ByteString -> IO Bool
groupLeave tox g = call tox . Group.leave g

groupKickPeer :: Tox -> Group.GroupNumber -> Group.PeerId -> IO (Either Group.ModError ())
groupKickPeer tox g = call tox . Group.kickPeer g

groupSetRole :: Tox -> Group.GroupNumber -> Group.PeerId -> Group.Role -> IO (Either Group.ModError ())
groupSetRole tox g n = call tox . Group.setRole g n

-- | The founder's settings: each change goes to every peer.
groupSetPassword :: Tox -> Group.GroupNumber -> BS.ByteString -> IO Bool
groupSetPassword tox g = call tox . Group.setPassword g

groupSetPeerLimit :: Tox -> Group.GroupNumber -> Word16 -> IO Bool
groupSetPeerLimit tox g = call tox . Group.setPeerLimit g

groupSetPrivacy :: Tox -> Group.GroupNumber -> Bool -> IO Bool
groupSetPrivacy tox g = call tox . Group.setPrivacy g

groupSetTopicLock :: Tox -> Group.GroupNumber -> Bool -> IO Bool
groupSetTopicLock tox g = call tox . Group.setTopicLock g

groupSetVoice :: Tox -> Group.GroupNumber -> Group.Voice -> IO Bool
groupSetVoice tox g = call tox . Group.setVoice g

groupList :: Tox -> IO [Group.GroupNumber]
groupList tox = call tox $ Map.keys . Group.groups <$> Group.getGroups

-- | One group as toxcore's save file holds it, with its keys, once we know
-- its shared state.
groupSave :: Tox -> Group.GroupNumber -> IO (Maybe BS.ByteString)
groupSave tox g = call tox $ (>>= fmap (LBS.toStrict . Binary.encode . PG.Groups . pure) . Group.saveGroup) <$> Group.getGroup g

-- | Rejoin a group from 'groupSave', unless we are in it.
groupRestore :: Tox -> BS.ByteString -> IO (Maybe Group.GroupNumber)
groupRestore tox bytes = case Binary.decodeOrFail (LBS.fromStrict bytes) of
  Right (rest, _, PG.Groups [saved]) | LBS.null rest -> call tox $ Group.restoreGroup saved
  _ -> return Nothing
