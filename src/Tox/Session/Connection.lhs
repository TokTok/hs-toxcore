\begin{code}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tox.Session.Connection where

import           Control.Monad                      (forM_, guard, join, unless,
                                                     void, when)
import           Control.Monad.Logger               (MonadLogger, logInfoN)
import           Control.Monad.State                (StateT, runStateT)
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Get                    as Get
import qualified Data.Binary.Put                    as Put
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Foldable                      (find)
import           Data.List                          (partition)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe, isJust,
                                                     isNothing, mapMaybe)
import           Data.Text                          (pack)
import           Data.Word                          (Word32, Word8)

import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (TimeDiff, Timestamp)
import qualified Tox.Core.Timed                     as Timed
import           Tox.Core.Timed                     (Timed)
import           Tox.Crypto.Core.Key                (CombinedKey, PublicKey)
import           Tox.Crypto.Core.Keyed              (Keyed)
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes)
import qualified Tox.DHT.DhtState                   as DhtState
import qualified Tox.DHT.Operation                  as DHT
import qualified Tox.Network.Core.HostAddress       as HostAddress
import           Tox.Network.Core.Networked         (Networked)
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo)
import           Tox.Network.Core.Packet            (Packet (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Network.TCP.Connections        as Tcp
import qualified Tox.Onion.Operation                as Onion
import qualified Tox.Transport.SecureSession        as SecureSession
import           Tox.Transport.SecureSession        (SecureSessionState,
                                                     SessionEvent (..))

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

packetIdAlive, packetIdShareRelays, packetIdFriendRequests :: Word8
packetIdAlive          = 16
packetIdShareRelays    = 17
packetIdFriendRequests = 18

packetIds :: [Word8]
packetIds = [packetIdAlive, packetIdShareRelays, packetIdFriendRequests]

friendPingInterval :: TimeDiff
friendPingInterval = Time.seconds 8

friendConnectionTimeout :: TimeDiff
friendConnectionTimeout = Time.seconds 32

shareRelaysInterval :: TimeDiff
shareRelaysInterval = Time.seconds 120

maxSharedRelays, maxStoredRelays :: Int
maxSharedRelays = 3
maxStoredRelays = 24

data FriendConnection = FriendConnection
  { fcRealPk         :: PublicKey
  , fcDhtPk          :: Maybe PublicKey
  , fcIpPort         :: Maybe NodeInfo -- ^ Direct UDP address of the friend's DHT node.
  , fcRelays         :: [NodeInfo]     -- ^ TCP relays the friend said it is connected to.
  , fcSession        :: Maybe SecureSessionState
  , fcOnline         :: Bool
  , fcPingLastSent   :: Maybe Timestamp
  , fcPingLastRecv   :: Maybe Timestamp
  , fcRelaysLastSent :: Maybe Timestamp
  } deriving (Eq, Show)

data ConnectionManager = ConnectionManager
  { friends     :: Map PublicKey FriendConnection
  , ourRealKeys :: KeyPair
  , ourDhtKeys  :: KeyPair
  , cookieKey   :: CombinedKey
  }

-- | What friend connections report to Messenger.
data ConnectionEvent
  = FriendOnline PublicKey
  | FriendOffline PublicKey
  | FriendData PublicKey ByteString
  | FriendLossyData PublicKey ByteString
  | FriendRequest PublicKey ByteString -- ^ Sender, then nospam and message.
  deriving (Eq, Show)


class (Monad m, Timed m, MonadRandomBytes m, Keyed m, Networked m, MonadLogger m, Tcp.TcpMonad m) => ConnectionMonad m where
  getConnManager :: m ConnectionManager
  putConnManager :: ConnectionManager -> m ()

getsConn :: ConnectionMonad m => (ConnectionManager -> a) -> m a
getsConn f = f <$> getConnManager

modifyConn :: ConnectionMonad m => (ConnectionManager -> ConnectionManager) -> m ()
modifyConn f = getConnManager >>= putConnManager . f

modifyFriend :: ConnectionMonad m => PublicKey -> (FriendConnection -> FriendConnection) -> m ()
modifyFriend pk f = modifyConn $ \s -> s { friends = Map.adjust f pk (friends s) }


-- | Initialise a new connection manager.
initManager :: KeyPair -> KeyPair -> CombinedKey -> ConnectionManager
initManager realKeys dhtKeys cKey = ConnectionManager
  { friends = Map.empty
  , ourRealKeys = realKeys
  , ourDhtKeys = dhtKeys
  , cookieKey = cKey
  }


-- | Add a new friend to be managed.
addFriend :: ConnectionMonad m => PublicKey -> m ()
addFriend realPk = do
  logInfoN $ "Adding friend: " <> pack (show realPk)
  modifyConn $ \s ->
    let newFriend = FriendConnection realPk Nothing Nothing [] Nothing False Nothing Nothing Nothing
    in s { friends = Map.insertWith (\_ old -> old) realPk newFriend (friends s) }

-- | The friend's DHT public key, found by the onion or another way.  A
-- different key means the friend restarted, so the old session goes.
setFriendDhtPk :: ConnectionMonad m => PublicKey -> PublicKey -> m [ConnectionEvent]
setFriendDhtPk realPk dhtPk = do
  mFriend <- getsConn (Map.lookup realPk . friends)
  case mFriend of
    Just f | fcDhtPk f /= Just dhtPk -> do
      events <- killSession realPk
      mapM_ Tcp.removePeer (fcDhtPk f)
      modifyFriend realPk $ \f' -> f' { fcDhtPk = Just dhtPk, fcIpPort = Nothing, fcRelays = [] }
      return events
    _ -> return []

-- | A direct UDP address of the friend's DHT node.
setFriendIpPort :: ConnectionMonad m => PublicKey -> NodeInfo -> m ()
setFriendIpPort realPk node = modifyFriend realPk $ \f ->
  if fcDhtPk f == Just (NodeInfo.publicKey node) then f { fcIpPort = Just node } else f

-- | Send a friend request: over the session if we are connected, otherwise
-- through the onion.
sendFriendRequestPacket :: (ConnectionMonad m, Onion.OnionNodeMonad m) => PublicKey -> ByteString -> m Bool
sendFriendRequestPacket pk request = do
  online <- getsConn (maybe False fcOnline . Map.lookup pk . friends)
  if online
  then sendFriendData pk (BS.cons packetIdFriendRequests request)
  else (> 0) <$> Onion.sendToFriend pk (BS.cons Onion.packetIdFriendRequest request)

-- | Send lossless data to an online friend.
sendFriendData :: ConnectionMonad m => PublicKey -> ByteString -> m Bool
sendFriendData pk bytes = isJust <$> sendFriendDataNumbered pk bytes

sendFriendDataNumbered :: ConnectionMonad m => PublicKey -> ByteString -> m (Maybe Word32)
sendFriendDataNumbered pk bytes = join <$> withSession pk (SecureSession.sendLossless bytes)

-- | Send lossless data that counts against the congestion control.
sendFriendLossy :: ConnectionMonad m => PublicKey -> ByteString -> m Bool
sendFriendLossy pk bytes = or <$> withSession pk (SecureSession.sendLossy bytes)

sendFriendDataCongested :: ConnectionMonad m => PublicKey -> ByteString -> m (Maybe Word32)
sendFriendDataCongested pk bytes = join <$> withSession pk (SecureSession.sendCongested bytes)

-- | How many more packets subject to congestion control we may send the
-- friend now.
freeSendSlots :: ConnectionMonad m => PublicKey -> m Int
freeSendSlots pk = getsConn $ \s -> maybe 0 SecureSession.freeSendSlots (Map.lookup pk (friends s) >>= fcSession)

isDelivered :: ConnectionMonad m => PublicKey -> Word32 -> m Bool
isDelivered pk num = getsConn $ \s ->
  maybe False (SecureSession.isDelivered num) (Map.lookup pk (friends s) >>= fcSession)

-- | How many sent lossless packets the friend has not confirmed yet.
sendQueueLength :: ConnectionMonad m => PublicKey -> m Int
sendQueueLength pk = getsConn $ \s -> maybe 0 SecureSession.sendQueueLength (Map.lookup pk (friends s) >>= fcSession)

-- | How we reach an online friend.
friendLink :: ConnectionMonad m => PublicKey -> m (Maybe NodeInfo)
friendLink pk = getsConn $ \s -> do
  f <- Map.lookup pk (friends s)
  guard $ fcOnline f
  SecureSession.ssPeerNodeInfo <$> fcSession f

-- | Stop connecting to a friend.
removeFriend :: (ConnectionMonad m, Onion.OnionNodeMonad m, DHT.DhtNodeMonad m) => PublicKey -> m ()
removeFriend pk = do
  mFriend <- getsConn (Map.lookup pk . friends)
  forM_ mFriend $ \f -> do
    _ <- killSession pk
    forM_ (fcDhtPk f) $ \dhtPk -> do
      DHT.modifyDht $ DhtState.removeSearchKey dhtPk
      Tcp.removePeer dhtPk
    Onion.removeFriend pk
    modifyConn $ \s -> s { friends = Map.delete pk (friends s) }


-- | Run a session action for a friend that has a session.
withSession :: ConnectionMonad m => PublicKey -> StateT SecureSessionState m a -> m (Maybe a)
withSession pk action = do
  mSession <- getsConn (\s -> Map.lookup pk (friends s) >>= fcSession)
  case mSession of
    Nothing -> return Nothing
    Just ss -> do
      (a, ss') <- runStateT action ss
      modifyFriend pk $ \f -> f { fcSession = Just ss' }
      return $ Just a

runSession :: ConnectionMonad m => PublicKey -> StateT SecureSessionState m [SessionEvent] -> m [ConnectionEvent]
runSession pk action = withSession pk action >>= fmap concat . mapM (sessionEvent pk) . fromMaybe []

killSession :: ConnectionMonad m => PublicKey -> m [ConnectionEvent]
killSession pk = do
  _ <- withSession pk SecureSession.sendKill
  dropSession pk

dropSession :: ConnectionMonad m => PublicKey -> m [ConnectionEvent]
dropSession pk = do
  wasOnline <- getsConn (maybe False fcOnline . Map.lookup pk . friends)
  modifyFriend pk $ \f -> f { fcSession = Nothing, fcOnline = False }
  return [FriendOffline pk | wasOnline]

sessionEvent :: ConnectionMonad m => PublicKey -> SessionEvent -> m [ConnectionEvent]
sessionEvent pk event = case event of
  SessionEstablished -> do
    now <- Timed.askTime
    modifyFriend pk $ \f -> f { fcOnline = True, fcPingLastRecv = Just now, fcPingLastSent = Nothing, fcRelaysLastSent = Nothing }
    return [FriendOnline pk]
  SessionKilled -> dropSession pk
  SessionDhtPkChanged dhtPk -> setFriendDhtPk pk dhtPk
  SessionLossyData bytes -> return [FriendLossyData pk bytes]
  SessionLosslessData bytes -> case BS.uncons bytes of
    Just (ident, _)
      | ident == packetIdAlive -> do
          now <- Timed.askTime
          modifyFriend pk $ \f -> f { fcPingLastRecv = Just now }
          return []
      | ident == packetIdFriendRequests -> return [FriendRequest pk (BS.drop 1 bytes)]
      | ident == packetIdShareRelays -> do
          mapM_ (addFriendRelay pk) $ filter ((== TCP) . NodeInfo.protocol) $ decodeNodes (BS.drop 1 bytes)
          return []
    _ -> return [FriendData pk bytes]


-- | Handle a net_crypto packet.  Packets for a session are found by the
-- link they came from.  A handshake from elsewhere may start a session.
handlePacket :: ConnectionMonad m => NodeInfo -> Packet ByteString -> m [ConnectionEvent]
handlePacket from pkt@(Packet kind payload) = do
  cm <- getConnManager
  -- Packets also come from the friend's DHT address when it moves off a relay,
  -- and through any relay the friend shares with us, not only the session's.
  let fromSession f = case fcSession f of
        Nothing -> False
        Just ss -> sameLink from (SecureSession.ssPeerNodeInfo ss)
          || (NodeInfo.protocol from == UDP && (NodeInfo.address <$> fcIpPort f) == Just (NodeInfo.address from))
          || (NodeInfo.protocol from == TCP && fcDhtPk f == Just (NodeInfo.publicKey from))
  case (kind, find fromSession (friends cm)) of
    (PacketKind.CookieRequest, _) -> do
      SecureSession.handleCookieRequest (cookieKey cm) (ourDhtKeys cm) from payload
      return []
    (_, Just f) -> runSession (fcRealPk f) (SecureSession.handlePacket (cookieKey cm) from pkt)
    (PacketKind.CryptoHandshake, Nothing) -> handleNewHandshake from payload
    _ -> return []

-- | Whether two nodes are the same link: a UDP address, or a relay together
-- with the peer's DHT key.
sameLink :: NodeInfo -> NodeInfo -> Bool
sameLink a b =
  NodeInfo.protocol a == NodeInfo.protocol b && NodeInfo.address a == NodeInfo.address b
    && (NodeInfo.protocol a == UDP || NodeInfo.publicKey a == NodeInfo.publicKey b)

-- | A handshake that does not belong to a session, from a friend.  As in
-- toxcore, a new DHT key means the friend restarted, so it replaces the
-- session, and without one it starts a session.  A session that still waits
-- for the friend's handshake takes it.  One further along keeps going: the
-- friend sends its handshake through every relay, and a new session would
-- have keys the friend doesn't use.
handleNewHandshake :: ConnectionMonad m => NodeInfo -> ByteString -> m [ConnectionEvent]
handleNewHandshake from payload = do
  cm <- getConnManager
  opened <- SecureSession.openHandshake (cookieKey cm) (KeyPair.secretKey $ ourRealKeys cm) payload
  case opened of
    Nothing -> return []
    Just oh -> case Map.lookup (SecureSession.ohPeerRealPk oh) (friends cm) of
      Just f
        | fcDhtPk f /= Just (SecureSession.ohPeerDhtPk oh) || isNothing (fcSession f) -> do
            let pk = fcRealPk f
                dhtNode = from { NodeInfo.publicKey = SecureSession.ohPeerDhtPk oh }
            logInfoN $ "Accepting session from " <> pack (show pk) <> " at " <> pack (show dhtNode)
            events <- setFriendDhtPk pk (SecureSession.ohPeerDhtPk oh)
            ss <- SecureSession.acceptSession (cookieKey cm) (ourRealKeys cm) (ourDhtKeys cm) from oh
            modifyFriend pk $ \f' -> f' { fcSession = Just ss, fcIpPort = if NodeInfo.protocol from == UDP then Just dhtNode else fcIpPort f' }
            return events
        | maybe False SecureSession.awaitsHandshake (fcSession f) ->
            [] <$ withSession (fcRealPk f) (SecureSession.adoptHandshake (cookieKey cm) from oh)
      _ -> return []


-- | The onion found a friend's DHT public key: ask the nodes the friend sent
-- for the friend's address.
handleOnionEvent :: (ConnectionMonad m, DHT.DhtNodeMonad m) => Onion.OnionEvent -> m [ConnectionEvent]
handleOnionEvent (Onion.FriendRequestReceived pk bytes) = return [FriendRequest pk bytes]
handleOnionEvent (Onion.GroupAnnouncesFound _ _) = return []
handleOnionEvent (Onion.FriendDhtPkFound pk dhtPk nodes) = do
  known <- getsConn (Map.member pk . friends)
  if not known then return [] else do
    events <- setFriendDhtPk pk dhtPk
    now <- Timed.askTime
    DHT.modifyDht $ DhtState.addSearchKey now dhtPk
    let (relays, dhtNodes) = partition ((== TCP) . NodeInfo.protocol) nodes
    mapM_ (\node -> DHT.sendNodesRequest $ DHT.RequestInfo node dhtPk) dhtNodes
    mapM_ (addFriendRelay pk) relays
    return events

-- | A relay the friend is connected to, to be routed to them on.  A relay
-- the friend hosts itself is reached at the friend's address.
addFriendRelay :: ConnectionMonad m => PublicKey -> NodeInfo -> m ()
addFriendRelay pk relay0 = getsConn (Map.lookup pk . friends) >>= mapM_ (\f -> do
  let relay = case (NodeInfo.address relay0, NodeInfo.address <$> fcIpPort f) of
        (SocketAddress host port, Just (SocketAddress friendHost _))
          | HostAddress.isLoopback host, fcDhtPk f == Just (NodeInfo.publicKey relay0) ->
              relay0 { NodeInfo.address = SocketAddress friendHost port }
        _ -> relay0
      other = (/= NodeInfo.publicKey relay) . NodeInfo.publicKey
  modifyFriend pk $ \f' -> f' { fcRelays = take maxStoredRelays $ relay : filter other (fcRelays f') }
  mapM_ (`Tcp.addPeerRelay` relay) (fcDhtPk f))

decodeNodes :: ByteString -> [NodeInfo]
decodeNodes = either (const []) (\(_, _, nodes) -> nodes) . Get.runGetOrFail NodeInfo.getNodes . LBS.fromStrict


-- | Periodic work for all friends.
doFriendConnections :: (ConnectionMonad m, Onion.OnionNodeMonad m, DHT.DhtNodeMonad m) => m [ConnectionEvent]
doFriendConnections = do
  fs <- getsConn friends
  -- Only friends' current DHT keys are searched for.
  searched <- Map.keys . DhtState.dhtSearchList <$> DHT.getDht
  let current = mapMaybe fcDhtPk (Map.elems fs)
  mapM_ (DHT.modifyDht . DhtState.removeSearchKey) (filter (`notElem` current) searched)
  concat <$> mapM doFriendConnection (Map.keys fs)

doFriendConnection :: (ConnectionMonad m, Onion.OnionNodeMonad m, DHT.DhtNodeMonad m) => PublicKey -> m [ConnectionEvent]
doFriendConnection pk = do
  Onion.startFriendSearch pk
  cm <- getConnManager
  case Map.lookup pk (friends cm) of
    Nothing -> return []
    Just f -> Onion.setFriendOnline pk (fcOnline f) >> case (fcSession f, fcDhtPk f, fcIpPort f) of
      (Nothing, Nothing, _) -> return []
      (Nothing, Just dhtPk, Nothing) -> do
        -- The DHT searches for the friend's DHT node to learn its address.
        now <- Timed.askTime
        dht <- DHT.getDht
        unless (DhtState.containsSearchKey dhtPk dht) $ DHT.modifyDht (DhtState.addSearchKey now dhtPk)
        mapM_ (setFriendIpPort pk) (DhtState.findNode dhtPk dht)
        -- Without an address, go through a relay the friend is connected to.
        case fcRelays f of
          relay : _ | Nothing <- DhtState.findNode dhtPk dht -> connectThrough pk dhtPk (Tcp.peerLink relay dhtPk)
          _ -> return []
      (Nothing, Just dhtPk, Just node) -> connectThrough pk dhtPk node
      (Just ss, mDhtPk, _) -> do
        -- C only takes handshakes out of band, so the relay must connect us.
        let link = SecureSession.ssPeerNodeInfo ss
        when (NodeInfo.protocol link == TCP) $ Tcp.addPeerOnLink link
        dht <- DHT.getDht
        mapM_ (setFriendIpPort pk) (mDhtPk >>= (`DhtState.findNode` dht))
        events <- runSession pk SecureSession.maintainSession
        shareRelays pk
        (events ++) <$> pingFriend pk

connectThrough :: ConnectionMonad m => PublicKey -> PublicKey -> NodeInfo -> m [ConnectionEvent]
connectThrough pk dhtPk link = do
  cm <- getConnManager
  logInfoN $ "Connecting to " <> pack (show pk) <> " at " <> pack (show link)
  ss <- SecureSession.initSession (ourRealKeys cm) pk (ourDhtKeys cm) dhtPk link
  modifyFriend pk $ \f -> f { fcSession = Just ss }
  runSession pk SecureSession.maintainSession

-- | Tell an online friend the relays we are connected to, and have it routed
-- to us on them.
shareRelays :: ConnectionMonad m => PublicKey -> m ()
shareRelays pk = do
  now <- Timed.askTime
  mFriend <- getsConn (Map.lookup pk . friends)
  relays <- take maxSharedRelays <$> Tcp.connectedRelays
  case mFriend of
    Just f | fcOnline f, not (null relays), Time.due now shareRelaysInterval (fcRelaysLastSent f) -> do
      mapM_ (addFriendRelay pk) relays
      let packet = LBS.toStrict $ Put.runPut $ Put.putWord8 packetIdShareRelays >> mapM_ Binary.put relays
      sent <- sendFriendData pk packet
      when sent $ modifyFriend pk $ \f' -> f' { fcRelaysLastSent = Just now }
    _ -> return ()

-- | Keep an online friend's connection alive, or drop it if the friend stopped
-- answering.
pingFriend :: ConnectionMonad m => PublicKey -> m [ConnectionEvent]
pingFriend pk = do
  now <- Timed.askTime
  mFriend <- getsConn (Map.lookup pk . friends)
  case mFriend of
    Just f | fcOnline f -> do
      let due = Time.due now friendPingInterval
      if maybe False (\t -> Time.addTime t friendConnectionTimeout < now) (fcPingLastRecv f)
      then do
        logInfoN $ "Friend connection timed out: " <> pack (show pk)
        killSession pk
      else do
        when (due $ fcPingLastSent f) $ do
          sent <- sendFriendData pk (BS.singleton packetIdAlive)
          when sent $ modifyFriend pk $ \f' -> f' { fcPingLastSent = Just now }
          -- On a relay, try the friend's direct address too.
          case (NodeInfo.protocol . SecureSession.ssPeerNodeInfo <$> fcSession f, fcIpPort f) of
            (Just TCP, Just node) -> void $ withSession pk (SecureSession.sendLosslessTo node (BS.singleton packetIdAlive))
            _ -> return ()
        return []
    _ -> return []

\end{code}

\chapter{Friend connection}

\texttt{friend\_connection} is the module that sits on top of the DHT, onion and
\texttt{net\_crypto} modules and takes care of linking the 3 together.

Friends in \texttt{friend\_connection} are represented by their real public key.
When a friend is added in \texttt{friend\_connection}, an onion search entry is
created for that friend.  This means that the onion module will start looking
for this friend and send that friend their DHT public key, and the TCP relays
it is connected to, in case a connection is only possible with TCP.

Once the onion returns the DHT public key of the peer, the DHT public key is
saved, added to the DHT friends list and a new \texttt{net\_crypto} connection
is created.  Any TCP relays returned by the onion for this friend are passed to
the \texttt{net\_crypto} connection.

If the DHT establishes a direct UDP connection with the friend,
\texttt{friend\_connection} will pass the IP/port of the friend to
\texttt{net\_crypto} and also save it to be used to reconnect to the friend if
they disconnect.

If \texttt{net\_crypto} finds that the friend has a different DHT public key,
which can happen if the friend restarted their client, \texttt{net\_crypto} will
pass the new DHT public key to the onion module and will remove the DHT entry
for the old DHT public key and replace it with the new one.  The current
\texttt{net\_crypto} connection will also be killed and a new one with the
correct DHT public key will be created.

When the \texttt{net\_crypto} connection for a friend goes online,
\texttt{friend\_connection} will tell the onion module that the friend is online
so that it can stop spending resources looking for the friend.  When the friend
connection goes offline, \texttt{friend\_connection} will tell the onion module
so that it can start looking for the friend again.

There are 2 types of data packets sent to friends with the \texttt{net\_crypto}
connection handled at the level of \texttt{friend\_connection}, Alive packets
and TCP relay packets.  Alive packets are packets with the packet id or first
byte of data (only byte in this packet) being 16.  They are used in order to
check if the other friend is still online.  \texttt{net\_crypto} does not have
any timeout when the connection is established so timeouts are caught using
this packet.  In toxcore, this packet is sent every 8 seconds.  If none of
these packets are received for 32 seconds, the connection is timed out and
killed.  These numbers seem to cause the least issues and 32 seconds is not too
long so that, if a friend times out, toxcore won't falsely see them online for
too long.  Usually when a friend goes offline they have time to send a
disconnect packet in the \texttt{net\_crypto} connection which makes them appear
offline almost instantly.

The timeout for when to stop retrying to connect to a friend by creating new
\texttt{net\_crypto} connections when the old one times out in toxcore is the
same as the timeout for DHT peers (122 seconds).  However, it is calculated
from the last time a DHT public key was received for the friend or time the
friend's \texttt{net\_crypto} connection went offline after being online.  The
highest time is used to calculate when the timeout is.  \texttt{net\_crypto}
connections will be recreated (if the connection fails) until this timeout.

\texttt{friend\_connection} sends a list of 3 relays (the same number as the
target number of TCP relay connections in \texttt{TCP\_connections}) to each
connected friend every 5 minutes in toxcore.  Immediately before sending the
relays, they are associated to the current \texttt{net\_crypto->TCP\_connections}
connection.  This facilitates connecting the two friends together using the
relays as the friend who receives the packet will associate the sent relays to
the \texttt{net\_crypto} connection they received it from.  When both sides do
this they will be able to connect to each other using the relays.  The packet
id or first byte of the packet of share relay packets is 0x11.  This is then
followed by some TCP relays stored in packed node format.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x11) \\
  variable           & TCP relays in packed node format (see DHT) \\
\end{tabular}

If local IPs are received as part of the packet, the local IP will be replaced
with the IP of the peer that sent the relay.  This is because we assume this is
the best way to attempt to connect to the TCP relay.  If the peer that sent the
relay is using a local IP, then the sent local IP should be used to connect to
the relay.

For all other data packets, are passed by \texttt{friend\_connection} up to the
upper Messenger module.  It also separates lossy and lossless packets from
\texttt{net\_crypto}.

Friend connection takes care of establishing the connection to the friend and
gives the upper messenger layer a simple interface to receive and send
messages, add and remove friends and know if a friend is connected (online) or
not connected (offline).
