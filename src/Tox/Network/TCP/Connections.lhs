\begin{code}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tox.Network.TCP.Connections where

import           Control.Monad                      (forM_, unless, when)
import           Control.Monad.Logger               (MonadLogger, logInfoN)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.List                          (find)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, mapMaybe)
import           Data.Text                          (pack)

import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (TimeDiff, Timestamp)
import           Tox.Core.Timed                     (Timed (..))
import           Tox.Crypto.Core.Key                (PublicKey)
import           Tox.Crypto.Core.KeyPair            (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..),
                                                     randomNonce, randomWord64)
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Network.TCP.Client             as Client
import           Tox.Network.TCP.Client             (RelayConnection,
                                                     RelayEvent (..))

-- | Opening, writing to and closing TCP connections to relays, named by the
-- relay's public key.  Received bytes go to 'handleRelayBytes', closed
-- connections to 'relayClosed'.
class Monad m => TcpNetworked m where
  tcpConnect :: NodeInfo -> m ()
  tcpSend    :: PublicKey -> ByteString -> m ()
  tcpClose   :: PublicKey -> m ()
  -- | Whether the connection holds more than the relay has taken.
  tcpBusy    :: PublicKey -> m Bool
  tcpBusy _ = return False

class (TcpNetworked m, Timed m, MonadRandomBytes m, MonadLogger m) => TcpMonad m where
  getTcpState  :: m TcpState
  putTcpState  :: TcpState -> m ()
  getTcpDhtKeys :: m KeyPair

data TcpState = TcpState
  { tsWanted   :: Map PublicKey NodeInfo        -- ^ Relays we want to stay connected to.
  , tsRelays   :: Map PublicKey RelayConnection -- ^ Open connections to relays.
  , tsPeers    :: Map PublicKey [NodeInfo]      -- ^ Peer key to the relays it should be routed on, newest first.
  , tsAttempts :: Map PublicKey Timestamp       -- ^ When we last tried to connect to a relay.
  }

-- | Relays a peer may be routed on, as in toxcore. Older ones are let go.
maxPeerRelays :: Int
maxPeerRelays = 6

-- | How long a relay has to accept our handshake, and how long we wait to
-- try one again.
connectTimeout :: TimeDiff
connectTimeout = Time.seconds 10

-- | A packet a peer sent us through a relay, with the link it came on.
data TcpEvent
  = TcpPacket NodeInfo ByteString
  | TcpOnionResponse ByteString
  deriving (Eq, Show)

emptyTcpState :: TcpState
emptyTcpState = TcpState Map.empty Map.empty Map.empty Map.empty

modifyTcp :: TcpMonad m => (TcpState -> TcpState) -> m ()
modifyTcp f = getTcpState >>= putTcpState . f

-- | The link to a peer through a relay: the relay's address with the peer's
-- DHT key.
peerLink :: NodeInfo -> PublicKey -> NodeInfo
peerLink relay = NodeInfo TCP (NodeInfo.address relay)

relayFor :: NodeInfo -> TcpState -> Maybe RelayConnection
relayFor link s = find ((== NodeInfo.address link) . NodeInfo.address . Client.rcRelay) (Map.elems $ tsRelays s)

-- | Relays we are connected to, as TCP nodes.
connectedRelays :: TcpMonad m => m [NodeInfo]
connectedRelays = do
  s <- getTcpState
  return [ (Client.rcRelay rc) { protocol = TCP } | rc <- Map.elems (tsRelays s), Client.isConfirmed rc ]

-- | Stay connected to a relay.
addRelay :: TcpMonad m => NodeInfo -> m ()
addRelay relay = do
  let pk = NodeInfo.publicKey relay
  modifyTcp $ \s -> s { tsWanted = Map.insert pk relay (tsWanted s) }
  open <- Map.member pk . tsRelays <$> getTcpState
  unless open $ connectRelay relay

connectRelay :: TcpMonad m => NodeInfo -> m ()
connectRelay relay = do
  now <- askTime
  modifyTcp $ \s -> s { tsAttempts = Map.insert (NodeInfo.publicKey relay) now (tsAttempts s) }
  dhtKeys <- getTcpDhtKeys
  tempKeys <- newKeyPair
  sentNonce <- randomNonce
  handshakeNonce <- randomNonce
  let (rc, handshake) = Client.newRelayConnection dhtKeys tempKeys sentNonce handshakeNonce relay
  modifyTcp $ \s -> s { tsRelays = Map.insert (NodeInfo.publicKey relay) rc (tsRelays s) }
  logInfoN $ "Connecting to TCP relay " <> pack (show relay)
  tcpConnect relay
  tcpSend (NodeInfo.publicKey relay) handshake

-- | Have a peer routed to us through a relay.
addPeerRelay :: TcpMonad m => PublicKey -> NodeInfo -> m ()
addPeerRelay peer relay = do
  let relayPk = NodeInfo.publicKey relay
  modifyTcp $ \s -> s { tsPeers = Map.insert peer
    (take maxPeerRelays $ relay : filter ((/= relayPk) . NodeInfo.publicKey) (Map.findWithDefault [] peer (tsPeers s))) (tsPeers s) }
  mRc <- Map.lookup relayPk . tsRelays <$> getTcpState
  case mRc of
    Nothing -> connectRelay relay
    Just rc -> when (Client.isConfirmed rc && not (Client.hasRoute peer rc)) $
      sendToRelay relayPk (Client.routingRequest peer)
  dropUnused

-- | Have the peer of a link routed on its relay.
addPeerOnLink :: TcpMonad m => NodeInfo -> m ()
addPeerOnLink link = do
  s <- getTcpState
  forM_ (relayFor link s) $ \rc -> do
    let relayPk = NodeInfo.publicKey (Client.rcRelay rc)
        known = maybe False (any ((== relayPk) . NodeInfo.publicKey)) $ Map.lookup (NodeInfo.publicKey link) (tsPeers s)
    unless known $ addPeerRelay (NodeInfo.publicKey link) (Client.rcRelay rc)

-- | Stop routing a peer to us.
removePeer :: TcpMonad m => PublicKey -> m ()
removePeer peer = do
  modifyTcp $ \s -> s { tsPeers = Map.delete peer (tsPeers s) }
  dropUnused

-- | The relays we want, ourselves or for a peer.
wantedRelays :: TcpState -> Map PublicKey NodeInfo
wantedRelays s = Map.union (tsWanted s) $ Map.fromList [ (NodeInfo.publicKey r, r) | rs <- Map.elems (tsPeers s), r <- rs ]

-- | Close the relays nobody wants any more.
dropUnused :: TcpMonad m => m ()
dropUnused = do
  s <- getTcpState
  let wanted = wantedRelays s
  mapM_ relayClosed [ pk | pk <- Map.keys (tsRelays s), Map.notMember pk wanted ]
  modifyTcp $ \s' -> s' { tsAttempts = Map.intersection (tsAttempts s') wanted }

-- | Encrypt and send a packet to a relay.
sendToRelay :: TcpMonad m => PublicKey -> ByteString -> m ()
sendToRelay relayPk bytes = do
  s <- getTcpState
  forM_ (Map.lookup relayPk (tsRelays s) >>= Client.encodePacket bytes) $ \(frame, rc) -> do
    putTcpState s { tsRelays = Map.insert relayPk rc (tsRelays s) }
    tcpSend relayPk frame

-- | Send a packet to a peer: on a relay that connected us to them, or else
-- out of band through the relay of the link.
sendToPeer :: TcpMonad m => NodeInfo -> ByteString -> m ()
sendToPeer link bytes = do
  s <- getTcpState
  let peer = NodeInfo.publicKey link
  case peerRoute link s of
    Just (relayPk, True)  -> mapM_ (sendToRelay relayPk) (routeTo peer relayPk s)
    Just (relayPk, False) -> mapM_ (sendToRelay relayPk) (Client.oobSend peer bytes)
    Nothing               -> return ()
  where
    routeTo peer relayPk s = Map.lookup relayPk (tsRelays s) >>= Client.routeData peer bytes

-- | The relay a packet to a peer goes through, and whether the relay routes
-- it to the peer rather than sending it out of band.
peerRoute :: NodeInfo -> TcpState -> Maybe (PublicKey, Bool)
peerRoute link s = case routed of
  relayPk : _ -> Just (relayPk, True)
  []          -> (\rc -> (NodeInfo.publicKey $ Client.rcRelay rc, False)) <$> relayFor link s
  where
    routed = [ NodeInfo.publicKey (Client.rcRelay rc) | rc <- Map.elems (tsRelays s)
             , isJust $ Client.routeData (NodeInfo.publicKey link) BS.empty rc ]

-- | Whether the relay a packet to the peer would go through is busy.
peerBusy :: TcpMonad m => NodeInfo -> m Bool
peerBusy link = getTcpState >>= maybe (return False) (tcpBusy . fst) . peerRoute link

-- | Send a packet to a peer by its key: on a relay that connected us to them,
-- or else out of band through the first of the given relays we are on.
-- 'False' if neither works.
sendToKey :: TcpMonad m => PublicKey -> [PublicKey] -> ByteString -> m Bool
sendToKey peer relays bytes = do
  s <- getTcpState
  let routed = [ (NodeInfo.publicKey (Client.rcRelay rc), packet)
               | rc <- Map.elems (tsRelays s), Just packet <- [Client.routeData peer bytes rc] ]
      oob = [ relayPk | relayPk <- relays, maybe False Client.isConfirmed (Map.lookup relayPk $ tsRelays s) ]
  case (routed, oob, Client.oobSend peer bytes) of
    ((relayPk, packet) : _, _, _)  -> True <$ sendToRelay relayPk packet
    ([], relayPk : _, Just packet) -> True <$ sendToRelay relayPk packet
    _                              -> return False

-- | Send an onion request through a relay, which acts as the first node.
sendOnionRequest :: TcpMonad m => NodeInfo -> ByteString -> m ()
sendOnionRequest relay = sendToRelay (NodeInfo.publicKey relay) . Client.onionRequest

-- | Take bytes received from a relay.
handleRelayBytes :: TcpMonad m => PublicKey -> ByteString -> m [TcpEvent]
handleRelayBytes relayPk bytes = do
  s <- getTcpState
  case Map.lookup relayPk (tsRelays s) of
    Nothing -> return []
    Just rc -> case Client.receiveBytes bytes rc of
      Left () -> [] <$ relayClosed relayPk
      Right (rc', events, replies) -> do
        putTcpState s { tsRelays = Map.insert relayPk rc' (tsRelays s) }
        mapM_ (sendToRelay relayPk) replies
        when (not (Client.isConfirmed rc) && Client.isConfirmed rc') $ relayConfirmed relayPk
        return $ mapMaybe (toEvent $ Client.rcRelay rc) events
  where
    toEvent relay = \case
      RelayData peer packet    -> Just $ TcpPacket (peerLink relay peer) packet
      RelayOob peer packet     -> Just $ TcpPacket (peerLink relay peer) packet
      RelayOnionResponse packet -> Just $ TcpOnionResponse packet
      RelayRouteChanged _ _    -> Nothing

-- | A relay accepted our handshake: confirm with a ping and ask for the peers
-- we want routed on it.
relayConfirmed :: TcpMonad m => PublicKey -> m ()
relayConfirmed relayPk = do
  logInfoN $ "Connected to TCP relay " <> pack (show relayPk)
  pingRelay relayPk
  peers <- Map.keys . Map.filter (any ((== relayPk) . NodeInfo.publicKey)) . tsPeers <$> getTcpState
  mapM_ (sendToRelay relayPk . Client.routingRequest) peers

pingRelay :: TcpMonad m => PublicKey -> m ()
pingRelay relayPk = do
  now <- askTime
  pingId <- max 1 <$> randomWord64
  s <- getTcpState
  forM_ (Map.lookup relayPk $ tsRelays s) $ \rc -> case Client.pingIfDue now pingId rc of
    Left () -> relayClosed relayPk
    Right (ping, rc') -> do
      putTcpState s { tsRelays = Map.insert relayPk rc' (tsRelays s) }
      mapM_ (sendToRelay relayPk) ping

-- | The connection to a relay was closed.  'doTcp' reopens it.
relayClosed :: TcpMonad m => PublicKey -> m ()
relayClosed relayPk = do
  open <- isJust . Map.lookup relayPk . tsRelays <$> getTcpState
  when open $ do
    logInfoN $ "TCP relay connection closed: " <> pack (show relayPk)
    modifyTcp $ \s -> s { tsRelays = Map.delete relayPk (tsRelays s) }
    tcpClose relayPk

-- | Periodic work: ping relays, give up on the ones that do not accept us in
-- time, and now and then reconnect to the wanted ones.
doTcp :: TcpMonad m => m ()
doTcp = do
  now <- askTime
  s <- getTcpState
  let due pk = Time.due now connectTimeout (Map.lookup pk $ tsAttempts s)
  mapM_ pingRelay (Map.keys $ tsRelays s)
  mapM_ relayClosed [ pk | (pk, rc) <- Map.toList (tsRelays s), not (Client.isConfirmed rc), due pk ]
  s' <- getTcpState
  forM_ (Map.elems $ wantedRelays s') $ \relay -> do
    let pk = NodeInfo.publicKey relay
    when (Map.notMember pk (tsRelays s') && due pk) $ connectRelay relay
\end{code}

\chapter{TCP connections}

\texttt{TCP\_connections} takes care of handling multiple TCP client instances
to establish a reliable connection via TCP relays to a friend.  Connecting to a
friend with only one relay would not be very reliable, so
\texttt{TCP\_connections} provides the level of abstraction needed to manage
multiple relays.  For example, it ensures that if a relay goes down, the
connection to the peer will not be impacted.  This is done by connecting to the
other peer with more than one relay.

\texttt{TCP\_connections} is above \href{#tcp-client}{\texttt{TCP client}} and
below \texttt{net\_crypto}.

A TCP connection in \texttt{TCP\_connections} is defined as a connection to a
peer though one or more TCP relays.  To connect to another peer with
\texttt{TCP\_connections}, a connection in \texttt{TCP\_connections} to the peer
with DHT public key X will be created.  Some TCP relays which we know the peer
is connected to will then be associated with that peer.  If the peer isn't
connected directly yet, these relays will be the ones that the peer has sent to
us via the onion module.  The peer will also send some relays it is directly
connected to once a connection is established, however, this is done by another
module.

\texttt{TCP\_connections} has a list of all relays it is connected to.  It tries
to keep the number of relays it is connected to as small as possible in order
to minimize load on relays and lower bandwidth usage for the client.  The
desired number of TCP relay connections per peer is set to 3 in toxcore with
the maximum number set to 6.  The reason for these numbers is that 1 would mean
no backup relays and 2 would mean only 1 backup.  To be sure that the
connection is reliable 3 seems to be a reasonable lower bound.  The maximum
number of 6 is the maximum number of relays that can be tied to each peer.  If
2 peers are connected each to the same 6+ relays and they both need to be
connected to that amount of relays because of other friends this is where this
maximum comes into play.  There is no reason why this number is 6 but in
toxcore it has to be at least double than the desired number (3) because the
code assumes this.

If necessary, \texttt{TCP\_connections} will connect to TCP relays to use them
to send onion packets.  This is only done if there is no UDP connection to the
network.  When there is a UDP connection, packets are sent with UDP only
because sending them with TCP relays can be less reliable.  It is also
important that we are connected at all times to some relays as these relays
will be used by TCP only peers to initiate a connection to us.

In toxcore, each client is connected to 3 relays even if there are no TCP peers
and the onion is not needed.  It might be optimal to only connect to these
relays when toxcore is initializing as this is the only time when peers will
connect to us via TCP relays we are connected to.  Due to how the onion works,
after the initialization phase, where each peer is searched in the onion and
then if they are found the info required to connect back (DHT pk, TCP relays)
is sent to them, there should be no more peers connecting to us via TCP relays.
This may be a way to further reduce load on TCP relays, however, more research
is needed before it is implemented.

\texttt{TCP\_connections} picks one relay and uses only it for sending data to
the other peer.  The reason for not picking a random connected relay for each
packet is that it severely deteriorates the quality of the link between two
peers and makes performance of lossy video and audio transmissions really poor.
For this reason, one relay is picked and used to send all data.  If for any
reason no more data can be sent through that relay, the next relay is used.
This may happen if the TCP socket is full and so the relay should not
necessarily be dropped if this occurs.  Relays are only dropped if they time
out or if they become useless (if the relay is one too many or is no longer
being used to relay data to any peers).

\texttt{TCP\_connections} in toxcore also contains a mechanism to make
connections go to sleep.  TCP connections to other peers may be put to sleep if
the connection to the peer establishes itself with UDP after the connection is
established with TCP.  UDP is the method preferred by \texttt{net\_crypto} to
communicate with other peers.  In order to keep track of the relays which were
used to connect with the other peer in case the UDP connection fails, they are
saved by \texttt{TCP\_connections} when the connection is put to sleep.  Any
relays which were only used by this redundant connection are saved then
disconnected from.  If the connection is awakened, the relays are reconnected
to and the connection is reestablished.  Putting a connection to sleep is the
same as saving all the relays used by the connection and removing the
connection.  Awakening the connection is the same as creating a new connection
with the same parameters and restoring all the relays.

A method to detect potentially dysfunctional relays that try to disrupt the
network by lying that they are connecting to a peer when they are not or that
maliciously drop all packets should be considered.  Toxcore doesn't currently
implement such a system and adding one requires more research and likely also
requires extending the protocol.

When TCP connections connects to a relay it will create a new
\href{#tcp-client}{\texttt{TCP\_client}} instance for that relay.  At any time
if the \texttt{TCP\_client} instance reports that it has disconnected, the TCP
relay will be dropped.  Once the TCP relay reports that it is connected,
\texttt{TCP\_connections} will find all the connections that are associated to
the relay and announce to the relay that it wants to connect to each of them
with routing requests.  If the relay reports that the peer for a connection is
online, the connection number and relay will be used to send data in that
connection with data packets.  If the peer isn't reported as online but the
relay is associated to a connection, TCP OOB (out of band) packets will be used
to send data instead of data packets.  TCP OOB packets are used in this case
since the relay most likely has the peer connected but it has not sent a
routing request to connect to us.

\texttt{TCP\_connections} is used as the bridge between individual
\texttt{TCP\_client} instances and \texttt{net\_crypto}, or the bridge between
individual connections and something that requires an interface that looks like
one connection.
