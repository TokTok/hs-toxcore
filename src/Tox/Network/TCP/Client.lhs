\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Tox.Network.TCP.Client where

import           Control.Monad               (guard)
import           Control.Monad.Validate      (runValidate)
import qualified Crypto.Saltine.Class        as Sodium
import           Data.Binary.Get             (getWord64be, runGetOrFail)
import           Data.Binary.Put             (putWord16be, putWord64be, runPut)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isJust)
import           Data.Word                   (Word64, Word8)

import qualified Tox.Core.Time               as Time
import           Tox.Core.Time               (TimeDiff, Timestamp)
import qualified Tox.Crypto.Core.Box         as Box
import           Tox.Crypto.Core.Box         (PlainText (..))
import qualified Tox.Crypto.Core.CombinedKey as CombinedKey
import           Tox.Crypto.Core.Key         (CombinedKey, Key (..), Nonce,
                                              PublicKey, unKey)
import qualified Tox.Crypto.Core.KeyPair     as KeyPair
import           Tox.Crypto.Core.KeyPair     (KeyPair)
import qualified Tox.Crypto.Core.Nonce       as Nonce
import qualified Tox.Network.Core.NodeInfo   as NodeInfo
import           Tox.Network.Core.NodeInfo   (NodeInfo)


{-------------------------------------------------------------------------------
 -
 - :: Packets.
 -
 ------------------------------------------------------------------------------}

packetRoutingRequest, packetRoutingResponse, packetConnectNotification,
  packetDisconnectNotification, packetPing, packetPong, packetOobSend,
  packetOobRecv, packetOnionRequest, packetOnionResponse :: Word8
packetRoutingRequest         = 0
packetRoutingResponse        = 1
packetConnectNotification    = 2
packetDisconnectNotification = 3
packetPing                   = 4
packetPong                   = 5
packetOobSend                = 6
packetOobRecv                = 7
packetOnionRequest           = 8
packetOnionResponse          = 9

-- | Packet ids below this are control packets.  The others carry data for the
-- routed connection with that id.
numReservedPorts :: Word8
numReservedPorts = 16

maxPacketSize, maxOobDataLength, serverHandshakeLength :: Int
maxPacketSize         = 2048
maxOobDataLength      = 1024
serverHandshakeLength = 24 + 32 + 24 + 16

pingInterval, pingTimeout :: TimeDiff
pingInterval = Time.seconds 30
pingTimeout  = Time.seconds 10


{-------------------------------------------------------------------------------
 -
 - :: Connection state.
 -
 ------------------------------------------------------------------------------}

-- | Whether a peer we asked the relay for is connected to it too.
data RouteStatus = RouteRegistered | RouteOnline
  deriving (Eq, Show)

data Keys = Keys
  { kSharedKey :: CombinedKey
  , kSentNonce :: Nonce
  , kRecvNonce :: Nonce
  } deriving (Eq, Show)

data RelayConnection = RelayConnection
  { rcRelay        :: NodeInfo
  , rcHandshakeKey :: CombinedKey -- ^ Our DHT key with the relay's.
  , rcTempKeys     :: KeyPair   -- ^ Ours for this connection.
  , rcSentNonce    :: Nonce     -- ^ Our first nonce, until 'rcKeys' is set.
  , rcKeys         :: Maybe Keys -- ^ Set by the relay's handshake.
  , rcBuffer       :: ByteString -- ^ Received bytes not yet a whole packet.
  , rcRoutes       :: Map Word8 (PublicKey, RouteStatus)
  , rcLastPinged   :: Maybe Timestamp
  , rcPendingPing  :: Maybe Word64 -- ^ Ping we expect a pong for.
  } deriving (Eq, Show)

-- | What the relay told us.
data RelayEvent
  = RelayRouteChanged PublicKey (Maybe RouteStatus)
  | RelayData PublicKey ByteString
  | RelayOob PublicKey ByteString -- ^ Sender's key and data.
  | RelayOnionResponse ByteString
  deriving (Eq, Show)

isConfirmed :: RelayConnection -> Bool
isConfirmed = isJust . rcKeys

-- | Start a connection to a relay: the state and the handshake to send.
newRelayConnection :: KeyPair -> KeyPair -> Nonce -> Nonce -> NodeInfo -> (RelayConnection, ByteString)
newRelayConnection ourDhtKeys tempKeys sentNonce handshakeNonce relay =
  ( RelayConnection relay shared tempKeys sentNonce Nothing BS.empty Map.empty Nothing Nothing
  , encodeKey (KeyPair.publicKey ourDhtKeys) <> Sodium.encode (unKey handshakeNonce)
      <> Box.unCipherText (Box.encrypt shared handshakeNonce plain)
  )
  where
    shared = CombinedKey.precompute (KeyPair.secretKey ourDhtKeys) (NodeInfo.publicKey relay)
    plain = PlainText $ encodeKey (KeyPair.publicKey tempKeys) <> Sodium.encode (unKey sentNonce)

encodeKey :: PublicKey -> ByteString
encodeKey = Sodium.encode . unKey


{-------------------------------------------------------------------------------
 -
 - :: Sending.
 -
 ------------------------------------------------------------------------------}

-- | Encrypt a packet as a length-prefixed frame.
sealFrame :: ByteString -> Keys -> Maybe (ByteString, Keys)
sealFrame bytes keys = do
  guard $ BS.length bytes + 16 <= maxPacketSize
  let sealed = Box.unCipherText $ Box.encrypt (kSharedKey keys) (kSentNonce keys) (PlainText bytes)
      frame = LBS.toStrict (runPut $ putWord16be $ fromIntegral $ BS.length sealed) <> sealed
  return (frame, keys { kSentNonce = Nonce.increment (kSentNonce keys) })

-- | Encrypt a packet for the relay.
encodePacket :: ByteString -> RelayConnection -> Maybe (ByteString, RelayConnection)
encodePacket bytes rc = do
  (frame, keys) <- rcKeys rc >>= sealFrame bytes
  return (frame, rc { rcKeys = Just keys })

routingRequest :: PublicKey -> ByteString
routingRequest pk = BS.cons packetRoutingRequest (encodeKey pk)

-- | 'Nothing' for more data than relays take out of band, since they drop
-- a client that sends more.
oobSend :: PublicKey -> ByteString -> Maybe ByteString
oobSend pk bytes = do
  guard $ not (BS.null bytes) && BS.length bytes <= maxOobDataLength
  return $ BS.cons packetOobSend (encodeKey pk) <> bytes

onionRequest :: ByteString -> ByteString
onionRequest = BS.cons packetOnionRequest

pingPacket :: Word8 -> Word64 -> ByteString
pingPacket ident pingId = BS.cons ident . LBS.toStrict . runPut $ putWord64be pingId

-- | Data for a peer, if the relay has connected us to them.
routeData :: PublicKey -> ByteString -> RelayConnection -> Maybe ByteString
routeData pk bytes rc =
  case [ ident | (ident, (pk', RouteOnline)) <- Map.toList (rcRoutes rc), pk' == pk ] of
    ident : _ -> Just $ BS.cons ident bytes
    []        -> Nothing

-- | Whether we asked the relay for the peer.
hasRoute :: PublicKey -> RelayConnection -> Bool
hasRoute pk = any ((== pk) . fst) . Map.elems . rcRoutes

-- | A ping to send if one is due, or 'Left' if the relay stopped answering.
pingIfDue :: Timestamp -> Word64 -> RelayConnection -> Either () (Maybe ByteString, RelayConnection)
pingIfDue now pingId rc
  | not (isConfirmed rc) = Right (Nothing, rc)
  | Just _ <- rcPendingPing rc, maybe False (\t -> Time.addTime t pingTimeout < now) (rcLastPinged rc) = Left ()
  | Time.due now pingInterval (rcLastPinged rc) =
      Right (Just $ pingPacket packetPing pingId, rc { rcLastPinged = Just now, rcPendingPing = Just pingId })
  | otherwise = Right (Nothing, rc)


{-------------------------------------------------------------------------------
 -
 - :: Receiving.
 -
 ------------------------------------------------------------------------------}

-- | Take received bytes: the events, packets to send back, or 'Left' if the
-- relay broke the protocol and the connection must be closed.
receiveBytes :: ByteString -> RelayConnection -> Either () (RelayConnection, [RelayEvent], [ByteString])
receiveBytes bytes rc0 = go rc0 { rcBuffer = rcBuffer rc0 <> bytes } [] []
  where
    go rc events replies = case rcKeys rc of
      Nothing
        | BS.length (rcBuffer rc) < serverHandshakeLength -> Right (rc, events, replies)
        | otherwise -> do
            let (handshake, rest) = BS.splitAt serverHandshakeLength (rcBuffer rc)
            keys <- maybe (Left ()) Right $ openHandshake rc handshake
            go rc { rcKeys = Just keys, rcBuffer = rest } events replies
      Just keys -> openFrame (rcBuffer rc) keys >>= \case
        Nothing -> Right (rc, events, replies)
        Just (plain, keys', rest) -> do
          (rc', newEvents, newReplies) <- handlePacket plain rc { rcBuffer = rest, rcKeys = Just keys' }
          go rc' (events ++ newEvents) (replies ++ newReplies)

-- | Decrypt the first frame of a buffer, if it is all there: the packet, the
-- keys for the next one and the rest of the buffer.
openFrame :: ByteString -> Keys -> Either () (Maybe (ByteString, Keys, ByteString))
openFrame buf keys
  | BS.length buf < 2 = Right Nothing
  | len > maxPacketSize = Left ()
  | BS.length buf < 2 + len = Right Nothing
  | otherwise = do
      let (sealed, rest) = BS.splitAt len (BS.drop 2 buf)
      PlainText plain <- maybe (Left ()) Right $
        cipher sealed >>= Box.decrypt (kSharedKey keys) (kRecvNonce keys)
      return $ Just (plain, keys { kRecvNonce = Nonce.increment (kRecvNonce keys) }, rest)
  where
    len = fromIntegral (BS.index buf 0) * 256 + fromIntegral (BS.index buf 1)

cipher :: ByteString -> Maybe Box.CipherText
cipher = either (const Nothing) Just . runValidate . Box.cipherText

-- | The relay's handshake: its temporary key and first nonce.
openHandshake :: RelayConnection -> ByteString -> Maybe Keys
openHandshake rc handshake = do
  let (nonceBytes, sealed) = BS.splitAt 24 handshake
  nonce <- Key <$> Sodium.decode nonceBytes
  PlainText plain <- cipher sealed >>= Box.decrypt (rcHandshakeKey rc) nonce
  guard $ BS.length plain == 32 + 24
  relayTempPk <- Key <$> Sodium.decode (BS.take 32 plain)
  recvNonce <- Key <$> Sodium.decode (BS.drop 32 plain)
  return Keys
    { kSharedKey = CombinedKey.precompute (KeyPair.secretKey $ rcTempKeys rc) relayTempPk
    , kSentNonce = rcSentNonce rc
    , kRecvNonce = recvNonce
    }

-- | Handle a decrypted packet from the relay.
handlePacket :: ByteString -> RelayConnection -> Either () (RelayConnection, [RelayEvent], [ByteString])
handlePacket plain rc = case BS.uncons plain of
  Just (ident, rest)
    | ident == packetRoutingResponse, BS.length rest == 33 ->
        let (routeId, pkBytes) = (BS.head rest, BS.tail rest)
        in case Key <$> Sodium.decode pkBytes of
          Just pk | routeId >= numReservedPorts, Map.notMember routeId (rcRoutes rc) ->
            Right (setRoute routeId pk RouteRegistered, [RelayRouteChanged pk (Just RouteRegistered)], [])
          _ -> Right (rc, [], [])
    | ident == packetConnectNotification -> notification rest RouteRegistered RouteOnline
    | ident == packetDisconnectNotification -> notification rest RouteOnline RouteRegistered
    | ident == packetPing, BS.length rest == 8 ->
        Right (rc, [], [BS.cons packetPong rest])
    | ident == packetPong, Just pingId <- word64 rest, pingId /= 0 ->
        Right (if rcPendingPing rc == Just pingId then rc { rcPendingPing = Nothing } else rc, [], [])
    | ident == packetOobRecv, BS.length rest > 32, Just pk <- Key <$> Sodium.decode (BS.take 32 rest) ->
        Right (rc, [RelayOob pk (BS.drop 32 rest)], [])
    | ident == packetOnionResponse -> Right (rc, [RelayOnionResponse rest], [])
    | ident >= numReservedPorts ->
        Right (rc, [ RelayData pk rest | Just (pk, _) <- [Map.lookup ident (rcRoutes rc)] ], [])
  _ -> Left ()
  where
    setRoute routeId pk status = rc { rcRoutes = Map.insert routeId (pk, status) (rcRoutes rc) }
    notification rest from to = case BS.unpack rest of
      [routeId] | routeId >= numReservedPorts -> Right $ case Map.lookup routeId (rcRoutes rc) of
        Just (pk, status) | status == from -> (setRoute routeId pk to, [RelayRouteChanged pk (Just to)], [])
        _ -> (rc, [], [])
      _ -> Left ()
    word64 bytes = case runGetOrFail getWord64be (LBS.fromStrict bytes) of
      Right (rest, _, w) | LBS.null rest -> Just w
      _                                  -> Nothing
\end{code}

\chapter{TCP client}

\texttt{TCP client} is the client for the TCP server.  It establishes and keeps
a connection to the TCP server open.

All the packet formats are explained in detail in \texttt{TCP server} so this
section will only cover \texttt{TCP client} specific details which are not
covered in the \texttt{TCP server} documentation.

TCP clients can choose to connect to TCP servers through a proxy.  Most common
types of proxies (SOCKS, HTTP) work by establishing a connection through a
proxy using the protocol of that specific type of proxy.  After the connection
through that proxy to a TCP server is established, the socket behaves from the
point of view of the application exactly like a TCP socket that connects
directly to a TCP server instance.  This means supporting proxies is easy.

\texttt{TCP client} first establishes a TCP connection, either through a proxy
or directly to a TCP server.  It uses the DHT public key as its long term key
when connecting to the TCP server.

It establishes a secure connection to the TCP server.  After establishing a
connection to the TCP server, and when the handshake response has been received
from the TCP server, the toxcore implementation immediately sends a ping
packet.  Ideally the first packets sent would be routing request packets but
this solution aids code simplicity and allows the server to confirm the
connection.

Ping packets, like all other data packets, are sent as encrypted packets.

Ping packets are sent by the toxcore TCP client every 30 seconds with a timeout
of 10 seconds, the same interval and timeout as toxcore TCP server ping
packets.  They are the same because they accomplish the same thing.

\texttt{TCP client} must have a mechanism to make sure important packets
(routing requests, disconnection notifications, ping packets, ping response
packets) don't get dropped because the TCP socket is full.  Should this happen,
the TCP client must save these packets and prioritize sending them, in order,
when the TCP socket on the server becomes available for writing again.
\texttt{TCP client} must also take into account that packets might be bigger
than the number of bytes it can currently write to the socket.  In this case,
it must save the bytes of the packet that it didn't write to the socket and
write them to the socket as soon as the socket allows so that the connection
does not get broken.  It must also assume that it may receive only part of an
encrypted packet.  If this occurs it must save the part of the packet it has
received and wait for the rest of the packet to arrive before handling it.

\texttt{TCP client} can be used to open up a route to friends who are connected
to the TCP server.  This is done by sending a routing request to the TCP server
with the DHT public key of the friend.  This tells the server to register a
\texttt{connection\_id} to the DHT public key sent in the packet.  The server
will then respond with a routing response packet.  If the connection was
accepted, the \texttt{TCP client} will store the \texttt{connection id} for
this connection.  The \texttt{TCP client} will make sure that routing response
packets are responses to a routing packet that it sent by storing that it sent
a routing packet to that public key and checking the response against it.  This
prevents the possibility of a bad TCP server exploiting the client.

The \texttt{TCP client} will handle connection notifications and disconnection
notifications by alerting the module using it that the connection to the peer
is up or down.

\texttt{TCP client} will send a disconnection notification to kill a connection
to a friend.  It must send a disconnection notification packet regardless of
whether the peer was online or offline so that the TCP server will unregister
the connection.

Data to friends can be sent through the TCP relay using OOB (out of band)
packets and connected connections.  To send an OOB packet, the DHT public key
of the friend must be known.  OOB packets are sent in blind and there is no way
to query the TCP relay to see if the friend is connected before sending one.
OOB packets should be sent when the connection to the friend via the TCP relay
isn't in an connected state but it is known that the friend is connected to
that relay.  If the friend is connected via the TCP relay, then normal data
packets must be sent as they are smaller than OOB packets.

OOB recv and data packets must be handled and passed to the module using it.
