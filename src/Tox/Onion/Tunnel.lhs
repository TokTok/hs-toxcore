\begin{code}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Tox.Onion.Tunnel where

import           Control.Monad                  (guard)
import           Control.Monad.Validate         (runValidate)
import           Data.Binary                    (Binary, get, put)
import qualified Data.Binary.Get                as Get
import qualified Data.Binary.Put                as Put
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Data.MessagePack               (MessagePack)
import           GHC.Generics                   (Generic)
import           Test.QuickCheck.Arbitrary      (Arbitrary (..))

import qualified Crypto.Saltine.Class           as Sodium
import           Data.Maybe                     (fromJust)
import           Data.Word                      (Word64, Word8)
import qualified Tox.Crypto.Core.Box            as Box
import           Tox.Crypto.Core.Box            (CipherText, PlainText (..),
                                                 cipherText, decrypt, encrypt,
                                                 unCipherText)
import           Tox.Crypto.Core.Key            (CombinedKey, Key (..), Nonce,
                                                 PublicKey, unKey)
import qualified Tox.Crypto.Core.Keyed          as Keyed
import           Tox.Crypto.Core.Keyed          (Keyed)
import           Tox.Crypto.Core.KeyPair        (KeyPair (..))
import qualified Tox.Network.Core.HostAddress   as HostAddress
import           Tox.Network.Core.HostAddress   (HostAddress (..))
import           Tox.Network.Core.Packet        (Packet (..), RawPayload (..))
import qualified Tox.Network.Core.PacketKind    as PacketKind
import           Tox.Network.Core.PortNumber    (PortNumber (..))
import           Tox.Network.Core.SocketAddress (SocketAddress (..))


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


-- | Onion IP_Port format (fixed 19 bytes).
newtype OnionIPPort = OnionIPPort { unOnionIPPort :: SocketAddress }
  deriving (Eq, Show, Read, Generic)

instance MessagePack OnionIPPort

instance Binary OnionIPPort where
  put (OnionIPPort (SocketAddress hostAddr (PortNumber port))) = do
    case hostAddr of
      IPv4 addr -> do
        Put.putWord8 2
        HostAddress.putIPv4 addr
        Put.putByteString $ BS.replicate 12 0
      IPv6 (a, b, c, d) -> do
        Put.putWord8 10
        Put.putWord32be a
        Put.putWord32be b
        Put.putWord32be c
        Put.putWord32be d
    Put.putWord16be port

  get = do
    family <- Get.getWord8
    hostAddr <- case family of
      2 -> do
        addr <- HostAddress.getIPv4
        _ <- Get.getByteString 12
        return $ IPv4 addr
      10 -> do
        a <- Get.getWord32be
        b <- Get.getWord32be
        c <- Get.getWord32be
        d <- Get.getWord32be
        return $ IPv6 (a, b, c, d)
      f -> fail $ "Invalid Onion IP family: " ++ show f
    OnionIPPort . SocketAddress hostAddr . PortNumber <$> Get.getWord16be

instance Arbitrary OnionIPPort where
  arbitrary = OnionIPPort <$> arbitrary


-- | Initial Onion Request (0x80).
data OnionRequest0 = OnionRequest0
  { onion0Nonce            :: Nonce
  , onion0SenderPublicKey  :: PublicKey
  , onion0EncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance MessagePack OnionRequest0

instance Binary OnionRequest0 where
  put req = do
    Put.putByteString . Sodium.encode . unKey . onion0Nonce $ req
    Put.putByteString . Sodium.encode . unKey . onion0SenderPublicKey $ req
    Put.putByteString . unCipherText . onion0EncryptedPayload $ req
  get = do
    nonce <- Key . fromJust . Sodium.decode <$> Get.getByteString 24
    pk <- Key . fromJust . Sodium.decode <$> Get.getByteString 32
    -- The encrypted payload is the rest of the packet.
    payload <- Get.getRemainingLazyByteString >>= cipherText . LBS.toStrict
    return $ OnionRequest0 nonce pk payload

instance Arbitrary OnionRequest0 where
  arbitrary = OnionRequest0 <$> arbitrary <*> arbitrary <*> arbitrary


-- | Inner payload of an Onion Request (once decrypted).
data OnionRequestPayload = OnionRequestPayload
  { onionPayloadDestination      :: OnionIPPort
  , onionPayloadTemporaryKey     :: PublicKey
  , onionPayloadEncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance MessagePack OnionRequestPayload

instance Binary OnionRequestPayload where
  put req = do
    put $ onionPayloadDestination req
    Put.putByteString . Sodium.encode . unKey . onionPayloadTemporaryKey $ req
    Put.putByteString . unCipherText . onionPayloadEncryptedPayload $ req
  get = do
    dest <- get
    tempPk <- Key . fromJust . Sodium.decode <$> Get.getByteString 32
    payload <- Get.getRemainingLazyByteString >>= cipherText . LBS.toStrict
    return $ OnionRequestPayload dest tempPk payload

-- | Inner-most payload of an Onion Request (Node C to D).
data OnionRequestPayloadFinal = OnionRequestPayloadFinal
  { onionPayloadFinalDestination :: OnionIPPort
  , onionPayloadFinalData        :: BS.ByteString
  }
  deriving (Eq, Show, Read, Generic)

instance MessagePack OnionRequestPayloadFinal

instance Binary OnionRequestPayloadFinal where
  put req = do
    put $ onionPayloadFinalDestination req
    Put.putByteString $ onionPayloadFinalData req
  get = do
    dest <- get
    payload <- LBS.toStrict <$> Get.getRemainingLazyByteString
    return $ OnionRequestPayloadFinal dest payload

instance Arbitrary OnionRequestPayload where
  arbitrary = OnionRequestPayload <$> arbitrary <*> arbitrary <*> arbitrary


-- | Wrap a payload for Onion Request layer 0.
wrapOnion0 :: Keyed m
           => KeyPair -> PublicKey -> Nonce -> OnionRequestPayload -> m OnionRequest0
wrapOnion0 (KeyPair sk pk) receiverPk nonce payload = do
  combined <- Keyed.getCombinedKey sk receiverPk
  let encrypted = Box.encrypt combined nonce (Box.encode payload)
  return $ OnionRequest0 nonce pk encrypted

-- | Unwrap an Onion Request layer 0.
unwrapOnion0 :: Keyed m
             => KeyPair -> OnionRequest0 -> m (Maybe OnionRequestPayload)
unwrapOnion0 (KeyPair sk _) (OnionRequest0 nonce senderPk encrypted) = do
  combined <- Keyed.getCombinedKey sk senderPk
  case Box.decrypt combined nonce encrypted of
    Nothing    -> return Nothing
    Just plain -> return $ Box.decode plain



{-------------------------------------------------------------------------------
 -
 - :: Relaying.
 -
 ------------------------------------------------------------------------------}

-- | Size of an IP_Port inside onion packets.
ipPortSize :: Int
ipPortSize = 19

-- | Size of the return data after the given number of relays: each relay adds
-- a nonce and the encrypted address of the previous hop and its return data.
returnSize :: Int -> Int
returnSize 0   = 0
returnSize hop = 24 + ipPortSize + 16 + returnSize (hop - 1)

-- | Where a relay sends a packet: a UDP address, or a client of our TCP relay.
data Destination = ToUdp SocketAddress | ToTcpClient Word64
  deriving (Eq, Show)

-- | A packet a relay sends on.
data Relayed = Relayed Destination (Packet RawPayload)
  deriving (Eq, Show)

-- | Address family of TCP clients in our return data, as in toxcore.
tcpClientFamily :: Word8
tcpClientFamily = 11

putDestination :: Destination -> Put.Put
putDestination (ToUdp addr) = put (OnionIPPort addr)
putDestination (ToTcpClient client) = do
  Put.putWord8 tcpClientFamily
  Put.putWord64be client
  Put.putByteString $ BS.replicate (ipPortSize - 9) 0

getDestination :: Get.Get Destination
getDestination = do
  family <- Get.lookAhead Get.getWord8
  if family == tcpClientFamily
    then Get.skip 1 *> (ToTcpClient <$> Get.getWord64be) <* Get.skip (ipPortSize - 9)
    else ToUdp . unOnionIPPort <$> get

cipherTextMaybe :: BS.ByteString -> Maybe CipherText
cipherTextMaybe = either (const Nothing) Just . runValidate . cipherText

decodePacket :: BS.ByteString -> Maybe (Packet RawPayload)
decodePacket bs = case Get.runGetOrFail get (LBS.fromStrict bs) of
  Right (_, _, pkt) -> Just pkt
  Left _            -> Nothing

decodeIPPort :: BS.ByteString -> Maybe (SocketAddress, BS.ByteString)
decodeIPPort bs = case Get.runGetOrFail get (LBS.fromStrict bs) of
  Right (rest, _, OnionIPPort addr) -> Just (addr, LBS.toStrict rest)
  Left _                            -> Nothing

-- | Seal the previous hop and its return data with our return key.
sealReturn :: CombinedKey -> Nonce -> Destination -> BS.ByteString -> BS.ByteString
sealReturn key nonce from ret =
  Sodium.encode (unKey nonce) <> unCipherText (encrypt key nonce plain)
  where
    plain = PlainText $ LBS.toStrict (Put.runPut $ putDestination from) <> ret

-- | Open return data we sealed as the given relay (1 to 3).
openReturn :: CombinedKey -> Int -> BS.ByteString -> Maybe (Destination, BS.ByteString)
openReturn key hop ret = do
  guard $ BS.length ret == returnSize hop
  let (nonceBytes, sealed) = BS.splitAt 24 ret
  nonce <- Key <$> Sodium.decode nonceBytes
  PlainText plain <- cipherTextMaybe sealed >>= decrypt key nonce
  case Get.runGetOrFail getDestination (LBS.fromStrict plain) of
    Right (rest, _, to) -> Just (to, LBS.toStrict rest)
    Left _              -> Nothing

-- | Relay an onion request as the given relay (1 to 3): decrypt our layer and
-- send its contents to the address in it, with our return data appended.
-- The third relay only forwards announce and data requests.
relayRequest :: Keyed m
             => KeyPair -> CombinedKey -> Nonce -> Int -> SocketAddress -> BS.ByteString -> m (Maybe Relayed)
relayRequest (KeyPair sk _) returnKey returnNonce hop from payload =
  case (Sodium.decode nonceBytes, Sodium.decode pkBytes, cipherTextMaybe sealed) of
    (Just nonce, Just senderPk, Just ct) | BS.length rest >= returnSize (hop - 1) -> do
      shared <- Keyed.getCombinedKey sk (Key senderPk)
      return $ do
        PlainText plain <- decrypt shared (Key nonce) ct
        forwardRequest hop nonceBytes plain $ sealReturn returnKey returnNonce (ToUdp from) retIn
    _ -> return Nothing
  where
    (nonceBytes, afterNonce) = BS.splitAt 24 payload
    (pkBytes, rest) = BS.splitAt 32 afterNonce
    (sealed, retIn) = BS.splitAt (BS.length rest - returnSize (hop - 1)) rest

-- | Relay an onion request from a client of our TCP relay, as the first
-- relay.  The client sends our layer unencrypted.
relayTcpRequest :: CombinedKey -> Nonce -> Word64 -> BS.ByteString -> Maybe Relayed
relayTcpRequest returnKey returnNonce client payload =
  forwardRequest 1 nonceBytes plain $ sealReturn returnKey returnNonce (ToTcpClient client) BS.empty
  where
    (nonceBytes, plain) = BS.splitAt 24 payload

-- | Send a decrypted layer on to the address in it, with our return data.
forwardRequest :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe Relayed
forwardRequest hop nonceBytes plain ret = do
  (to, inner) <- decodeIPPort plain
  (ident, _) <- BS.uncons inner
  pkt <- decodePacket $ case hop of
    1 -> BS.singleton 0x81 <> nonceBytes <> inner <> ret
    2 -> BS.singleton 0x82 <> nonceBytes <> inner <> ret
    _ -> inner <> ret
  guard $ hop < 3 || ident `elem` [0x83, 0x85, 0x87]
  return $ Relayed (ToUdp to) pkt

-- | Relay an onion response as the given relay (3 to 1) back towards the
-- previous hop.  The first relay sends the bare response to the requester.
relayResponse :: CombinedKey -> Int -> BS.ByteString -> Maybe Relayed
relayResponse returnKey hop payload = do
  let (ret, response) = BS.splitAt (returnSize hop) payload
  (ident, _) <- BS.uncons response
  guard $ ident `elem` [0x84, 0x86, 0x88]
  (to, retOut) <- openReturn returnKey hop ret
  pkt <- decodePacket $ case hop of
    3 -> BS.singleton 0x8d <> retOut <> response
    2 -> BS.singleton 0x8e <> retOut <> response
    _ -> response
  return $ Relayed to pkt

-- | The response an announce node sends back along the request's path.
responseToReturn :: BS.ByteString -> BS.ByteString -> Packet RawPayload
responseToReturn ret response = Packet PacketKind.OnionResponse3 (RawPayload $ ret <> response)
\end{code}

\chapter{Onion}

The goal of the onion module in Tox is to prevent peers that are not friends
from finding out the temporary DHT public key from a known long term public key
of the peer and to prevent peers from discovering the long term public key of
peers when only the temporary DHT key is known.

It makes sure only friends of a peer can find it and connect to it and
indirectly makes sure non friends cannot find the ip address of the peer when
knowing the Tox address of the friend.

The only way to prevent peers in the network from associating the temporary DHT
public key with the long term public key is to not broadcast the long term key
and only give others in the network that are not friends the DHT public key.

The onion lets peers send their friends, whose real public key they know as it
is part of the Tox ID, their DHT public key so that the friends can then find
and connect to them without other peers being able to identify the real public
keys of peers.

So how does the onion work?

The onion works by enabling peers to announce their real public key to peers by
going through the onion path.  It is like a DHT but through onion paths.  In
fact it uses the DHT in order for peers to be able to find the peers with ids
closest to their public key by going through onion paths.

In order to announce its real public key anonymously to the Tox network while
using the onion, a peer first picks 3 random nodes that it knows (they can be
from anywhere: the DHT, connected TCP relays or nodes found while finding peers
with the onion).  The nodes should be picked in a way that makes them unlikely
to be operated by the same person perhaps by looking at the ip addresses and
looking if they are in the same subnet or other ways.  More research is needed
to make sure nodes are picked in the safest way possible.

The reason for 3 nodes is that 3 hops is what they use in Tor and other
anonymous onion based networks.

These nodes are referred to as nodes A, B and C.  Note that if a peer cannot
communicate via UDP, its first peer will be one of the TCP relays it is
connected to, which will be used to send its onion packet to the network.

TCP relays can only be node A or the first peer in the chain as the TCP relay
is essentially acting as a gateway to the network.  The data sent to the TCP
Client module to be sent as a TCP onion packet by the module is different from
the one sent directly via UDP.  This is because it doesn't need to be encrypted
(the connection to the TCP relay server is already encrypted).

First I will explain how communicating via onion packets work.

Note: nonce is a 24 byte nonce.  The nested nonces are all the same as the
outer nonce.

Onion packet (request):

Initial (TCP) data sent as the data of an onion packet through the TCP client
module:

\begin{itemize}
  \item \texttt{IP\_Port} of node B
  \item A random public key PK1
  \item Encrypted with the secret key SK1 and the public key of Node B and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} of node C
      \item A random public key PK2
      \item Encrypted with the secret key SK2 and the public key of Node C and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} of node D
          \item Data to send to Node D
        \end{itemize}
    \end{itemize}
\end{itemize}

Initial (UDP) (sent from us to node A):

\begin{itemize}
  \item \texttt{uint8\_t} (0x80) packet id
  \item Nonce
  \item Our temporary DHT public key
  \item Encrypted with our temporary DHT secret key and the public key of Node A and
    the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} of node B
      \item A random public key PK1
      \item Encrypted with the secret key SK1 and the public key of Node B and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} of node C
          \item A random public key PK2
          \item Encrypted with the secret key SK2 and the public key of Node C and the
            nonce:
            \begin{itemize}
              \item \texttt{IP\_Port} of node D
              \item Data to send to Node D
            \end{itemize}
        \end{itemize}
    \end{itemize}
\end{itemize}

(sent from node A to node B):

\begin{itemize}
  \item \texttt{uint8\_t} (0x81) packet id
  \item Nonce
  \item A random public key PK1
  \item Encrypted with the secret key SK1 and the public key of Node B and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} of node C
      \item A random public key PK2
      \item Encrypted with the secret key SK2 and the public key of Node C and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} of node D
          \item Data to send to Node D
        \end{itemize}
    \end{itemize}
  \item Nonce
  \item Encrypted with temporary symmetric key of Node A and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} (of us)
    \end{itemize}
\end{itemize}

(sent from node B to node C):

\begin{itemize}
  \item \texttt{uint8\_t} (0x82) packet id
  \item Nonce
  \item A random public key PK1
  \item Encrypted with the secret key SK1 and the public key of Node C and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} of node D
      \item Data to send to Node D
    \end{itemize}
  \item Nonce
  \item Encrypted with temporary symmetric key of Node B and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} (of Node A)
      \item Nonce
      \item Encrypted with temporary symmetric key of Node A and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} (of us)
        \end{itemize}
    \end{itemize}
\end{itemize}

(sent from node C to node D):

\begin{itemize}
  \item Data to send to Node D
  \item Nonce
  \item Encrypted with temporary symmetric key of Node C and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} (of Node B)
      \item Nonce
      \item Encrypted with temporary symmetric key of Node B and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} (of Node A)
          \item Nonce
          \item Encrypted with temporary symmetric key of Node A and the nonce:
            \begin{itemize}
              \item \texttt{IP\_Port} (of us)
            \end{itemize}
        \end{itemize}
    \end{itemize}
\end{itemize}

Onion packet (response):

initial (sent from node D to node C):

\begin{itemize}
  \item \texttt{uint8\_t} (0x8c) packet id
  \item Nonce
  \item Encrypted with the temporary symmetric key of Node C and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} (of Node B)
      \item Nonce
      \item Encrypted with the temporary symmetric key of Node B and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} (of Node A)
          \item Nonce
          \item Encrypted with the temporary symmetric key of Node A and the nonce:
            \begin{itemize}
              \item \texttt{IP\_Port} (of us)
            \end{itemize}
        \end{itemize}
    \end{itemize}
  \item Data to send back
\end{itemize}

(sent from node C to node B):

\begin{itemize}
  \item \texttt{uint8\_t} (0x8d) packet id
  \item Nonce
  \item Encrypted with the temporary symmetric key of Node B and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} (of Node A)
      \item Nonce
      \item Encrypted with the temporary symmetric key of Node A and the nonce:
        \begin{itemize}
          \item \texttt{IP\_Port} (of us)
        \end{itemize}
    \end{itemize}
  \item Data to send back
\end{itemize}

(sent from node B to node A):

\begin{itemize}
  \item \texttt{uint8\_t} (0x8e) packet id
  \item Nonce
  \item Encrypted with the temporary symmetric key of Node A and the nonce:
    \begin{itemize}
      \item \texttt{IP\_Port} (of us)
    \end{itemize}
  \item Data to send back
\end{itemize}

(sent from node A to us):

\begin{itemize}
  \item Data to send back
\end{itemize}

Each packet is encrypted multiple times so that only node A will be able to
receive and decrypt the first packet and know where to send it to, node B will
only be able to receive that decrypted packet, decrypt it again and know where
to send it and so on.  You will also notice a piece of encrypted data (the
sendback) at the end of the packet that grows larger and larger at every layer
with the IP of the previous node in it.  This is how the node receiving the end
data (Node D) will be able to send data back.

When a peer receives an onion packet, they will decrypt it, encrypt the
coordinates (IP/port) of the source along with the already existing encrypted
data (if it exists) with a symmetric key known only by the peer and only
refreshed every hour (in toxcore) as a security measure to force expire paths.

Here's a diagram how it works:

\begin{verbatim}
peer
  -> [onion1[onion2[onion3[data]]]] -> Node A
  -> [onion2[onion3[data]]][sendbackA] -> Node B
  -> [onion3[data]][sendbackB[sendbackA]] -> Node C
  -> [data][SendbackC[sendbackB[sendbackA]]]-> Node D (end)
\end{verbatim}

\begin{verbatim}
Node D
  -> [SendbackC[sendbackB[sendbackA]]][response] -> Node C
  -> [sendbackB[sendbackA]][response] -> Node B
  -> [sendbackA][response] -> Node A
  -> [response] -> peer
\end{verbatim}

The random public keys in the onion packets are temporary public keys generated
for and used for that onion path only.  This is done in order to make it
difficult for others to link different paths together.  Each encrypted layer
must have a different public key.  This is the reason why there are multiple
keys in the packet definintions above.

The nonce is used to encrypt all the layers of encryption.  This 24 byte nonce
should be randomly generated.  If it isn't randomly generated and has a
relation to nonces used for other paths it could be possible to tie different
onion paths together.

The \texttt{IP\_Port} is an ip and port in packed format:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{TOX\_AF\_INET} (2) for IPv4 or \texttt{TOX\_AF\_INET6} (10) for IPv6 \\
  \texttt{4 / 16}    & IP address (4 bytes if IPv4, 16 if IPv6) \\
  \texttt{12 / 0}    & Zeroes \\
  \texttt{2}         & \texttt{uint16\_t} Port \\
\end{tabular}

If IPv4 the format is padded with 12 bytes of zeroes so that both IPv4 and IPv6
have the same stored size.

The \texttt{IP\_Port} will always end up being of size 19 bytes.  This is to
make it hard to know if an ipv4 or ipv6 ip is in the packet just by looking at
the size.  The 12 bytes of zeros when ipv4 must be set to 0 and not left
uninitialized as some info may be leaked this way if it stays uninitialized.
All numbers here are in big endian format.

The \texttt{IP\_Port} in the sendback data can be in any format as long as the
length is 19 bytes because only the one who writes it can decrypt it and read
it, however, using the previous format is recommended because of code reuse.
The nonce in the sendback data must be a 24 byte nonce.

Each onion layers has a different packed id that identifies it so that an
implementation knows exactly how to handle them.  Note that any data being sent
back must be encrypted, appear random and not leak information in any way as
all the nodes in the path will see it.

If anything is wrong with the received onion packets (decryption fails) the
implementation should drop them.

The implementation should have code for each different type of packet that
handles it, adds (or decrypts) a sendback and sends it to the next peer in the
path.  There are a lot of packets but an implementation should be very
straightforward.

Note that if the first node in the path is a TCP relay, the TCP relay must put
an identifier (instead of an IP/Port) in the sendback so that it knows that any
response should be sent to the appropriate peer connected to the TCP relay.
