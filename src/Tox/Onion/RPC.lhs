\begin{code}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Tox.Onion.RPC where

import           Data.Binary               (Binary, get, put)
import           Data.Word                 (Word64, Word8)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           Tox.Crypto.Core.Box       (CipherText)
import           Tox.Crypto.Core.Key       (Nonce, PublicKey)
import qualified Tox.Network.Core.NodeInfo as NodeInfo
import           Tox.Network.Core.NodeInfo (NodeInfo)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


-- | Announce Request Payload (decrypted).
data AnnounceRequestPayload = AnnounceRequestPayload
  { announceRequestPingId          :: PublicKey -- ^ Ping ID or 0
  , announceRequestSearchKey       :: PublicKey -- ^ Public key we are searching for
  , announceRequestDataSendbackKey :: PublicKey -- ^ Key for data packets back to us
  , announceRequestSendbackData    :: Word64    -- ^ 8 bytes of sendback data
  }
  deriving (Eq, Show, Read, Generic)

instance Binary AnnounceRequestPayload where
  put req = do
    put $ announceRequestPingId req
    put $ announceRequestSearchKey req
    put $ announceRequestDataSendbackKey req
    put $ announceRequestSendbackData req
  get = AnnounceRequestPayload <$> get <*> get <*> get <*> get

instance Arbitrary AnnounceRequestPayload where
  arbitrary = AnnounceRequestPayload <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | Announce Request Packet (0x83).
data AnnounceRequest = AnnounceRequest
  { announceRequestNonce            :: Nonce
  , announceRequestSenderPublicKey  :: PublicKey
  , announceRequestEncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance Binary AnnounceRequest where
  put req = do
    put $ announceRequestNonce req
    put $ announceRequestSenderPublicKey req
    put $ announceRequestEncryptedPayload req
  get = AnnounceRequest <$> get <*> get <*> get

instance Arbitrary AnnounceRequest where
  arbitrary = AnnounceRequest <$> arbitrary <*> arbitrary <*> arbitrary


-- | Announce Response Payload (decrypted).
data AnnounceResponsePayload = AnnounceResponsePayload
  { announceResponseIsStored :: Word8     -- ^ 0, 1, or 2
  , announceResponsePingId   :: PublicKey -- ^ Ping ID or Public Key
  , announceResponseNodes    :: [NodeInfo] -- ^ Up to 4 nodes
  }
  deriving (Eq, Show, Read, Generic)

instance Binary AnnounceResponsePayload where
  put res = do
    put $ announceResponseIsStored res
    put $ announceResponsePingId res
    mapM_ put (take 4 $ announceResponseNodes res)
  get = do
    isStored <- get
    pingId <- get
    AnnounceResponsePayload isStored pingId <$> NodeInfo.getNodes

instance Arbitrary AnnounceResponsePayload where
  arbitrary = AnnounceResponsePayload <$> arbitrary <*> arbitrary <*> arbitrary


-- | Announce Response Packet (0x84).
data AnnounceResponse = AnnounceResponse
  { announceResponseSendbackData     :: Word64 -- ^ 8 bytes of sendback data
  , announceResponseNonce            :: Nonce
  , announceResponseEncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance Binary AnnounceResponse where
  put res = do
    put $ announceResponseSendbackData res
    put $ announceResponseNonce res
    put $ announceResponseEncryptedPayload res
  get = AnnounceResponse <$> get <*> get <*> get

instance Arbitrary AnnounceResponse where
  arbitrary = AnnounceResponse <$> arbitrary <*> arbitrary <*> arbitrary


-- | Data to Route Request Packet (0x85).
data DataRouteRequest = DataRouteRequest
  { dataRouteRequestDestination      :: PublicKey -- ^ Destination real PK
  , dataRouteRequestNonce            :: Nonce
  , dataRouteRequestTemporaryKey     :: PublicKey
  , dataRouteRequestEncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance Binary DataRouteRequest where
  put req = do
    put $ dataRouteRequestDestination req
    put $ dataRouteRequestNonce req
    put $ dataRouteRequestTemporaryKey req
    put $ dataRouteRequestEncryptedPayload req
  get = DataRouteRequest <$> get <*> get <*> get <*> get

instance Arbitrary DataRouteRequest where
  arbitrary = DataRouteRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | Data to Route Response Packet (0x86).
data DataRouteResponse = DataRouteResponse
  { dataRouteResponseNonce            :: Nonce
  , dataRouteResponseTemporaryKey     :: PublicKey
  , dataRouteResponseEncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance Binary DataRouteResponse where
  put res = do
    put $ dataRouteResponseNonce res
    put $ dataRouteResponseTemporaryKey res
    put $ dataRouteResponseEncryptedPayload res
  get = DataRouteResponse <$> get <*> get <*> get

instance Arbitrary DataRouteResponse where
  arbitrary = DataRouteResponse <$> arbitrary <*> arbitrary <*> arbitrary


-- | Inner payload of a Data Route packet (decrypted by destination).
data DataRouteInner = DataRouteInner
  { dataRouteInnerSenderPublicKey  :: PublicKey -- ^ Sender real PK
  , dataRouteInnerEncryptedPayload :: CipherText
  }
  deriving (Eq, Show, Read, Generic)

instance Binary DataRouteInner where
  put inner = do
    put $ dataRouteInnerSenderPublicKey inner
    put $ dataRouteInnerEncryptedPayload inner
  get = DataRouteInner <$> get <*> get

instance Arbitrary DataRouteInner where
  arbitrary = DataRouteInner <$> arbitrary <*> arbitrary


-- | DHT Public Key Packet (0x9c).
-- Sent anonymously via Onion to help friends connect back.
data DHTPublicKeyPacket = DHTPublicKeyPacket
  { dhtPKPacketNoReplay  :: Word64
  , dhtPKPacketOurDHTKey :: PublicKey
  , dhtPKPacketNodes     :: [NodeInfo] -- ^ Up to 4 nodes
  }
  deriving (Eq, Show, Read, Generic)

instance Binary DHTPublicKeyPacket where
  put pkt = do
    put $ dhtPKPacketNoReplay pkt
    put $ dhtPKPacketOurDHTKey pkt
    mapM_ put (take 4 $ dhtPKPacketNodes pkt)
  get = do
    noReplay <- get
    dhtKey <- get
    DHTPublicKeyPacket noReplay dhtKey <$> NodeInfo.getNodes

instance Arbitrary DHTPublicKeyPacket where
  arbitrary = DHTPublicKeyPacket <$> arbitrary <*> arbitrary <*> arbitrary
\end{code}

This explained how to create onion packets and how they are sent back.  Next is
what is actually sent and received on top of these onion packets or paths.

Note: nonce is a 24 byte nonce.

announce request packet:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x83) \\
  \texttt{24}        & Nonce \\
  \texttt{32}        & A public key (real or temporary) \\
  \texttt{?}         & Payload \\
\end{tabular}

The public key is our real long term public key if we want to announce
ourselves, a temporary one if we are searching for friends.

The payload is encrypted with the secret key part of the sent public key, the
public key of Node D and the nonce, and contains:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{32}        & Ping ID \\
  \texttt{32}        & Public key we are searching for \\
  \texttt{32}        & Public key that we want those sending back data packets to use \\
  \texttt{8}         & Data to send back in response \\
\end{tabular}

If the ping id is zero, respond with an announce response packet.

If the ping id matches the one the node sent in the announce response and the
public key matches the one being searched for, add the part used to send data
to our list.  If the list is full make it replace the furthest entry.

data to route request packet:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x85) \\
  \texttt{32}        & Public key of destination node \\
  \texttt{24}        & Nonce \\
  \texttt{32}        & Temporary just generated public key \\
  variable           & Payload \\
\end{tabular}

The payload is encrypted with that temporary secret key and the nonce and the
public key from the announce response packet of the destination node.  If Node
D contains the ret data for the node, it sends the stuff in this packet as a
data to route response packet to the right node.

The data in the previous packet is in format:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{32}        & Real public key of sender \\
  variable           & Payload \\
\end{tabular}

The payload is encrypted with real secret key of the sender, the nonce in the
data packet and the real public key of the receiver:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} id \\
  variable           & Data (optional) \\
\end{tabular}

Data sent to us:

announce response packet:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x84) \\
  \texttt{8}         & Data to send back in response \\
  \texttt{24}        & Nonce \\
  variable           & Payload \\
\end{tabular}

The payload is encrypted with the DHT secret key of Node D, the public key in
the request and the nonce:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} \texttt{is\_stored} \\
  \texttt{32}        & Ping ID or Public Key \\
  variable           & Maximum of 4 nodes in packed node format (see DHT) \\
\end{tabular}

The packet contains a ping ID if \texttt{is\_stored} is 0 or 2, or the public
key that must be used to send data packets if \texttt{is\_stored} is 1.

If the \texttt{is\_stored} is not 0, it means the information to reach the
public key we are searching for is stored on this node.  \texttt{is\_stored} is
2 as a response to a peer trying to announce himself to tell the peer that he
is currently announced successfully.

data to route response packet:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x86) \\
  \texttt{24}        & Nonce \\
  \texttt{32}        & Temporary just generated public key \\
  variable           & Payload \\
\end{tabular}

The payload is encrypted with that temporary secret key, the nonce and the
public key from the announce response packet of the destination node.

There are 2 types of request packets and 2 'response' packets to go with them.
The announce request is used to announce ourselves to a node and announce
response packet is used by the node to respond to this packet.  The data to
route request packet is a packet used to send packets through the node to
another peer that has announced itself and that we have found.  The data to
route response packet is what the node transforms this packet into.

To announce ourselves to the network we must first find, using announce
packets, the peers with the DHT public key closest to our real public key.  We
must then announce ourselves to these peers.  Friends will then be able to send
messages to us using data to route packets by sending them to these peers.  To
find the peers we have announced ourselves to, our friends will find the peers
closest to our real public key and ask them if they know us.  They will then be
able to use the peers that know us to send us some messages that will contain
their DHT public key (which we need to know to connect directly to them), TCP
relays that they are connected to (so we can connect to them with these relays
if we need to) and some DHT peers they are connected to (so we can find them
faster in the DHT).

Announce request packets are the same packets used slightly differently if we
are announcing ourselves or searching for peers that know one of our friends.

If we are announcing ourselves we must put our real long term public key in the
packet and encrypt it with our long term private key.  This is so the peer we
are announcing ourselves to can be sure that we actually own that public key.
If we are looking for peers we use a temporary public key used only for packets
looking for that peer in order to leak as little information as possible.  The
\texttt{ping\_id} is a 32 byte number which is sent to us in the announce
response and we must send back to the peer in another announce request.  This
is done in order to prevent people from easily announcing themselves many times
as they have to prove they can respond to packets from the peer before the peer
will let them announce themselves.  This \texttt{ping\_id} is set to 0 when none
is known.

The public key we are searching for is set to our long term public key when
announcing ourselves and set to the long term public key of the friend we are
searching for if we are looking for peers.

When announcing ourselves, the public key we want others to use to send us data
back is set to a temporary public key and we use the private key part of this
key to decrypt packet routing data sent to us.  This public key is to prevent
peers from saving old data to route packets from previous sessions and be able
to replay them in future Tox sessions.  This key is set to zero when searching
for peers.

The sendback data is an 8 byte number that will be sent back in the announce
packet response.  Its goal is to be used to learn which announce request packet
the response is responding to, and hence its location in the unencrypted part
of the response.  This is needed in toxcore to find and check info about the
packet in order to decrypt it and handle it correctly.  Toxcore uses it as an
index to its special \texttt{ping\_array}.

Why don't we use different packets instead of having one announce packet
request and one response that does everything? It makes it a lot more difficult
for possible attackers to know if we are merely announcing ourselves or if we
are looking for friends as the packets for both look the same and are the same
size.

The unencrypted part of an announce response packet contains the sendback data,
which was sent in the request this packet is responding to and a 24 byte random
nonce used to encrypt the encrypted part.

The \texttt{is\_stored} number is set to either 0, 1 or 2.  0 means that the
public key that was being searched in the request isn't stored or known by this
peer.  1 means that it is and 2 means that we are announced successfully at
that node.  Both 1 and 2 are needed so that when clients are restarted it is
possible to reannounce without waiting for the timeout of the previous
announce.  This would not otherwise be possible as a client would receive
response 1 without a \texttt{ping\_id} which is needed in order to reannounce
successfully.

When the \texttt{is\_stored} number is 0 or 2, the next 32 bytes is a
\texttt{ping\_id}.  When \texttt{is\_stored} is 1 it corresponds to a public key
(the send back data public key set by the friend in their announce request)
that must be used to encrypt and send data to the friend.

Then there is an optional maximum 4 nodes, in DHT packed nodes format (see
DHT), attached to the response which denote the 4 DHT peers with the DHT public
keys closest to the searched public key in the announce request known by the
peer (see DHT).  To find these peers, toxcore uses the same function as is used
to find peers for get node DHT responses.  Peers wanting to announce themselves
or searching for peers that 'know' their friends will recursively query closer
and closer peers until they find the closest possible and then either announce
themselves to them or just ping them every once in a while to know if their
friend can be contacted.  Note that the distance function used for this is the
same as the Tox DHT.

Data to route request packets are packets used to send data directly to another
peer via a node that knows that peer.  The public key is the public key of the
final destination where we want the packet to be sent (the real public key of
our friend).  The nonce is a 24 byte random nonce and the public key is a
random temporary public key used to encrypt the data in the packet and, if
possible, only to send packets to this friend (we want to leak as little info
to the network as possible so we use temp public keys as we don't want a peer
to see the same public keys and be able to link things together).  The data is
encrypted data that we want to send to the peer with the public key.

The route response packets are just the last elements (nonce, public key,
encrypted data) of the data to route request packet copied into a new packet
and sent to the appropriate destination.

To handle onion announce packets, toxcore first receives an announce packet and
decrypts it.

Toxcore generates \texttt{ping\_id}s by taking a 32 byte sha hash of the current
time, some secret bytes generated when the instance is created, the current
time divided by a 300 second timeout, the public key of the requester and the
source ip/port that the packet was received from.  Since the ip/port that the
packet was received from is in the \texttt{ping\_id}, the announce packets being
sent with a ping id must be sent using the same path as the packet that we
received the \texttt{ping\_id} from or announcing will fail.

The reason for this 300 second timeout in toxcore is that it gives a reasonable
time (300 to 600 seconds) for peers to announce themselves.

Toxcore generates 2 different ping ids, the first is generated with the current
time (divided by 300) and the second with the current time + 300 (divided by 300).
The two ping ids are then compared to the ping ids in the received packets.
The reason for doing this is that storing every ping id received might be
expensive and leave us vulnerable to a DoS attack, this method makes sure that
the other cannot generate \texttt{ping\_id}s and must ask for them.  The reason
for the 2 \texttt{ping\_id}s is that we want to make sure that the timeout is at
least 300 seconds and cannot be 0.

If one of the two ping ids is equal to the ping id in the announce request,
the sendback data public key and the sendback data are stored in the
datastructure used to store announced peers.  If the implementation has a
limit to how many announced entries it can store, it should only store the
entries closest (determined by the DHT distance function) to its DHT public
key.  If the entry is already there, the information will simply be updated
with the new one and the timeout will be reset for that entry.

Toxcore has a timeout of 300 seconds for announce entries after which they are
removed which is long enough to make sure the entries don't expire prematurely
but not long enough for peers to stay announced for extended amounts of time
after they go offline.

Toxcore will then copy the 4 DHT nodes closest to the public key being searched
to a new packet (the response).

Toxcore will look if the public key being searched is in the datastructure.  If
it isn't it will copy the second generated \texttt{ping\_id} (the one generated
with the current time plus 300 seconds) to the response, set the
\texttt{is\_stored} number to 0 and send the packet back.

If the public key is in the datastructure, it will check whether the public key
that was used to encrypt the announce packet is equal to the announced public
key, if it isn't then it means that the peer is searching for a peer and that
we know it.  This means the \texttt{is\_stored} is set to 1 and the sending back
data public key in the announce entry is copied to the packet.

If it (key used to encrypt the announce packet) is equal (to the announced
public key which is also the 'public key we are searching for' in the announce
packet) meaning the peer is announcing itself and an entry for it exists, the
sending back data public key is checked to see if it equals the one in the
packet.  If it is not equal it means that it is outdated, probably because the
announcing peer's toxcore instance was restarted and so their
\texttt{is\_stored} is set to 0, if it is equal it means the peer is announced
correctly so the \texttt{is\_stored} is set to 2.  The second generated
\texttt{ping\_id} is then copied to the packet.

Once the packet is contructed a random 24 byte nonce is generated, the packet
is encrypted (the shared key used to decrypt the request can be saved and used
to encrypt the response to save an expensive key derivation operation), the
data to send back is copied to the unencrypted part and the packet is sent back
as an onion response packet.
