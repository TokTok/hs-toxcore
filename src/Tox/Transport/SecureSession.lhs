\begin{code}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}

module Tox.Transport.SecureSession where

import           Control.Applicative                ((<|>))
import           Control.Monad                      (guard, unless, when)
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN, logWarnN)
import           Control.Monad.State                (MonadState, execStateT,
                                                     get, gets, modify)
import           Data.Binary                        (Binary)
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Get                    as Get
import qualified Data.Binary.Put                    as Put
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Foldable                      (forM_)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (pack)
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)
import           GHC.Generics                       (Generic)
import qualified System.Clock                       as Clock

import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (TimeDiff, Timestamp (..))
import           Tox.Core.Timed                     (Timed (..))
import qualified Tox.Crypto.Core.Box                as Box
import           Tox.Crypto.Core.Box                (CipherText, PlainText (..))
import qualified Tox.Crypto.Core.Hash               as Hash
import           Tox.Crypto.Core.Key                (CombinedKey, Nonce,
                                                     PublicKey, SecretKey)
import           Tox.Crypto.Core.Keyed              (Keyed (..))
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair (..))
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..),
                                                     randomNonce, randomWord64)
import qualified Tox.Crypto.Core.Nonce              as Nonce
import           Tox.Network.Core.Networked         (Networked (..))
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..),
                                                     RawPayload (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Transport.Reliability          as Reliability
import qualified Tox.Transport.Stream               as Stream
\end{code}

\chapter{Net crypto}

The Tox transport protocol is what Tox uses to establish and send data securely
to friends and provides encryption, ordered delivery, and perfect forward
secrecy.  It is a UDP protocol but it is also used when 2 friends connect over
TCP relays.

The reason the protocol for connections to friends over TCP relays and direct
UDP is the same is for simplicity and so the connection can switch between both
without the peers needing to disconnect and reconnect.  For example two Tox
friends might first connect over TCP and a few seconds later switch to UDP when
a direct UDP connection becomes possible.  The opening up of the UDP route or
'hole punching' is done by the DHT module and the opening up of a relayed TCP
connection is done by the \texttt{TCP\_connection} module.  The Tox transport
protocol has the job of connecting two peers (tox friends) safely once a route
or communications link between both is found.  Direct UDP is preferred over TCP
because it is direct and isn't limited by possibly congested TCP relays.  Also,
a peer can only connect to another using the Tox transport protocol if they
know the real public key and DHT public key of the peer they want to connect
to.  However, both the DHT and TCP connection modules require this information
in order to find and open the route to the peer which means we assume this
information is known by toxcore and has been passed to \texttt{net\_crypto} when
the \texttt{net\_crypto} connection was created.

Because this protocol has to work over UDP it must account for possible packet
loss, packets arriving in the wrong order and has to implement some kind of
congestion control.  This is implemented above the level at which the packets
are encrypted.  This prevents a malicious TCP relay from disrupting the
connection by modifying the packets that go through it.  The packet loss
prevention makes it work very well on TCP relays that we assume may go down at
any time as the connection will stay strong even if there is need to switch to
another TCP relay which will cause some packet loss.

Before sending the actual handshake packet the peer must obtain a cookie.  This
cookie step serves as a way for the receiving peer to confirm that the peer
initiating the connection can receive the responses in order to prevent certain
types of DoS attacks.

The peer receiving a cookie request packet must not allocate any resources to
the connection.  They will simply respond to the packet with a cookie response
packet containing the cookie that the requesting peer must then use in the
handshake to initiate the actual connection.

The cookie response must be sent back using the exact same link the cookie
request packet was sent from.  The reason for this is that if it is sent back
using another link, the other link might not work and the peer will not be
expecting responses from another link.  For example, if a request is sent from
UDP with ip port X, it must be sent back by UDP to ip port X.  If it was
received from a TCP OOB packet it must be sent back by a TCP OOB packet via the
same relay with the destination being the peer who sent the request.  If it was
received from an established TCP relay connection it must be sent back via that
same exact connection.

When a cookie request is received, the peer must not use the information in the
request packet for anything, he must not store it, he must only create a cookie
and cookie response from it, then send the created cookie response packet and
forget them.  The reason for this is to prevent possible attacks.  For example
if a peer would allocate long term memory for each cookie request packet
received then a simple packet flood would be enough to achieve an effective
denial of service attack by making the program run out of memory.

cookie request packet (145 bytes):

\begin{verbatim}
[uint8_t 24]
[Sender's DHT Public key (32 bytes)]
[Random nonce (24 bytes)]
[Encrypted message containing:
    [Sender's real public key (32 bytes)]
    [padding (32 bytes)]
    [uint64_t echo id (must be sent back untouched in cookie response)]
]
\end{verbatim}

Encrypted message is encrypted with sender's DHT private key, receiver's DHT
public key and the nonce.

\begin{code}
-- | Cookie Request Packet (0x18 / 24).
data CookieRequest = CookieRequest
  { crSenderDhtPk      :: PublicKey
  , crNonce            :: Nonce
  , crEncryptedMessage :: CipherText -- ^ Decrypts to crInnerMessage
  } deriving (Eq, Show, Generic)

instance Binary CookieRequest where
  put cr = do
    Binary.put $ crSenderDhtPk cr
    Binary.put $ crNonce cr
    Binary.put $ crEncryptedMessage cr
  get = CookieRequest <$> Binary.get <*> Binary.get <*> Binary.get

-- | Inner message of a Cookie Request.
data CookieRequestInner = CookieRequestInner
  { criSenderRealPk :: PublicKey
  , criPadding      :: BS.ByteString -- ^ 32 bytes
  , criEchoId       :: Word64
  } deriving (Eq, Show, Generic)

instance Binary CookieRequestInner where
  put cri = do
    Binary.put $ criSenderRealPk cri
    Put.putByteString $ criPadding cri
    Put.putWord64be $ criEchoId cri
  get = CookieRequestInner <$> Binary.get <*> Get.getByteString 32 <*> Get.getWord64be
\end{code}

The packet id for cookie request packets is 24.  The request contains the DHT
public key of the sender which is the key used (The DHT private key) (along
with the DHT public key of the receiver) to encrypt the encrypted part of the
cookie packet and a nonce also used to encrypt the encrypted part of the
packet.  Padding is used to maintain backwards-compatibility with previous
versions of the protocol.  The echo id in the cookie request must be sent back
untouched in the cookie response.  This echo id is how the peer sending the
request can be sure that the response received was a response to the packet
that he sent.

The reason for sending the DHT public key and real public key in the cookie
request is that both are contained in the cookie sent back in the response.

Toxcore currently sends 1 cookie request packet every second 8 times before it
kills the connection if there are no responses.

cookie response packet (161 bytes):

\begin{verbatim}
[uint8_t 25]
[Random nonce (24 bytes)]
[Encrypted message containing:
    [Cookie]
    [uint64_t echo id (that was sent in the request)]
]
\end{verbatim}

Encrypted message is encrypted with the exact same symmetric key as the cookie
request packet it responds to but with a different nonce.

\begin{code}
-- | Cookie Response Packet (0x19 / 25).
data CookieResponse = CookieResponse
  { rsNonce            :: Nonce
  , rsEncryptedMessage :: CipherText -- ^ Decrypts to rsInnerMessage
  } deriving (Eq, Show, Generic)

instance Binary CookieResponse where
  put rs = do
    Binary.put $ rsNonce rs
    Binary.put $ rsEncryptedMessage rs
  get = CookieResponse <$> Binary.get <*> Binary.get

-- | Inner message of a Cookie Response.
data CookieResponseInner = CookieResponseInner
  { rsiCookie :: Cookie
  , rsiEchoId :: Word64
  } deriving (Eq, Show, Generic)

instance Binary CookieResponseInner where
  put rsi = do
    Binary.put $ rsiCookie rsi
    Put.putWord64be $ rsiEchoId rsi
  get = CookieResponseInner <$> Binary.get <*> Get.getWord64be
\end{code}

The packet id for cookie request packets is 25.  The response contains a nonce
and an encrypted part encrypted with the nonce.  The encrypted part is
encrypted with the same key used to decrypt the encrypted part of the request
meaning the expensive shared key generation needs to be called only once in
order to handle and respond to a cookie request packet with a cookie response.

The Cookie (see below) and the echo id that was sent in the request are the
contents of the encrypted part.

The Cookie should be (112 bytes):

\begin{verbatim}
[nonce]
[encrypted data:
    [uint64_t time]
    [Sender's real public key (32 bytes)]
    [Sender's DHT public key (32 bytes)]
]
\end{verbatim}

The cookie is a 112 byte piece of data that is created and sent to the
requester as part of the cookie response packet.  A peer who wants to connect
to another must obtain a cookie packet from the peer they are trying to connect
to.  The only way to send a valid handshake packet to another peer is to first
obtain a cookie from them.

\begin{code}
-- | Cookie structure (112 bytes).
data Cookie = Cookie
  { cookieNonce            :: Nonce
  , cookieEncryptedPayload :: CipherText -- ^ Decrypts to cookieInner
  } deriving (Eq, Show, Generic)

instance Binary Cookie where
  put c = do
    Binary.put $ cookieNonce c
    Binary.put $ cookieEncryptedPayload c
  get = do
    nonce <- Binary.get
    -- Cookie payload is exactly 88 bytes in Tox (72 bytes payload + 16 bytes MAC)
    payload <- Get.getByteString (cookieLength - 24) >>= Box.cipherText
    return $ Cookie nonce payload

-- | Inner payload of a Cookie.
data CookieInner = CookieInner
  { ciTime         :: Word64
  , ciSenderRealPk :: PublicKey
  , ciSenderDhtPk  :: PublicKey
  } deriving (Eq, Show, Generic)

instance Binary CookieInner where
  put ci = do
    Put.putWord64be $ ciTime ci
    Binary.put $ ciSenderRealPk ci
    Binary.put $ ciSenderDhtPk ci
  get = CookieInner <$> Get.getWord64be <*> Binary.get <*> Binary.get
\end{code}

The cookie contains information that will both prove to the receiver of the
handshake that the peer has received a cookie response and contains encrypted
info that tell the receiver of the handshake packet enough info to both decrypt
and validate the handshake packet and accept the connection.

When toxcore is started it generates a symmetric encryption key that it uses to
encrypt and decrypt all cookie packets (using NaCl authenticated encryption
exactly like encryption everywhere else in toxcore).  Only the instance of
toxcore that create the packets knows the encryption key meaning any cookie it
successfully decrypts and validates were created by it.

The time variable in the cookie is used to prevent cookie packets that are too
old from being used.  Toxcore has a time out of 15 seconds for cookie packets.
If a cookie packet is used more than 15 seconds after it is created toxcore
will see it as invalid.

When responding to a cookie request packet the sender's real public key is the
known key sent by the peer in the encrypted part of the cookie request packet
and the senders DHT public key is the key used to encrypt the encrypted part of
the cookie request packet.

When generating a cookie to put inside the encrypted part of the handshake: One
of the requirements to connect successfully to someone else is that we know
their DHT public key and their real long term public key meaning there is
enough information to construct the cookie.

Handshake packet:

\begin{verbatim}
[uint8_t 26]
[Cookie]
[nonce (24 bytes)]
[Encrypted message containing:
    [24 bytes base nonce]
    [session public key of the peer (32 bytes)]
    [sha512 hash of the entire Cookie sitting outside the encrypted part]
    [Other Cookie (used by the other to respond to the handshake packet)]
]
\end{verbatim}

\begin{code}
-- | Handshake Packet (0x1a / 26).
data Handshake = Handshake
  { hCookie           :: Cookie
  , hNonce            :: Nonce
  , hEncryptedMessage :: CipherText -- ^ Decrypts to hInnerMessage
  } deriving (Eq, Show, Generic)

instance Binary Handshake where
  put h = do
    Binary.put $ hCookie h
    Binary.put $ hNonce h
    Binary.put $ hEncryptedMessage h
  get = do
    cookie <- Binary.get
    nonce <- Binary.get
    payload <- Get.getRemainingLazyByteString >>= Box.cipherText . LBS.toStrict
    return $ Handshake cookie nonce payload

-- | Inner message of a Handshake.
data HandshakeInner = HandshakeInner
  { hiBaseNonce   :: Nonce
  , hiSessionPk   :: PublicKey
  , hiCookieHash  :: BS.ByteString -- ^ 64 bytes (SHA512)
  , hiOtherCookie :: Cookie
  } deriving (Eq, Show, Generic)

instance Binary HandshakeInner where
  put hi = do
    Binary.put $ hiBaseNonce hi
    Binary.put $ hiSessionPk hi
    Put.putByteString $ hiCookieHash hi
    Binary.put $ hiOtherCookie hi
  get = HandshakeInner <$> Binary.get <*> Binary.get <*> Get.getByteString 64 <*> Binary.get
\end{code}

The packet id for handshake packets is 26.  The cookie is a cookie obtained by
sending a cookie request packet to the peer and getting a cookie response
packet with a cookie in it.  It may also be obtained in the handshake packet by
a peer receiving a handshake packet (Other Cookie).

The nonce is a nonce used to encrypt the encrypted part of the handshake
packet.  The encrypted part of the handshake packet is encrypted with the long
term keys of both peers.  This is to prevent impersonation.

Inside the encrypted part of the handshake packet there is a 'base nonce' and a
session public key.  The 'base nonce' is a nonce that the other should use to
encrypt each data packet, adding + 1 to it for each data packet sent.  (first
packet is 'base nonce' + 0, next is 'base nonce' + 1, etc.  Note that for
mathematical operations the nonce is considered to be a 24 byte number in big
endian format).  The session key is the temporary connection public key that
the peer has generated for this connection and it sending to the other.  This
session key is used so that the connection has perfect forward secrecy.  It is
important to save the private key counterpart of the session public key sent in
the handshake, the public key received by the other and both the received and
sent base nonces as they are used to encrypt/decrypt the data packets.

The hash of the cookie in the encrypted part is used to make sure that an
attacker has not taken an older valid handshake packet and then replaced the
cookie packet inside with a newer one which would be bad as they could replay
it and might be able to make a mess.

The 'Other Cookie' is a valid cookie that we put in the handshake so that the
other can respond with a valid handshake without having to make a cookie
request to obtain one.

The handshake packet is sent by both sides of the connection.  If a peer
receives a handshake it will check if the cookie is valid, if the encrypted
section decrypts and validates, if the cookie hash is valid, if long term
public key belongs to a known friend.  If all these are true then the
connection is considered 'Accepted' but not 'Confirmed'.

If there is no existing connection to the peer identified by the long term
public key to set to 'Accepted', one will be created with that status.  If a
connection to such peer with a not yet 'Accepted' status to exists, this
connection is set to accepted.  If a connection with a 'Confirmed' status
exists for this peer, the handshake packet will be ignored and discarded (The
reason for discarding it is that we do not want slightly late handshake packets
to kill the connection) except if the DHT public key in the cookie contained in
the handshake packet is different from the known DHT public key of the peer.
If this happens the connection will be immediately killed because it means it
is no longer valid and a new connection will be created immediately with the
'Accepted' status.

Sometimes toxcore might receive the DHT public key of the peer first with a
handshake packet so it is important that this case is handled and that the
implementation passes the DHT public key to the other modules (DHT,
\texttt{TCP\_connection}) because this does happen.

Handshake packets must be created only once during the connection but must be
sent in intervals until we are sure the other received them.  This happens when
a valid encrypted data packet is received and decrypted.

The states of a connection:

\begin{enumerate}
  \item Not accepted: Send handshake packets.

  \item Accepted: A handshake packet has been received from the other peer but
    no encrypted packets: continue (or start) sending handshake packets because
    the peer can't know if the other has received them.

  \item Confirmed: A valid encrypted packet has been received from the other
    peer: Connection is fully established: stop sending handshake packets.
\end{enumerate}

Toxcore sends handshake packets every second 8 times and times out the
connection if the connection does not get confirmed (no encrypted packet is
received) within this time.

Perfect handshake scenario:

\begin{verbatim}
Peer 1                Peer 2
Cookie request   ->
                      <- Cookie response
Handshake packet ->
                      * accepts connection
                      <- Handshake packet
*accepts connection
Encrypted packet ->   <- Encrypted packet
*confirms connection  *confirms connection
       Connection successful.
Encrypted packets -> <- Encrypted packets

More realistic handshake scenario:
Peer 1                Peer 2
Cookie request   ->   *packet lost*
Cookie request   ->
                      <- Cookie response
                      *Peer 2 randomly starts new connection to peer 1
                      <- Cookie request
Cookie response  ->
Handshake packet ->   <- Handshake packet
*accepts connection   * accepts connection
Encrypted packet ->   <- Encrypted packet
*confirms connection  *confirms connection
       Connection successful.
Encrypted packets -> <- Encrypted packets
\end{verbatim}

The reason why the handshake is like this is because of certain design
requirements:

\begin{enumerate}
  \item The handshake must not leak the long term public keys of the peers to a
     possible attacker who would be looking at the packets but each peer must know
     for sure that they are connecting to the right peer and not an impostor.
  \item A connection must be able of being established if only one of the peers has
     the information necessary to initiate a connection (DHT public key of the
     peer and a link to the peer).
  \item If both peers initiate a connection to each other at the same time the
     connection must succeed without issues.
  \item There must be perfect forward secrecy.
  \item Must be resistant to any possible attacks.
\end{enumerate}

Due to how it is designed only one connection is possible at a time between 2
peers.

Encrypted packets:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x1b) \\
  \texttt{2}         & \texttt{uint16\_t} The last 2 bytes of the nonce used to encrypt this \\
  variable           & Payload \\
\end{tabular}

\begin{code}
-- | Encrypted Packet (0x1b / 27).
data CryptoDataPacket = CryptoDataPacket
  { cdNonceShort :: Word16 -- ^ Last 2 bytes of the nonce
  , cdPayload    :: CipherText
  } deriving (Eq, Show, Generic)

instance Binary CryptoDataPacket where
  put cd = do
    Put.putWord16be $ cdNonceShort cd
    Binary.put $ cdPayload cd
  get = CryptoDataPacket <$> Get.getWord16be <*> Binary.get
\end{code}

The payload is encrypted with the session key and 'base nonce' set by the
receiver in their handshake + packet number (starting at 0, big endian math).

The packet id for encrypted packets is 27.  Encrypted packets are the packets
used to send data to the other peer in the connection.  Since these packets can
be sent over UDP the implementation must assume that they can arrive out of
order or even not arrive at all.

To get the key used to encrypt/decrypt each packet in the connection a peer
takes the session public key received in the handshake and the private key
counterpart of the key it sent it the handshake and generates a shared key from
it.  This shared key will be identical for both peers.  It is important to note
that connection keys must be wiped when the connection is killed.

To create an encrypted packet to be sent to the other peer, the data is
encrypted with the shared key for this connection and the base nonce that the
other peer sent in the handshake packet with the total number of encrypted
packets sent in the connection added to it ('base nonce' + 0 for the first
encrypted data packet sent, 'base nonce' + 1 for the second, etc.  Note that
the nonce is treated as a big endian number for mathematical operations like
additions).  The 2 byte (\texttt{uint16\_t}) number at the beginning of the
encrypted packet is the last 2 bytes of this 24 byte nonce.

To decrypt a received encrypted packet, the nonce the packet was encrypted with
is calculated using the base nonce that the peer sent to the other and the 2
byte number at the beginning of the packet.  First we assume that packets will
most likely arrive out of order and that some will be lost but that packet loss
and out of orderness will never be enough to make the 2 byte number need an
extra byte.  The packet is decrypted using the shared key for the connection
and the calculated nonce.

Toxcore uses the following method to calculate the nonce for each packet:

\begin{enumerate}
  \item \texttt{diff} = (2 byte number on the packet) - (last 2 bytes of the current saved
     base nonce) NOTE: treat the 3 variables as 16 bit unsigned ints, the result
     is expected to sometimes roll over.
  \item copy \texttt{saved\_base\_nonce} to \texttt{temp\_nonce}.
  \item \texttt{temp\_nonce = temp\_nonce + diff}.  \texttt{temp\_nonce} is the correct nonce that
     can be used to decrypt the packet.
  \item \texttt{DATA\_NUM\_THRESHOLD} = (1/3 of the maximum number that can be stored in an
     unsigned 2 bit integer)
  \item if decryption succeeds and \texttt{diff > (DATA\_NUM\_THRESHOLD * 2)} then:
    \begin{itemize}
      \item \texttt{saved\_base\_nonce = saved\_base\_nonce + DATA\_NUM\_THRESHOLD}
    \end{itemize}
\end{enumerate}

\begin{code}
{-------------------------------------------------------------------------------
 -
 - :: Constants.
 -
 ------------------------------------------------------------------------------}

-- | Payload lengths of the handshake packets, without the packet kind.
cookieRequestLength, cookieResponseLength, handshakeLength :: Int
cookieRequestLength  = 32 + 24 + 32 + 32 + 8 + 16
cookieResponseLength = 24 + cookieLength + 8 + 16
handshakeLength      = cookieLength + 24 + 24 + 32 + 64 + cookieLength + 16

cookieLength :: Int
cookieLength = 24 + 8 + 32 + 32 + 16

-- | Seconds a cookie stays valid.
cookieTimeout :: Word64
cookieTimeout = 15

packetIdKill :: Word8
packetIdKill = 2

isLossless, isLossy :: Word8 -> Bool
isLossless ident = ident >= 16 && ident <= 191
isLossy ident = ident >= 192 && ident <= 254

-- | Interval for resending the cookie request or handshake, and for sending
-- request packets.
sendPacketInterval :: TimeDiff
sendPacketInterval = Time.seconds 1

-- | How often the cookie request or handshake is sent before giving up.
maxSendTries :: Int
maxSendTries = 8

-- | Requested packets sent less than a round trip ago are not resent yet.
-- These are the round trips toxcore assumes over UDP until it measures one,
-- and over TCP.
resendDelay, tcpResendDelay :: TimeDiff
resendDelay    = Time.seconds 1
tcpResendDelay = Time.milliseconds 500

-- | Threshold for base nonce rotation (1/3 of 65536).
dataNumThreshold :: Word16
dataNumThreshold = 21845

-- | How often we measure the receive rate, and the rate under which request
-- packets go at 'sendPacketInterval'.
packetCounterInterval :: TimeDiff
packetCounterInterval = Stream.sampleInterval

minPacketRate :: Double
minPacketRate = Stream.minRate

-- | toxcore's REQUEST_PACKETS_COMPARE_CONSTANT.
requestCompareConstant :: Double
requestCompareConstant = 12.5


{-------------------------------------------------------------------------------
 -
 - :: State Definitions.
 -
 ------------------------------------------------------------------------------}

data SessionStatus
  = CookieRequesting Word64 -- ^ Echo id of our cookie request.
  | HandshakeSent
  | NotConfirmed            -- ^ Handshakes exchanged, no data packet yet.
  | Established
  deriving (Eq, Show)

-- | Keys from the peer's handshake.
data SessionKeys = SessionKeys
  { skSharedKey :: CombinedKey
  , skRecvNonce :: Nonce -- ^ Peer's base nonce, advanced as packets arrive.
  } deriving (Eq, Show)

-- | What a session reports to the layer above.
data SessionEvent
  = SessionEstablished
  | SessionKilled
  | SessionLosslessData ByteString
  | SessionLossyData ByteString
  | SessionDhtPkChanged PublicKey
  deriving (Eq, Show)

data SecureSessionState = SecureSessionState
  { ssOurRealKeyPair    :: KeyPair
  , ssPeerRealPk        :: PublicKey
  , ssOurDhtKeyPair     :: KeyPair
  , ssPeerDhtPk         :: PublicKey
  , ssPeerNodeInfo      :: NodeInfo
  , ssStatus            :: SessionStatus
  , ssOurSessionKeyPair :: KeyPair
  , ssKeys              :: Maybe SessionKeys
  , ssSentNonce         :: Nonce -- ^ Nonce for our next data packet.
  , ssTempPacket        :: Maybe (Packet RawPayload) -- ^ Cookie request or handshake.
  , ssTempSent          :: Int
  , ssTempLastSent      :: Maybe Timestamp
  , ssLastRequestSent   :: Maybe Timestamp
  , ssPacketCounter     :: Int             -- ^ Lossless packets since 'ssCounterSince'.
  , ssCounterSince      :: Maybe Timestamp
  , ssRecvRate          :: Double          -- ^ Lossless packets per second.
  , ssReliability       :: Reliability.ReliabilityState
  , ssRtt               :: TimeDiff        -- ^ The shortest round trip seen.
  , ssCongestion        :: Stream.Congestion
  , ssLastTcpSent       :: Maybe Timestamp
  } deriving (Eq, Show)

type SessionMonad m =
  (MonadState SecureSessionState m, Timed m, MonadRandomBytes m, Keyed m, Networked m, MonadLogger m)

-- | A handshake that passed validation.
data OpenedHandshake = OpenedHandshake
  { ohPeerRealPk  :: PublicKey
  , ohPeerDhtPk   :: PublicKey
  , ohBaseNonce   :: Nonce
  , ohSessionPk   :: PublicKey
  , ohOtherCookie :: Cookie
  } deriving (Eq, Show)


{-------------------------------------------------------------------------------
 -
 - :: Cookie Logic.
 -
 ------------------------------------------------------------------------------}

-- | Cookie time is in seconds.  Only we read our cookies.
cookieTime :: Timestamp -> Word64
cookieTime (Timestamp t) = fromIntegral $ Clock.sec t

-- | Create a Cookie for a peer.
createCookie :: MonadRandomBytes m => CombinedKey -> Word64 -> PublicKey -> PublicKey -> m Cookie
createCookie cookieKey time peerRealPk peerDhtPk = do
  nonce <- randomNonce
  let inner = CookieInner time peerRealPk peerDhtPk
      plain = Box.encode inner
      encrypted = Box.encrypt cookieKey nonce plain
  return $ Cookie nonce encrypted

-- | Decrypt and validate a Cookie.
decryptCookie :: CombinedKey -> Cookie -> Maybe CookieInner
decryptCookie cookieKey (Cookie nonce encrypted) =
  Box.decrypt cookieKey nonce encrypted >>= Box.decode

-- | Decrypt a cookie that has not expired.
openCookie :: CombinedKey -> Timestamp -> Cookie -> Maybe CookieInner
openCookie cookieKey now cookie = do
  ci <- decryptCookie cookieKey cookie
  guard $ ciTime ci + cookieTimeout >= cookieTime now && cookieTime now >= ciTime ci
  return ci

cookieHash :: Cookie -> ByteString
cookieHash = Hash.hash . LBS.toStrict . Binary.encode


{-------------------------------------------------------------------------------
 -
 - :: Packet Helpers.
 -
 ------------------------------------------------------------------------------}

rawPacket :: Binary a => PacketKind.PacketKind -> a -> Packet RawPayload
rawPacket kind = Packet kind . RawPayload . LBS.toStrict . Binary.encode

-- | Decode a packet payload of exactly the given length.
decodeExact :: Binary a => Int -> ByteString -> Maybe a
decodeExact len payload = do
  guard $ BS.length payload == len
  Box.decode $ PlainText payload

nonceShort :: Nonce -> Word16
nonceShort n = fromIntegral $ Nonce.nonceToInteger n `mod` 65536

addNonce :: Integral a => Nonce -> a -> Nonce
addNonce n k = Nonce.integerToNonce $ Nonce.nonceToInteger n + toInteger k

-- | The nonce a data packet was encrypted with, and the peer's base nonce to
-- keep if it decrypts.
packetNonce :: Nonce -> Word16 -> (Nonce, Nonce)
packetNonce base short =
  ( addNonce base diff
  , if diff > dataNumThreshold * 2 then addNonce base dataNumThreshold else base
  )
  where
    diff = short - nonceShort base

getDhtSharedKey :: Keyed m => SecureSessionState -> m CombinedKey
getDhtSharedKey ss = getCombinedKey (KeyPair.secretKey $ ssOurDhtKeyPair ss) (ssPeerDhtPk ss)

getRealSharedKey :: Keyed m => SecureSessionState -> m CombinedKey
getRealSharedKey ss = getCombinedKey (KeyPair.secretKey $ ssOurRealKeyPair ss) (ssPeerRealPk ss)


{-------------------------------------------------------------------------------
 -
 - :: Session Creation.
 -
 ------------------------------------------------------------------------------}

newSessionState :: MonadRandomBytes m
                => KeyPair -> PublicKey -> KeyPair -> PublicKey -> NodeInfo -> SessionStatus -> m SecureSessionState
newSessionState ourRealKp peerRealPk ourDhtKp peerDhtPk peerNode status = do
  ourSessionKp <- newKeyPair
  sentNonce <- randomNonce
  return SecureSessionState
    { ssOurRealKeyPair    = ourRealKp
    , ssPeerRealPk        = peerRealPk
    , ssOurDhtKeyPair     = ourDhtKp
    , ssPeerDhtPk         = peerDhtPk
    , ssPeerNodeInfo      = peerNode
    , ssStatus            = status
    , ssOurSessionKeyPair = ourSessionKp
    , ssKeys              = Nothing
    , ssSentNonce         = sentNonce
    , ssTempPacket        = Nothing
    , ssTempSent          = 0
    , ssTempLastSent      = Nothing
    , ssLastRequestSent   = Nothing
    , ssPacketCounter     = 0
    , ssCounterSince      = Nothing
    , ssRecvRate          = 0
    , ssReliability       = Reliability.initState
    , ssRtt               = resendDelay
    , ssCongestion        = Stream.initState
    , ssLastTcpSent       = Nothing
    }

-- | A session we initiate.  'maintainSession' sends its cookie request.
initSession :: (MonadRandomBytes m, Keyed m)
            => KeyPair -> PublicKey -> KeyPair -> PublicKey -> NodeInfo -> m SecureSessionState
initSession ourRealKp peerRealPk ourDhtKp peerDhtPk peerNode = do
  echoId <- randomWord64
  ss <- newSessionState ourRealKp peerRealPk ourDhtKp peerDhtPk peerNode (CookieRequesting echoId)
  nonce <- randomNonce
  dhtSharedKey <- getDhtSharedKey ss
  let cri = CookieRequestInner (KeyPair.publicKey ourRealKp) (BS.replicate 32 0) echoId
      cr = CookieRequest (KeyPair.publicKey ourDhtKp) nonce (Box.encrypt dhtSharedKey nonce (Box.encode cri))
  return ss { ssTempPacket = Just $ rawPacket PacketKind.CookieRequest cr }

-- | A session the peer initiated with a handshake we validated, answered with
-- our handshake.
acceptSession :: (Timed m, MonadRandomBytes m, Keyed m, Networked m, MonadLogger m)
              => CombinedKey -> KeyPair -> KeyPair -> NodeInfo -> OpenedHandshake -> m SecureSessionState
acceptSession cookieKey ourRealKp ourDhtKp from oh = do
  ss <- newSessionState ourRealKp (ohPeerRealPk oh) ourDhtKp (ohPeerDhtPk oh) from NotConfirmed
  sharedKey <- getCombinedKey (KeyPair.secretKey $ ssOurSessionKeyPair ss) (ohSessionPk oh)
  flip execStateT ss { ssKeys = Just $ SessionKeys sharedKey (ohBaseNonce oh) } $
    createHandshake cookieKey (ohOtherCookie oh) >>= setTempPacket

-- | Validate a handshake: it must carry an unexpired cookie of ours, sign the
-- hash of that cookie and be encrypted with the real key named in the cookie.
openHandshake :: (Timed m, Keyed m) => CombinedKey -> SecretKey -> ByteString -> m (Maybe OpenedHandshake)
openHandshake cookieKey ourRealSk payload = do
  now <- askTime
  case decodeExact handshakeLength payload of
    Nothing -> return Nothing
    Just h -> case openCookie cookieKey now (hCookie h) of
      Nothing -> return Nothing
      Just ci -> do
        realSharedKey <- getCombinedKey ourRealSk (ciSenderRealPk ci)
        return $ do
          hi <- Box.decrypt realSharedKey (hNonce h) (hEncryptedMessage h) >>= Box.decode
          guard $ hiCookieHash hi == cookieHash (hCookie h)
          return OpenedHandshake
            { ohPeerRealPk  = ciSenderRealPk ci
            , ohPeerDhtPk   = ciSenderDhtPk ci
            , ohBaseNonce   = hiBaseNonce hi
            , ohSessionPk   = hiSessionPk hi
            , ohOtherCookie = hiOtherCookie hi
            }

createHandshake :: (MonadState SecureSessionState m, Timed m, MonadRandomBytes m, Keyed m)
                => CombinedKey -> Cookie -> m (Packet RawPayload)
createHandshake cookieKey cookie = do
  ss <- get
  now <- askTime
  otherCookie <- createCookie cookieKey (cookieTime now) (ssPeerRealPk ss) (ssPeerDhtPk ss)
  nonce <- randomNonce
  realSharedKey <- getRealSharedKey ss
  let hi = HandshakeInner (ssSentNonce ss) (KeyPair.publicKey $ ssOurSessionKeyPair ss) (cookieHash cookie) otherCookie
  return $ rawPacket PacketKind.CryptoHandshake $
    Handshake cookie nonce (Box.encrypt realSharedKey nonce (Box.encode hi))

setTempPacket :: (MonadState SecureSessionState m, Timed m, Networked m) => Packet RawPayload -> m ()
setTempPacket pkt = do
  modify $ \s -> s { ssTempPacket = Just pkt, ssTempSent = 0, ssTempLastSent = Nothing }
  sendTempPacket

sendTempPacket :: (MonadState SecureSessionState m, Timed m, Networked m) => m ()
sendTempPacket = do
  ss <- get
  forM_ (ssTempPacket ss) $ \pkt -> do
    now <- askTime
    sendPacket (ssPeerNodeInfo ss) pkt
    modify $ \s -> s { ssTempSent = ssTempSent s + 1, ssTempLastSent = Just now }


{-------------------------------------------------------------------------------
 -
 - :: Session Handlers.
 -
 ------------------------------------------------------------------------------}

-- | Answer a cookie request.  This keeps no state.
handleCookieRequest :: (Timed m, MonadRandomBytes m, Keyed m, Networked m, MonadLogger m)
                    => CombinedKey -> KeyPair -> NodeInfo -> ByteString -> m ()
handleCookieRequest cookieKey ourDhtKp from payload =
  case decodeExact cookieRequestLength payload of
    Nothing -> logWarnN "Failed to decode CookieRequest"
    Just (cr :: CookieRequest) -> do
      sharedKey <- getCombinedKey (KeyPair.secretKey ourDhtKp) (crSenderDhtPk cr)
      case Box.decrypt sharedKey (crNonce cr) (crEncryptedMessage cr) >>= Box.decode of
        Nothing -> logWarnN "Failed to decrypt CookieRequest"
        Just (cri :: CookieRequestInner) -> do
          now <- askTime
          cookie <- createCookie cookieKey (cookieTime now) (criSenderRealPk cri) (crSenderDhtPk cr)
          nonce <- randomNonce
          let rsi = CookieResponseInner cookie (criEchoId cri)
          sendPacket from $ rawPacket PacketKind.CookieResponse $
            CookieResponse nonce (Box.encrypt sharedKey nonce (Box.encode rsi))

-- | Handle a cookie response, handshake or data packet from the peer.
handlePacket :: SessionMonad m => CombinedKey -> NodeInfo -> Packet ByteString -> m [SessionEvent]
handlePacket cookieKey from (Packet kind payload) = case kind of
  PacketKind.CookieResponse  -> [] <$ handleCookieResponse cookieKey payload
  PacketKind.CryptoHandshake -> handleHandshake cookieKey payload
  PacketKind.CryptoData      -> handleCryptoData from payload
  _                          -> return []

handleCookieResponse :: SessionMonad m => CombinedKey -> ByteString -> m ()
handleCookieResponse cookieKey payload = do
  ss <- get
  case ssStatus ss of
    CookieRequesting echoId -> do
      dhtSharedKey <- getDhtSharedKey ss
      let decrypted = do
            rs <- decodeExact cookieResponseLength payload
            Box.decrypt dhtSharedKey (rsNonce rs) (rsEncryptedMessage rs) >>= Box.decode
      case decrypted of
        Just rsi | rsiEchoId rsi == echoId -> do
          createHandshake cookieKey (rsiCookie rsi) >>= setTempPacket
          modify $ \s -> s { ssStatus = HandshakeSent }
        _ -> logWarnN "Invalid CookieResponse"
    _ -> return ()

-- | A handshake for a session that is not established yet.
handleHandshake :: SessionMonad m => CombinedKey -> ByteString -> m [SessionEvent]
handleHandshake cookieKey payload = do
  ss <- get
  if ssStatus ss == Established then return [] else do
    opened <- openHandshake cookieKey (KeyPair.secretKey $ ssOurRealKeyPair ss) payload
    case opened of
      Just oh | ohPeerRealPk oh == ssPeerRealPk ss ->
        if ohPeerDhtPk oh /= ssPeerDhtPk ss
        then return [SessionDhtPkChanged $ ohPeerDhtPk oh]
        else do
          case ssStatus ss of
            CookieRequesting _ -> createHandshake cookieKey (ohOtherCookie oh) >>= setTempPacket
            _                  -> return ()
          [] <$ takeHandshake oh
      _ -> [] <$ logWarnN "Invalid Handshake"

-- | Whether the session still waits for the peer's handshake.
awaitsHandshake :: SecureSessionState -> Bool
awaitsHandshake ss = case ssStatus ss of
  CookieRequesting _ -> True
  HandshakeSent      -> True
  _                  -> False

-- | The peer's handshake for a session that 'awaitsHandshake', which came
-- another way than the session's.  As in toxcore, the session keeps its own
-- session key, which the peer may already use, and answers with our handshake
-- the way the peer's came.
adoptHandshake :: SessionMonad m => CombinedKey -> NodeInfo -> OpenedHandshake -> m ()
adoptHandshake cookieKey from oh = do
  modify $ \s -> s { ssPeerNodeInfo = from }
  createHandshake cookieKey (ohOtherCookie oh) >>= setTempPacket
  takeHandshake oh

-- | The peer's session key and nonce, with our own session key.
takeHandshake :: SessionMonad m => OpenedHandshake -> m ()
takeHandshake oh = do
  ss <- get
  sharedKey <- getCombinedKey (KeyPair.secretKey $ ssOurSessionKeyPair ss) (ohSessionPk oh)
  modify $ \s -> s { ssKeys = Just $ SessionKeys sharedKey (ohBaseNonce oh), ssStatus = NotConfirmed }

handleCryptoData :: SessionMonad m => NodeInfo -> ByteString -> m [SessionEvent]
handleCryptoData from payload = do
  ss <- get
  case ssKeys ss of
    Just keys | ssStatus ss `elem` [NotConfirmed, Established]
              , BS.length payload < Reliability.maxCryptoPacketSize ->
      case Box.decode (PlainText payload) of
        Nothing -> return []
        Just (cd :: CryptoDataPacket) -> do
          let (nonce, recvNonce) = packetNonce (skRecvNonce keys) (cdNonceShort cd)
          case Box.decrypt (skSharedKey keys) nonce (cdPayload cd) >>= Reliability.decodeData . unPlainText of
            Nothing -> [] <$ logWarnN "Failed to decrypt CryptoData"
            Just (bufferStart, num, bytes) -> do
              when (protocol from /= protocol (ssPeerNodeInfo ss)) $
                logInfoN $ "SecureSession moved to " <> pack (show from)
              modify $ \s -> s { ssKeys = Just keys { skRecvNonce = recvNonce }, ssPeerNodeInfo = from }
              handleDataContents bufferStart num bytes
    _ -> return []

handleDataContents :: SessionMonad m => Word32 -> Word32 -> ByteString -> m [SessionEvent]
handleDataContents bufferStart num bytes = do
  rel <- gets ssReliability
  case Reliability.clearSentUntil bufferStart rel of
    Nothing -> return []
    Just rel' -> do
      now <- askTime
      -- toxcore only measures round trips with request packets, which on a
      -- link that loses nothing name no packets and measure nothing.
      forM_ (Reliability.latestSentBefore bufferStart rel) $ \sent ->
        modify $ \s -> s { ssRtt = min (ssRtt s) (Time.diffTime now sent) }
      modify $ \s -> s { ssReliability = rel' }
      let ident = BS.head bytes
      if ident == packetIdKill then return [SessionKilled] else do
        confirmed <- confirm
        delivered <- dispatch ident
        return $ confirmed ++ delivered
  where
    confirm = do
      status <- gets ssStatus
      if status /= NotConfirmed then return [] else do
        logInfoN "SecureSession established"
        modify $ \s -> s { ssStatus = Established, ssTempPacket = Nothing }
        return [SessionEstablished]

    dispatch ident
      | ident == Reliability.packetIdRequest = do
          now <- askTime
          s <- get
          let udp = NodeInfo.protocol (ssPeerNodeInfo s) == UDP
          forM_ (Reliability.handleRequest now (if udp then ssRtt s else tcpResendDelay) bytes (ssReliability s)) $ \(rel', latest) -> do
            setReliability $ fromMaybe rel' (Reliability.setReceivedEnd num rel')
            forM_ latest $ \sent -> modify $ \s' -> s' { ssRtt = min (ssRtt s') (Time.diffTime now sent) }
          return []
      | isLossless ident = do
          rel <- gets ssReliability
          case Reliability.addReceived num bytes rel of
            Nothing -> return []
            Just rel' -> do
              modify $ \s -> s { ssPacketCounter = ssPacketCounter s + 1 }
              let (received, rel'') = Reliability.takeReceived rel'
              setReliability rel''
              return $ map SessionLosslessData received
      | isLossy ident = do
          rel <- gets ssReliability
          forM_ (Reliability.setReceivedEnd num rel) setReliability
          return [SessionLossyData bytes]
      | otherwise = return []

    setReliability rel = modify $ \s -> s { ssReliability = rel }


{-------------------------------------------------------------------------------
 -
 - :: Sending Data.
 -
 ------------------------------------------------------------------------------}

-- | Encrypt and send a data packet with the next nonce.
sendDataPacket :: (MonadState SecureSessionState m, Timed m, Networked m) => Word32 -> ByteString -> m ()
sendDataPacket num bytes = gets ssPeerNodeInfo >>= \to -> sendDataPacketTo to num bytes

sendDataPacketTo :: (MonadState SecureSessionState m, Timed m, Networked m) => NodeInfo -> Word32 -> ByteString -> m ()
sendDataPacketTo to num bytes = do
  ss <- get
  forM_ (ssKeys ss) $ \keys -> do
    let bufferStart = Reliability.paStart . Reliability.rsRecv $ ssReliability ss
        plain = PlainText $ Reliability.encodeData bufferStart num bytes
        cd = CryptoDataPacket (nonceShort $ ssSentNonce ss) (Box.encrypt (skSharedKey keys) (ssSentNonce ss) plain)
    sendPacket to $ rawPacket PacketKind.CryptoData cd
    timestamp <- if protocol to == TCP then Just <$> askTime else return Nothing
    modify $ \s -> s { ssSentNonce = Nonce.increment (ssSentNonce s), ssLastTcpSent = timestamp <|> ssLastTcpSent s }

-- | Number the next lossless packet would get.
sendBufferEnd :: SecureSessionState -> Word32
sendBufferEnd = Reliability.paEnd . Reliability.rsSend . ssReliability

-- | Send lossless data, starting with its packet id: its packet number, or
-- 'Nothing' if the session is not established or the send buffer is full.
sendLossless :: (MonadState SecureSessionState m, Timed m, Networked m) => ByteString -> m (Maybe Word32)
sendLossless bytes = gets ssPeerNodeInfo >>= \to -> sendLosslessTo to bytes

-- | Send lossless data on another link than the session's, to try it.  On a
-- busy link it waits for 'sendUnsent'.
sendLosslessTo :: (MonadState SecureSessionState m, Timed m, Networked m) => NodeInfo -> ByteString -> m (Maybe Word32)
sendLosslessTo to bytes = do
  ss <- get
  case Reliability.addSent bytes (ssReliability ss) of
    Just (num, rel) | ssStatus ss == Established -> do
      modify $ \s -> s { ssReliability = rel }
      busy <- linkBusy to
      unless busy $ do
        sendDataPacketTo to num bytes
        now <- askTime
        modify $ \s -> s { ssReliability = Reliability.markSent now num (ssReliability s) }
      return $ Just num
    _ -> return Nothing

-- | Send lossy data, starting with its packet id.  It is dropped when the
-- session is not established or the link is busy.
sendLossy :: (MonadState SecureSessionState m, Timed m, Networked m) => ByteString -> m Bool
sendLossy bytes = do
  ss <- get
  busy <- linkBusy (ssPeerNodeInfo ss)
  case BS.uncons bytes of
    Just (ident, _)
      | isLossy ident && BS.length bytes <= Reliability.maxCryptoDataSize
      && ssStatus ss == Established && not busy -> do
          sendDataPacket (sendBufferEnd ss) bytes
          return True
    _ -> return False

-- | Send lossless data that counts against the congestion control, such as
-- file data.  It goes out even with no budget left, since what may be sent is
-- decided by 'freeSendSlots' before it is asked for.
sendCongested :: (MonadState SecureSessionState m, Timed m, Networked m) => ByteString -> m (Maybe Word32)
sendCongested bytes = do
  sent <- sendLossless bytes
  forM_ sent $ \_ -> modify $ \s -> s { ssCongestion = Stream.sentNew (ssCongestion s) }
  return sent

-- | How many more packets subject to congestion control we may queue.
freeSendSlots :: SecureSessionState -> Int
freeSendSlots ss = min (Stream.ccPacketsLeft $ ssCongestion ss) (fromIntegral Reliability.bufferSize - sendQueueLength ss)

sendQueueLength :: SecureSessionState -> Int
sendQueueLength = fromIntegral . Reliability.arrayLength . Reliability.rsSend . ssReliability

isDelivered :: Word32 -> SecureSessionState -> Bool
isDelivered num ss = num - Reliability.paStart send >= Reliability.arrayLength send
  where
    send = Reliability.rsSend (ssReliability ss)

-- | Tell the peer we are closing the session.
sendKill :: (MonadState SecureSessionState m, Timed m, Networked m) => m ()
sendKill = do
  ss <- get
  sendDataPacket (sendBufferEnd ss) (BS.singleton packetIdKill)

sendRequest :: (MonadState SecureSessionState m, Timed m, Networked m) => m ()
sendRequest = do
  ss <- get
  now <- askTime
  sendDataPacket (sendBufferEnd ss) $ Reliability.requestPacket Reliability.maxCryptoDataSize (ssReliability ss)
  modify $ \s -> s { ssLastRequestSent = Just now }

-- | Send the lossless packets that waited for the link or that the peer
-- asked for again, as many as the congestion control and the link let us.
sendUnsent :: (MonadState SecureSessionState m, Timed m, Networked m) => m ()
sendUnsent = do
  budget <- gets (Stream.resendBudget . ssCongestion)
  unsent <- gets (take budget . Reliability.unsentPackets . ssReliability)
  sent <- go unsent
  now <- askTime
  -- toxcore does not count a turn with no budget at all.
  when (budget > 0) $ modify $ \s -> s { ssCongestion = Stream.resent now sent (ssCongestion s) }
  where
    go [] = return (0 :: Int)
    go ((num, bytes) : rest) = do
      busy <- gets ssPeerNodeInfo >>= linkBusy
      if busy then return 0 else do
        sendDataPacket num bytes
        now <- askTime
        modify $ \s -> s { ssReliability = Reliability.markSent now num (ssReliability s) }
        (+ 1) <$> go rest

-- | Measure how fast packets arrive and, on an established session, how fast
-- we may send.
updateRecvRate :: MonadState SecureSessionState m => Timestamp -> m ()
updateRecvRate now = modify $ \s -> case ssCounterSince s of
  Just since | Time.addTime since packetCounterInterval > now -> s
  Just since -> sampled s
    { ssRecvRate = fromIntegral (ssPacketCounter s) * 1000 / millis (Time.diffTime now since)
    , ssPacketCounter = 0
    , ssCounterSince = Just now
    }
  Nothing -> s { ssPacketCounter = 0, ssCounterSince = Just now }
  where
    millis d = max 1 (fromIntegral (Time.toMilliseconds d))
    sampled s
      | ssStatus s /= Established = s
      | otherwise = s { ssCongestion = Stream.sample (ssRtt s) (sendQueueLength s) (justDirect s) now (ssCongestion s) }
    justDirect s = NodeInfo.protocol (ssPeerNodeInfo s) == UDP
      && not (Time.due now Stream.congestionTimeout (ssLastTcpSent s))

-- | While data comes in fast, toxcore asks for missing packets, which also
-- tells the sender what arrived, as often as every 50 ms.
requestInterval :: SecureSessionState -> TimeDiff
requestInterval ss
  | ssStatus ss /= Established || rate <= minPacketRate = sendPacketInterval
  | otherwise = Time.milliseconds . round . max counterMs . min sendMs $ min byBacklog byRate
  where
    rate = ssRecvRate ss
    backlog = fromIntegral . Reliability.arrayLength . Reliability.rsRecv $ ssReliability ss
    counterMs = fromIntegral $ Time.toMilliseconds packetCounterInterval
    sendMs = fromIntegral $ Time.toMilliseconds sendPacketInterval
    byBacklog = requestCompareConstant / ((backlog + 1) / (rate + 1)) :: Double
    byRate = (minPacketRate / rate) * sendMs + counterMs

-- | Periodic work: resend the cookie request or handshake until the session
-- is established, send request packets and requested packets.
maintainSession :: SessionMonad m => m [SessionEvent]
maintainSession = do
  ss <- get
  now <- askTime
  let due = Time.due now sendPacketInterval
  if ssStatus ss /= Established && due (ssTempLastSent ss) && ssTempSent ss >= maxSendTries
  then do
    logDebugN $ "SecureSession timed out with " <> pack (show $ ssPeerNodeInfo ss)
    return [SessionKilled]
  else do
    when (ssStatus ss /= Established && due (ssTempLastSent ss)) sendTempPacket
    updateRecvRate now
    interval <- gets requestInterval
    when (ssStatus ss `elem` [NotConfirmed, Established]
          && Time.due now interval (ssLastRequestSent ss)) sendRequest
    when (ssStatus ss == Established) $ do
      modify $ \s -> s { ssCongestion = Stream.refill now (ssCongestion s) }
      sendUnsent
    return []
\end{code}

First it takes the difference between the 2 byte number on the packet and the
last.  Because the 3 values are unsigned 16 bit ints and rollover is part of
the math something like diff = (10 - 65536) means diff is equal to 11.

Then it copies the saved base nonce to a temp nonce buffer.

Then it adds diff to the nonce (the nonce is in big endian format).

After if decryption was successful it checks if diff was bigger than 2/3 of the
value that can be contained in a 16 bit unsigned int and increases the saved
base nonce by 1/3 of the maximum value if it succeeded.

This is only one of many ways that the nonce for each encrypted packet can be
calculated.

Encrypted packets that cannot be decrypted are simply dropped.

The reason for exchanging base nonces is because since the key for encrypting
packets is the same for received and sent packets there must be a cryptographic
way to make it impossible for someone to do an attack where they would replay
packets back to the sender and the sender would think that those packets came
from the other peer.
