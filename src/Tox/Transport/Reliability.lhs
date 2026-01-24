\begin{code}
{-# LANGUAGE StrictData #-}
module Tox.Transport.Reliability where

import           Data.Binary.Get      (getRemainingLazyByteString, getWord32be,
                                       runGetOrFail)
import           Data.Binary.Put      (putByteString, putWord32be, runPut)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List            (sortOn)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (isNothing)
import           Data.Word            (Word32, Word8)

import qualified Tox.Core.Time        as Time
import           Tox.Core.Time        (TimeDiff, Timestamp)


-- | Maximum number of packets a 'PacketsArray' holds.
bufferSize :: Word32
bufferSize = 32768

maxCryptoPacketSize :: Int
maxCryptoPacketSize = 1400

-- | Maximum size of the data (packet id included) in a data packet.
maxCryptoDataSize :: Int
maxCryptoDataSize = maxCryptoPacketSize - (1 + 2 + 4 + 4 + 16)

-- | Data is padded with zeros so its length is a multiple of this, counted
-- back from 'maxCryptoDataSize'.
cryptoMaxPadding :: Int
cryptoMaxPadding = 8

packetIdRequest :: Word8
packetIdRequest = 1


-- | Packets indexed by packet number, covering the numbers from 'paStart' up
-- to but not including 'paEnd'.  All arithmetic on packet numbers wraps
-- around.
data PacketsArray a = PacketsArray
  { paStart   :: Word32
  , paEnd     :: Word32
  , paPackets :: Map Word32 a
  } deriving (Eq, Show)

emptyArray :: PacketsArray a
emptyArray = PacketsArray 0 0 Map.empty

arrayLength :: PacketsArray a -> Word32
arrayLength a = paEnd a - paStart a

-- | Packet numbers covered by the array, in order.
arrayRange :: PacketsArray a -> [Word32]
arrayRange a = take (fromIntegral $ arrayLength a) $ iterate (+ 1) (paStart a)

data SentPacket = SentPacket
  { spData     :: ByteString
  , spSentTime :: Maybe Timestamp -- ^ 'Nothing' if it still needs sending.
  , spResent   :: Bool
  } deriving (Eq, Show)

data ReliabilityState = ReliabilityState
  { rsSend :: PacketsArray SentPacket
  , rsRecv :: PacketsArray ByteString
  } deriving (Eq, Show)

initState :: ReliabilityState
initState = ReliabilityState emptyArray emptyArray


{-------------------------------------------------------------------------------
 -
 - :: Sending.
 -
 ------------------------------------------------------------------------------}

-- | Queue a lossless packet, returning its packet number.
addSent :: ByteString -> ReliabilityState -> Maybe (Word32, ReliabilityState)
addSent bytes rs
  | arrayLength send >= bufferSize = Nothing
  | otherwise = Just (num, rs { rsSend = send
      { paEnd     = num + 1
      , paPackets = Map.insert num (SentPacket bytes Nothing False) (paPackets send)
      } })
  where
    send = rsSend rs
    num = paEnd send

markSent :: Timestamp -> Word32 -> ReliabilityState -> ReliabilityState
markSent now num rs = rs { rsSend = send { paPackets = Map.adjust sent num (paPackets send) } }
  where
    send = rsSend rs
    sent p = p { spSentTime = Just now }

-- | Packets not sent yet or requested again by the peer, in order.
unsentPackets :: ReliabilityState -> [(Word32, ByteString)]
unsentPackets rs =
  sortOn (subtract (paStart send) . fst)
    [ (num, spData p) | (num, p) <- Map.toList (paPackets send), isNothing (spSentTime p) ]
  where
    send = rsSend rs

-- | When the packet before the given number was sent, unless it was sent
-- more than once: how long ago is a round trip, when the peer says it has
-- everything before that number.
latestSentBefore :: Word32 -> ReliabilityState -> Maybe Timestamp
latestSentBefore num rs = case Map.lookup (num - 1) (paPackets $ rsSend rs) of
  Just p | not (spResent p) -> spSentTime p
  _                         -> Nothing

-- | The peer has received every packet before the given number.
clearSentUntil :: Word32 -> ReliabilityState -> Maybe ReliabilityState
clearSentUntil num rs
  | num == paStart send = Just rs
  | paEnd send - num >= arrayLength send || num - paStart send > arrayLength send = Nothing
  | otherwise = Just rs { rsSend = send
      { paStart   = num
      , paPackets = Map.filterWithKey (\k _ -> k - num < paEnd send - num) (paPackets send)
      } }
  where
    send = rsSend rs

-- | Handle a request packet: requested packets are resent unless they were
-- sent less than @rtt@ ago, the ones before them were received by the peer.
-- Also gives the latest time one of the received ones was sent, to measure
-- the round trip time with.
handleRequest :: Timestamp -> TimeDiff -> ByteString -> ReliabilityState -> Maybe (ReliabilityState, Maybe Timestamp)
handleRequest now rtt bytes rs = case BS.unpack bytes of
  ident : nums | ident == packetIdRequest ->
    (\(ps, latest) -> (rs { rsSend = send { paPackets = ps } }, latest)) <$> go 1 nums (arrayRange send) (paPackets send, Nothing)
  _ -> Nothing
  where
    send = rsSend rs

    go :: Word32 -> [Word8] -> [Word32] -> (Map Word32 SentPacket, Maybe Timestamp) -> Maybe (Map Word32 SentPacket, Maybe Timestamp)
    go n nums@(d:ds) (i:is) (ps, latest)
      | n == fromIntegral d = next 0 ds (Map.adjust resend i ps, latest)
      | otherwise           = next n nums (Map.delete i ps, max latest (Map.lookup i ps >>= firstSent))
      where
        next 255 (0:ds') acc = go 1 ds' is acc
        next 255 _       _   = Nothing
        next n'  ds'     acc = go (n' + 1) ds' is acc
    go _ _ _ acc = Just acc

    -- A packet sent again may have arrived the first time, so it says
    -- nothing about the round trip.
    firstSent p = if spResent p then Nothing else spSentTime p

    resend p = case spSentTime p of
      Just sent | Time.addTime sent rtt < now -> p { spSentTime = Nothing, spResent = True }
      _                                       -> p


{-------------------------------------------------------------------------------
 -
 - :: Receiving.
 -
 ------------------------------------------------------------------------------}

-- | Store a received lossless packet.  Fails for duplicates and packets
-- outside the buffer.
addReceived :: Word32 -> ByteString -> ReliabilityState -> Maybe ReliabilityState
addReceived num bytes rs
  | num - paStart recv >= bufferSize = Nothing
  | Map.member num (paPackets recv) = Nothing
  | otherwise = Just rs { rsRecv = recv
      { paEnd     = if num - paStart recv >= arrayLength recv then num + 1 else paEnd recv
      , paPackets = Map.insert num bytes (paPackets recv)
      } }
  where
    recv = rsRecv rs

-- | Take the received packets that are next in order.
takeReceived :: ReliabilityState -> ([ByteString], ReliabilityState)
takeReceived rs = go [] (rsRecv rs)
  where
    go acc recv = case Map.lookup (paStart recv) (paPackets recv) of
      Just bytes | paStart recv /= paEnd recv ->
        go (bytes : acc) recv
          { paStart   = paStart recv + 1
          , paPackets = Map.delete (paStart recv) (paPackets recv)
          }
      _ -> (reverse acc, rs { rsRecv = recv })

-- | The peer has sent every lossless packet before the given number.
setReceivedEnd :: Word32 -> ReliabilityState -> Maybe ReliabilityState
setReceivedEnd num rs
  | num - paStart recv > bufferSize = Nothing
  | num - paEnd recv > bufferSize = Nothing
  | otherwise = Just rs { rsRecv = recv { paEnd = num } }
  where
    recv = rsRecv rs

-- | A request packet of at most @maxLength@ bytes for the packets missing from
-- the receive buffer.
requestPacket :: Int -> ReliabilityState -> ByteString
requestPacket maxLength rs =
  BS.pack $ packetIdRequest : take (maxLength - 1) (go 1 (arrayRange recv))
  where
    recv = rsRecv rs

    go :: Word32 -> [Word32] -> [Word8]
    go _ [] = []
    go n (i:is)
      | Map.notMember i (paPackets recv) = fromIntegral n : go 1 is
      | n == 255                         = 0 : go 1 is
      | otherwise                        = go (n + 1) is


{-------------------------------------------------------------------------------
 -
 - :: Data packet contents.
 -
 ------------------------------------------------------------------------------}

-- | The plaintext of a data packet.
encodeData :: Word32 -> Word32 -> ByteString -> ByteString
encodeData bufferStart num bytes = LBS.toStrict . runPut $ do
  putWord32be bufferStart
  putWord32be num
  putByteString $ BS.replicate padding 0
  putByteString bytes
  where
    padding = (maxCryptoDataSize - BS.length bytes) `mod` cryptoMaxPadding

-- | Split the plaintext of a data packet into the peer's receive buffer
-- start, the packet number and the data with the padding removed.
decodeData :: ByteString -> Maybe (Word32, Word32, ByteString)
decodeData bytes =
  case runGetOrFail header (LBS.fromStrict bytes) of
    Right (_, _, (bufferStart, num, rest))
      | not (BS.null payload) -> Just (bufferStart, num, payload)
      where
        payload = BS.dropWhile (== 0) $ LBS.toStrict rest
    _ -> Nothing
  where
    header = (,,) <$> getWord32be <*> getWord32be <*> getRemainingLazyByteString
\end{code}

Data in the encrypted packets:

\begin{verbatim}
[our recvbuffers buffer_start, (highest packet number handled + 1), (big endian)]
[uint32_t packet number if lossless, sendbuffer buffer_end if lossy, (big endian)]
[data]
\end{verbatim}

Encrypted packets may be lossy or lossless.  Lossy packets are simply encrypted
packets that are sent to the other.  If they are lost, arrive in the wrong
order or even if an attacker duplicates them (be sure to take this into account
for anything that uses lossy packets) they will simply be decrypted as they
arrive and passed upwards to what should handle them depending on the data id.

Lossless packets are packets containing data that will be delivered in order by
the implementation of the protocol.  In this protocol, the receiver tells the
sender which packet numbers he has received and which he has not and the sender
must resend any packets that are dropped.  Any attempt at doubling packets will
cause all (except the first received) to be ignored.

Each lossless packet contains both a 4 byte number indicating the highest
packet number received and processed and a 4 byte packet number which is the
packet number of the data in the packet.

In lossy packets, the layout is the same except that instead of a packet
number, the second 4 byte number represents the packet number of a lossless
packet if one were sent right after.  This number is used by the receiver to
know if any packets have been lost.  (for example if it receives 4 packets with
numbers (0, 1, 2, 5) and then later a lossy packet with this second number as:
8 it knows that packets: 3, 4, 6, 7 have been lost and will request them)

How the reliability is achieved:

First it is important to say that packet numbers do roll over, the next number
after 0xFFFFFFFF (maximum value in 4 bytes) is 0.  Hence, all the mathematical
operations dealing with packet numbers are assumed to be done only on unsigned
32 bit integer unless said otherwise.  For example 0 - 0xFFFFFFFF would equal
to 1 because of the rollover.

When sending a lossless packet, the packet is created with its packet number
being the number of the last lossless packet created + 1 (starting at 0).  The
packet numbers are used for both reliability and in ordered delivery and so
must be sequential.

The packet is then stored along with its packet number in order for the peer to
be able to send it again if the receiver does not receive it.  Packets are only
removed from storage when the receiver confirms they have received them.

The receiver receives packets and stores them along with their packet number.
When a receiver receives a packet he stores the packet along with its packet
number in an array.  If there is already a packet with that number in the
buffer, the packet is dropped.  If the packet number is smaller than the last
packet number that was processed, the packet is dropped.  A processed packet
means it was removed from the buffer and passed upwards to the relevant module.

Assuming a new connection, the sender sends 5 lossless packets to the receiver:
0, 1, 2, 3, 4 are the packet numbers sent and the receiver receives: 3, 2, 0, 2
in that order.

The receiver will save the packets and discards the second packet with the
number 2, he has: 0, 2, 3 in his buffer.  He will pass the first packet to the
relevant module and remove it from the array but since packet number 1 is
missing he will stop there.  Contents of the buffer are now: 2, 3.  The
receiver knows packet number 1 is missing and will request it from the sender
by using a packet request packet:

data ids:

\begin{tabular}{l|l}
  ID   & Data \\
  \hline
  0    & padding (skipped until we hit a non zero (data id) byte) \\
  1    & packet request packet (lossy packet) \\
  2    & connection kill packet (lossy packet) \\
  ...  & ... \\
  16+  & reserved for Messenger usage (lossless packets) \\
  192+ & reserved for Messenger usage (lossy packets) \\
  255  & reserved for Messenger usage (lossless packet) \\
\end{tabular}

Connection kill packets tell the other that the connection is over.

Packet numbers are the first byte of data in the packet.

packet request packet:

\begin{verbatim}
[uint8_t (1)][uint8_t num][uint8_t num][uint8_t num]...[uint8_t num]
\end{verbatim}

Packet request packets are used by one side of the connection to request
packets from the other.  To create a full packet request packet, the one
requesting the packet takes the last packet number that was processed (sent to
the relevant module and removed from the array (0 in the example above)).
Subtract the number of the first missing packet from that number (1 - 0) = 1.
Which means the full packet to request packet number 1 will look like:

\begin{verbatim}
[uint32_t 1]
[uint32_t 0]
[uint8_t 1][uint8_t 1]
\end{verbatim}

If packet number 4 was being requested as well, take the difference between the
packet number and the last packet number being requested (4 - 1) = 3.  So the
packet will look like:

\begin{verbatim}
[uint32_t 1]
[uint32_t 0]
[uint8_t 1][uint8_t 1][uint8_t 3]
\end{verbatim}

But what if the number is greater than 255? Let's say the peer needs to request
packets 3, 6, 1024, the packet will look like:

\begin{verbatim}
[uint32_t 1]
[uint32_t 2]
[uint8_t 1][uint8_t 3][uint8_t 3][uint8_t 0][uint8_t 0][uint8_t 0][uint8_t 253]
\end{verbatim}

Each 0 in the packet represents adding 255 until a non 0 byte is reached which
is then added and the resulting requested number is what is left.

This request is designed to be small when requesting packets in real network
conditions where the requested packet numbers will be close to each other.
Putting each requested 4 byte packet number would be very simple but would make
the request packets unnecessarily large which is why the packets look like
this.

When a request packet is received, it will be decoded and all packets in
between the requested packets will be assumed to be successfully received by
the other.

Packet request packets are sent at least every 1 second in toxcore and more
when packets are being received.

The current formula used is (note that this formula is likely sub-optimal):

\begin{verbatim}
REQUEST_PACKETS_COMPARE_CONSTANT = 50.0 double request_packet_interval =
(REQUEST_PACKETS_COMPARE_CONSTANT /
(((double)num_packets_array(&conn->recv_array) + 1.0) / (conn->packet_recv_rate
+ 1.0)));
\end{verbatim}

\texttt{num\_packets\_array(&conn->recv\_array)} returns the difference between
the highest packet number received and the last one handled.  In the toxcore
code it refers to the total size of the current array (with the holes which are
the placeholders for not yet received packets that are known to be missing).

\texttt{conn->packet\_recv\_rate} is the number of data packets successfully
received per second.

This formula was created with the logic that the higher the 'delay' in packets
(\texttt{num\_packets\_array(&conn->recv\_array)}) vs the speed of packets
received, the more request packets should be sent.

Requested packets are resent every time they can be resent as in they will obey
the congestion control and not bypass it.  They are resent once, subsequent
request packets will be used to know if the packet was received or if it should
be resent.
