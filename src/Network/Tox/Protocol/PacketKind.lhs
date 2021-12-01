\section{Packet Kind}

The following is an exhaustive list of top level packet kind names and their
number.  Their payload is specified in dedicated sections.  Each section is
named after the Packet Kind it describes followed by the byte value in
parentheses, e.g. \href{#ping-request-0x00}{Ping Request (0x00)}.

\begin{tabular}{l|l}
  Byte value        & Packet Kind \\
  \hline
  \texttt{0x00}     & Ping Request \\
  \texttt{0x01}     & Ping Response \\
  \texttt{0x02}     & Nodes Request \\
  \texttt{0x04}     & Nodes Response \\
  \texttt{0x18}     & Cookie Request \\
  \texttt{0x19}     & Cookie Response \\
  \texttt{0x1a}     & Crypto Handshake \\
  \texttt{0x1b}     & Crypto Data \\
  \texttt{0x20}     & DHT Request \\
  \texttt{0x21}     & LAN Discovery \\
  \texttt{0x80}     & Onion Request 0 \\
  \texttt{0x81}     & Onion Request 1 \\
  \texttt{0x82}     & Onion Request 2 \\
  \texttt{0x83}     & Announce Request \\
  \texttt{0x84}     & Announce Response \\
  \texttt{0x85}     & Onion Data Request \\
  \texttt{0x86}     & Onion Data Response \\
  \texttt{0x8c}     & Onion Response 3 \\
  \texttt{0x8d}     & Onion Response 2 \\
  \texttt{0x8e}     & Onion Response 1 \\
  \texttt{0xf0}     & Bootstrap Info \\
\end{tabular}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE StrictData         #-}
module Network.Tox.Protocol.PacketKind where

import           Control.Arrow             ((&&&))
import           Data.Binary               (Binary, get, put)
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word8)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary,
                                            arbitraryBoundedEnum)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data PacketKind
  = PingRequest
  | PingResponse
  | NodesRequest
  | NodesResponse
  | CookieRequest
  | CookieResponse
  | CryptoHandshake
  | CryptoData
  | Crypto
  | LanDiscovery
  | OnionRequest0
  | OnionRequest1
  | OnionRequest2
  | AnnounceRequest
  | AnnounceResponse
  | OnionDataRequest
  | OnionDataResponse
  | OnionResponse3
  | OnionResponse2
  | OnionResponse1
  | BootstrapInfo
  deriving (Eq, Read, Show, Bounded, Enum, Generic, Typeable)


instance MessagePack PacketKind


kindDescription :: PacketKind -> String
kindDescription = \case
  PingRequest       -> "Ping request"
  PingResponse      -> "Ping response"
  NodesRequest      -> "Nodes request"
  NodesResponse     -> "Nodes response"
  CookieRequest     -> "Cookie request"
  CookieResponse    -> "Cookie response"
  CryptoHandshake   -> "Crypto handshake"
  CryptoData        -> "Crypto data"
  Crypto            -> "Encrypted data"
  LanDiscovery      -> "LAN discovery"
  OnionRequest0     -> "Initial onion request"
  OnionRequest1     -> "First level wrapped onion request"
  OnionRequest2     -> "Second level wrapped onion request"
  AnnounceRequest   -> "Announce request"
  AnnounceResponse  -> "Announce response"
  OnionDataRequest  -> "Onion data request"
  OnionDataResponse -> "Onion data response"
  OnionResponse3    -> "Third level wrapped onion response"
  OnionResponse2    -> "Second level wrapped onion response"
  OnionResponse1    -> "First level wrapped onion response"
  BootstrapInfo     -> "Bootstrap node info request and response"


kindToByte :: PacketKind -> Word8
kindToByte = \case
  PingRequest       -> 0x00
  PingResponse      -> 0x01
  NodesRequest      -> 0x02
  NodesResponse     -> 0x04
  CookieRequest     -> 0x18
  CookieResponse    -> 0x19
  CryptoHandshake   -> 0x1a
  CryptoData        -> 0x1b
  Crypto            -> 0x20
  LanDiscovery      -> 0x21
  OnionRequest0     -> 0x80
  OnionRequest1     -> 0x81
  OnionRequest2     -> 0x82
  AnnounceRequest   -> 0x83
  AnnounceResponse  -> 0x84
  OnionDataRequest  -> 0x85
  OnionDataResponse -> 0x86
  OnionResponse3    -> 0x8c
  OnionResponse2    -> 0x8d
  OnionResponse1    -> 0x8e
  BootstrapInfo     -> 0xf0


byteToKind :: Word8 -> Maybe PacketKind
byteToKind =
  flip lookup mapping
  where
    mapping = map (kindToByte &&& id) [minBound..maxBound]


instance Binary PacketKind where
  put = put . kindToByte

  get = do
    byte <- get
    case byteToKind byte of
      Nothing   -> fail $ "no binary mapping for packet kind " ++ show byte
      Just kind -> return kind



{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary PacketKind where
  arbitrary = arbitraryBoundedEnum
\end{code}
