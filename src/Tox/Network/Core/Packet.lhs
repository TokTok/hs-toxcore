\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StrictData         #-}
module Tox.Network.Core.Packet where

import           Data.Binary                 (Binary, get, put)
import qualified Data.Binary.Get             as Binary (getRemainingLazyByteString)
import qualified Data.Binary.Put             as Binary (putByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.MessagePack            (MessagePack)
import           Data.Typeable               (Typeable)
import           GHC.Generics                (Generic)
import           Test.QuickCheck.Arbitrary   (Arbitrary, arbitrary)
import           Tox.Network.Core.PacketKind (PacketKind)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


\end{code}

A Protocol Packet is the top level Tox protocol element.  All other packet
types are wrapped in Protocol Packets.  It consists of a Packet Kind and a
payload.  The binary representation of a Packet Kind is a single byte (8 bits).
The payload is an arbitrary sequence of bytes.

\begin{tabular}{l|l|l}
  Length             & Type        & Contents \\
  \hline
  \texttt{1}         & Packet Kind & The packet kind identifier \\
  \texttt{[0,]}      & Bytes       & Payload \\
\end{tabular}

\begin{code}

-- | A newtype for ByteString that encodes/decodes without a length prefix.
newtype RawPayload = RawPayload { unRawPayload :: BS.ByteString }
  deriving (Eq, Show, Read, Generic, Typeable)

instance Binary RawPayload where
  put = Binary.putByteString . unRawPayload
  get = RawPayload . LBS.toStrict <$> Binary.getRemainingLazyByteString

instance Arbitrary RawPayload where
  arbitrary = RawPayload . BS.pack <$> arbitrary


data Packet payload = Packet
  { packetKind    :: PacketKind
  , packetPayload :: payload
  }
  deriving (Eq, Read, Show, Generic, Typeable, Functor)

instance Binary payload => Binary (Packet payload) where
  put (Packet kind payload) = put kind >> put payload
  get = Packet <$> get <*> get

instance MessagePack payload => MessagePack (Packet payload)

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary payload => Arbitrary (Packet payload) where
  arbitrary =
    Packet <$> arbitrary <*> arbitrary
\end{code}

These top level packets can be transported in a number of ways, the most common
way being over the network using UDP or TCP.  The protocol itself does not
prescribe transport methods, and an implementation is free to implement
additional transports such as WebRTC, IRC, or pipes.

In the remainder of the document, different kinds of Protocol Packet are
specified with their packet kind and payload.  The packet kind is not repeated
in the payload description (TODO: actually it mostly is, but later it won't).

Inside Protocol Packets payload, other packet types can specify additional
packet kinds.  E.g. inside a Crypto Data packet (\texttt{0x1b}), the
\href{#messenger}{Messenger} module defines its protocols for messaging, file
transfers, etc.  Top level Protocol Packets are themselves not encrypted,
though their payload may be.
