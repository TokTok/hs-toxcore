\begin{code}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.DHT.RpcPacket where

import           Data.Binary               (Binary)
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word64)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A DHT RPC Service consists of a Request packet and a Response packet.  A DHT
RPC Packet contains a payload and a Request ID.  This ID is a 64 bit unsigned
integer that helps identify the response for a given request.

\begin{code}

newtype RequestId = RequestId Word64
  deriving (Eq, Read, Show, Binary, Arbitrary, Generic)

instance MessagePack RequestId

\end{code}

\input{src/Network/Tox/DHT/PendingReplies.lhs}

DHT RPC Packets are encrypted and transported within DHT Packets.

\begin{tabular}{l|l|l}
  Length             & Type               & \href{#dht-packet}{Contents} \\
  \hline
  \texttt{[0,]}      & Bytes              & Payload \\
  \texttt{8}         & \texttt{uint64\_t}  & Request ID \\
\end{tabular}

The minimum payload size is 0, but in reality the smallest sensible payload
size is 1.  Since the same symmetric key is used in both communication
directions, an encrypted Request would be a valid encrypted Response if they
contained the same plaintext.

\begin{code}

data RpcPacket payload = RpcPacket
  { rpcPayload :: payload
  , requestId  :: RequestId
  }
  deriving (Eq, Read, Show, Generic, Typeable)

instance Binary payload => Binary (RpcPacket payload)
instance MessagePack payload => MessagePack (RpcPacket payload)


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary payload => Arbitrary (RpcPacket payload) where
  arbitrary =
    RpcPacket <$> arbitrary <*> arbitrary
\end{code}

Parts of the protocol using RPC packets must take care to make Request payloads
not be valid Response payloads.  For instance, \href{#ping-service}{Ping
Packets} carry a boolean flag that indicate whether the payload corresponds to
a Request or a Response.

The Request ID provides some resistance against replay attacks.  If there were
no Request ID, it would be easy for an attacker to replay old responses and
thus provide nodes with out-of-date information.  A Request ID should be
randomly generated for each Request which is sent.
