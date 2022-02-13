\section{DHT Request Packets}
DHT Request packets are used to route encrypted data from a sender to another
node, referred to as the addressee of the packet, via a third node.

A DHT Request Packet is sent as the payload of a Protocol Packet with the
corresponding Packet Kind. It contains the DHT Public Key of an addressee, and a
DHT Packet which is to be received by the addressee.

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#protocol-packet}{Contents} \\
  \hline
  \texttt{32}        & Public Key  & Addressee DHT Public Key \\
  \texttt{[72,]}     & DHT Packet  & DHT Packet \\
\end{tabular}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE StrictData         #-}
module Network.Tox.DHT.DhtRequestPacket where

import           Data.Binary               (Binary, get, put)
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

import           Network.Tox.Crypto.Key    (PublicKey)
import           Network.Tox.DHT.DhtPacket (DhtPacket)

import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)



{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data DhtRequestPacket = DhtRequestPacket
  { addresseePublicKey :: PublicKey
  , dhtPacket          :: DhtPacket
  }
  deriving (Eq, Read, Show, Generic, Typeable)

instance MessagePack DhtRequestPacket


instance Binary DhtRequestPacket where
  put packet = do
    put $ addresseePublicKey packet
    put $ dhtPacket packet

  get =
    DhtRequestPacket <$> get <*> get

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtRequestPacket where
  arbitrary =
    DhtRequestPacket <$> arbitrary <*> arbitrary
\end{code}
