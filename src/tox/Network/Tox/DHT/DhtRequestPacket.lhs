\section{DHT Request Packets}
DHT Request packets are used to route encrypted data from a sender to another
node, referred to as the addressee of the packet, via a third node.

A DHT Request Packet is a Protocol Packet. It contains the DHT Public Key of an
addressee, and a DHT Packet which is to be received by the addressee.

\begin{tabular}{l|l}
  Length             & Type        & \href{#protocol-packet}{Contents} \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x20) \\
  \texttt{32}        & addressee DHT public key \\
  \texttt{?}         & DHT Packet \\
\end{tabular}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE Safe               #-}
module Network.Tox.DHT.DhtRequestPacket where

import           Control.Applicative        ((<$>), (<*>))
import           Data.Binary                (Binary, get, put)
import           Data.MessagePack           (MessagePack)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)

import           Network.Tox.Crypto.Box     (PlainText (..))
import           Network.Tox.Crypto.Key     (Nonce, PublicKey)
import           Network.Tox.Crypto.KeyPair (KeyPair (..))
import           Network.Tox.DHT.DhtPacket  (DhtPacket)
import qualified Network.Tox.DHT.DhtPacket  as DhtPacket
import           Test.QuickCheck.Arbitrary  (Arbitrary, arbitrary)



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


encrypt :: KeyPair -> PublicKey -> Nonce -> PlainText -> DhtRequestPacket
encrypt keyPair addresseePublicKey nonce plainText =
  DhtRequestPacket addresseePublicKey $
    DhtPacket.encrypt keyPair addresseePublicKey nonce plainText


encode :: Binary payload => KeyPair -> PublicKey -> Nonce -> payload -> DhtRequestPacket
encode keyPair addresseePublicKey nonce payload =
  DhtRequestPacket addresseePublicKey $
    DhtPacket.encode keyPair addresseePublicKey nonce plainText payload

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtRequestPacket where
  arbitrary =
    DhtRequestPacket <$> arbitrary <*> arbitrary
\end{code}
