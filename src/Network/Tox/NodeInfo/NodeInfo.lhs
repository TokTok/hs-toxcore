\section{Node Info (packed node format)}

The Node Info data structure contains a Transport Protocol, a Socket Address,
and a Public Key.  This is sufficient information to start communicating with
that node.  The binary representation of a Node Info is called the "packed node
format".

\begin{tabular}{l|l|l}
  Length             & Type               & Contents \\
  \hline
  \texttt{1} bit     & Transport Protocol & UDP = 0, TCP = 1 \\
  \texttt{7} bit     & Address Family     & 2 = IPv4, 10 = IPv6 \\
  \texttt{4 | 16}    & IP address         & 4 bytes for IPv4, 16 bytes for IPv6 \\
  \texttt{2}         & Port Number        & Port number \\
  \texttt{32}        & Public Key         & Node ID \\
\end{tabular}

The packed node format is a way to store the node info in a small yet easy to
parse format.  To store more than one node, simply append another one to the
previous one: \texttt{[packed node 1][packed node 2][...]}.

In the packed node format, the first byte (high bit protocol, lower 7 bits
address family) are called the IP Type.  The following table is informative and
can be used to simplify the implementation.

\begin{tabular}{l|l|l}
  IP Type               & Transport Protocol & Address Family \\
  \hline
  \texttt{2   (0x02)}   & UDP                & IPv4 \\
  \texttt{10  (0x0a)}   & UDP                & IPv6 \\
  \texttt{130 (0x82)}   & TCP                & IPv4 \\
  \texttt{138 (0x8a)}   & TCP                & IPv6 \\
\end{tabular}

The number \texttt{130} is used for an IPv4 TCP relay and \texttt{138} is used
to indicate an IPv6 TCP relay.

The reason for these numbers is because the numbers on Linux for IPv4 and IPv6
(the \texttt{AF_INET} and \texttt{AF_INET6} defines) are \texttt{2} and
\texttt{10}.  The TCP numbers are just the UDP numbers \texttt{+ 128}.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Trustworthy    #-}
module Network.Tox.NodeInfo.NodeInfo where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.Binary                            (Binary)
import qualified Data.Binary                            as Binary (get, put)
import           GHC.Generics                           (Generic)
import           Network.Tox.Crypto.Key                 (PublicKey)
import           Network.Tox.NodeInfo.SocketAddress     (SocketAddress)
import qualified Network.Tox.NodeInfo.SocketAddress     as SocketAddress
import           Network.Tox.NodeInfo.TransportProtocol (TransportProtocol)
import           Test.QuickCheck.Arbitrary              (Arbitrary, arbitrary)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data NodeInfo = NodeInfo
  { protocol  :: TransportProtocol
  , address   :: SocketAddress
  , publicKey :: PublicKey
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON NodeInfo
instance FromJSON NodeInfo


instance Binary NodeInfo where
  get =
    uncurry NodeInfo <$> SocketAddress.getSocketAddress <*> Binary.get

  put NodeInfo { protocol, address, publicKey } = do
    SocketAddress.putSocketAddress protocol address
    Binary.put publicKey


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary NodeInfo where
  arbitrary =
    NodeInfo <$> arbitrary <*> arbitrary <*> arbitrary
\end{code}
