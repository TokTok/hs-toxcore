\subsection{Ping Service}

The Ping Service is used to periodically check if another node is still alive.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
module Network.Tox.DHT.PingPacket where

import           Data.Binary               (Binary)
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen       as Gen


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


\end{code}

A Ping Packet payload consists of just a boolean value saying whether it is a
request or a response.

The one byte boolean inside the encrypted payload is added to prevent peers
from creating a valid Ping Response from a Ping Request without decrypting the
packet and encrypting a new one.  Since symmetric encryption is used, the
encrypted Ping Response would be byte-wise equal to the Ping Request without
the discriminator byte.

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#rpc-services}{Contents} \\
  \hline
  \texttt{1}         & Bool        & Response flag: 0x00 for Request, 0x01 for Response \\
\end{tabular}

\subsubsection{Ping Request (0x00)}

A Ping Request is a Ping Packet with the response flag set to False.  When a
Ping Request is received and successfully decrypted, a Ping Response packet is
created and sent back to the requestor.

\subsubsection{Ping Response (0x01)}

A Ping Response is a Ping Packet with the response flag set to True.

\begin{code}


data PingPacket
  = PingRequest
  | PingResponse
  deriving (Eq, Read, Show, Generic, Typeable)

instance Binary PingPacket
instance MessagePack PingPacket


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary PingPacket where
  arbitrary =
    Gen.elements
      [ PingRequest
      , PingResponse
      ]
\end{code}
