\subsubsection{Nodes Request (0x02)}

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#rpc-services}{Contents} \\
  \hline
  \texttt{32}        & Public Key  & Requested DHT Public Key \\
\end{tabular}

The DHT Public Key sent in the request is the one the sender is searching for.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
module Network.Tox.DHT.NodesRequest where

import           Control.Applicative             ((<$>))
import           Data.Binary                     (Binary, get, put)
import           Data.MessagePack                (MessagePack)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Network.Tox.Crypto.Key          (PublicKey)
import           Network.Tox.Encoding            (getBool, putBool)
import qualified Network.Tox.Protocol.PacketKind as PacketKind
import           Test.QuickCheck.Arbitrary       (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen             as Gen


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data NodesRequest = NodesRequest
  { requestedKey :: PublicKey
  }
  deriving (Eq, Read, Show, Generic, Typeable)

instance Binary NodesRequest
instance MessagePack NodesRequest


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary NodesRequest where
  arbitrary = NodesRequest <$> arbitrary
\end{code}
