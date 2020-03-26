\subsubsection{Nodes Response (0x04)}

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#rpc-services}{Contents} \\
  \hline
  \texttt{1}         & Int         & Number of nodes in the response (maximum 4) \\
  \texttt{[39, 204]} & Node Infos  & Nodes in Packed Node Format \\
\end{tabular}

An IPv4 node is 39 bytes, an IPv6 node is 51 bytes, so the maximum size of the
packed Node Infos is \texttt{51 * 4 = 204} bytes.

Nodes responses should contain the 4 closest nodes that the sender of the
response has in their lists of known nodes.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
module Network.Tox.DHT.NodesResponse where

import           Control.Applicative           ((<$>))
import           Data.Binary                   (Binary, get, put)
import qualified Data.Binary.Get               as Binary (getWord8)
import qualified Data.Binary.Put               as Binary (putWord8)
import           Data.MessagePack              (MessagePack)
import           Data.Typeable                 (Typeable)
import           GHC.Generics                  (Generic)
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype NodesResponse = NodesResponse
  { foundNodes :: [NodeInfo]
  }
  deriving (Eq, Read, Show, Generic, Typeable)

instance MessagePack NodesResponse


instance Binary NodesResponse where
  put res = do
    Binary.putWord8 . fromInteger . toInteger . length . foundNodes $ res
    mapM_ put (foundNodes res)

  get = do
    count <- Binary.getWord8
    NodesResponse <$> mapM (const get) [1..count]


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary NodesResponse where
  arbitrary = NodesResponse <$> arbitrary
\end{code}
