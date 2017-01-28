\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe           #-}
module Network.Tox.DHT.ClientNode where

import           Control.Applicative           ((<$>), (<*>))
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)

import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import           Network.Tox.Time              (Timestamp)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

data ClientNode = ClientNode
  { nodeInfo  :: NodeInfo
  , lastPing  :: Timestamp
  , pingCount :: Int
  }
  deriving (Eq, Read, Show)

newNode :: Timestamp -> NodeInfo -> ClientNode
newNode time node = ClientNode node time 0

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

instance Arbitrary ClientNode where
  arbitrary = ClientNode <$> arbitrary <*> arbitrary <*> arbitrary

\end{code}
