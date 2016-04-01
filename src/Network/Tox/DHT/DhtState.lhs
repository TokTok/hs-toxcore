\section{DHT node state}

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Network.Tox.DHT.DhtState where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.Crypto.KeyPair    (KeyPair)
import qualified Network.Tox.Crypto.KeyPair    as KeyPair
import           Network.Tox.DHT.KBuckets      (KBuckets)
import qualified Network.Tox.DHT.KBuckets      as KBuckets
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen           as Gen


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


\end{code}

Every DHT node contains a Key Pair called the DHT Key Pair.

A DHT node also stores a set of Node Infos of nodes that are close to its own
DHT public key.  It uses the \href{#k-buckets}{k-buckets} data structure for
this, with the local DHT Public Key as the base key.

\begin{code}

data DhtState = DhtState
  { dhtKeyPair    :: KeyPair
  , dhtKBuckets   :: KBuckets
  , dhtSearchList :: Map PublicKey DhtSearchEntry
  }
  deriving (Eq, Read, Show)


empty :: KeyPair -> DhtState
empty keyPair =
  DhtState keyPair (KBuckets.empty $ KeyPair.publicKey keyPair) Map.empty


data DhtSearchEntry = DhtSearchEntry
  { searchNode     :: Maybe NodeInfo
  , searchKBuckets :: KBuckets
  }
  deriving (Eq, Read, Show)


emptySearchEntry :: PublicKey -> DhtSearchEntry
emptySearchEntry =
  DhtSearchEntry Nothing . KBuckets.empty


addSearchNode :: DhtState -> PublicKey -> DhtState
addSearchNode dhtState@DhtState { dhtSearchList } searchKey =
  let
    searchEntry =
      Map.findWithDefault (emptySearchEntry searchKey) searchKey dhtSearchList
    updatedSearchList =
      Map.insert searchKey searchEntry dhtSearchList
  in
  dhtState { dhtSearchList = updatedSearchList }


removeSearchNode :: DhtState -> PublicKey -> DhtState
removeSearchNode dhtState@DhtState { dhtSearchList } searchKey =
  dhtState { dhtSearchList = Map.delete searchKey dhtSearchList }


foldBuckets :: DhtState -> (a -> KBuckets -> a) -> a -> a
foldBuckets DhtState { dhtKBuckets, dhtSearchList } f x =
  Map.foldl (\x' -> f x' . searchKBuckets) (f x dhtKBuckets) dhtSearchList


mapBuckets :: DhtState -> (KBuckets -> KBuckets) -> DhtState
mapBuckets dhtState@DhtState { dhtKBuckets, dhtSearchList } f =
  let
    mappedKBuckets   = f dhtKBuckets
    mappedSearchList = Map.map updateSearchBucket dhtSearchList
  in
  dhtState
    { dhtKBuckets   = mappedKBuckets
    , dhtSearchList = mappedSearchList
    }

  where
    updateSearchBucket entry@DhtSearchEntry { searchKBuckets } =
      entry { searchKBuckets = f searchKBuckets }


updateSearchNode :: PublicKey -> Maybe NodeInfo -> DhtState -> DhtState
updateSearchNode publicKey nodeInfo dhtState@DhtState { dhtSearchList } =
  dhtState { dhtSearchList = Map.adjust update publicKey dhtSearchList }
  where
    update entry = entry { searchNode = nodeInfo }


addNode :: DhtState -> NodeInfo -> DhtState
addNode dhtState nodeInfo =
  updateSearchNode (NodeInfo.publicKey nodeInfo) (Just nodeInfo) $
    mapBuckets dhtState $ flip KBuckets.addNode nodeInfo


removeNode :: DhtState -> PublicKey -> DhtState
removeNode dhtState publicKey =
  updateSearchNode publicKey Nothing $
    mapBuckets dhtState $ flip KBuckets.removeNode publicKey


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtState where
  arbitrary =
    empty <$> arbitrary

\end{code}
