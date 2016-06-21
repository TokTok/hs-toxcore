\section{DHT node state}

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Network.Tox.DHT.DhtState where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen           as Gen

import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.Crypto.KeyPair    (KeyPair)
import qualified Network.Tox.Crypto.KeyPair    as KeyPair
import           Network.Tox.DHT.KBuckets      (KBuckets)
import qualified Network.Tox.DHT.KBuckets      as KBuckets
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


\end{code}

Every DHT node contains the following state:

\begin{itemize}
  \item DHT Key Pair: The Key Pair used to communicate with other DHT nodes. It
    is immutable throughout the lifetime of the DHT node.
  \item DHT Close List: A set of Node Infos of nodes that are close to the
    DHT Public Key (public part of the DHT Key Pair).  The Close List is
    represented as a \href{#k-buckets}{k-buckets} data structure, with the DHT
    Public Key as the Base Key.
  \item DHT Search List: A list of Public Keys of nodes that the DHT node is
    searching for, associated with a DHT Search Entry.
\end{itemize}

\begin{code}

data DhtState = DhtState
  { dhtKeyPair    :: KeyPair
  , dhtCloseList  :: KBuckets
  , dhtSearchList :: Map PublicKey DhtSearchEntry
  }
  deriving (Eq, Read, Show)

\end{code}

A DHT node state is initialised using a Key Pair, which is stored in the state
as DHT Key Pair and as base key for the Close List. Both the Close and Search
Lists are initialised to be empty.

\begin{code}

empty :: KeyPair -> DhtState
empty keyPair =
  DhtState keyPair (KBuckets.empty $ KeyPair.publicKey keyPair) Map.empty

\end{code}

\subsection{DHT Search Entry}

A DHT Search Entry contains a k-buckets instance, which serves the same purpose
as the Close List, but the base key is the searched node's Public Key. Once the
searched node is found, it is also stored in the Search Entry. Recall that
k-buckets never contain a Node Info for the base key, so it must be stored
outside the k-buckets instance.

\begin{code}

data DhtSearchEntry = DhtSearchEntry
  { searchNode     :: Maybe NodeInfo
  , searchKBuckets :: KBuckets
  }
  deriving (Eq, Read, Show)

\end{code}

A Search Entry is initialised with the searched-for Public Key. The contained
k-buckets instance is initialised to be empty.

\begin{code}

emptySearchEntry :: PublicKey -> DhtSearchEntry
emptySearchEntry =
  DhtSearchEntry Nothing . KBuckets.empty

\end{code}

\subsection{Manipulating the DHT node state}

Adding a search node to the DHT node state creates an empty entry in the Search
Nodes list. If a search entry for the public key already existed, the "add"
operation has no effect.

\begin{code}

addSearchNode :: DhtState -> PublicKey -> DhtState
addSearchNode dhtState@DhtState { dhtSearchList } searchKey =
  let
    searchEntry =
      Map.findWithDefault (emptySearchEntry searchKey) searchKey dhtSearchList
    updatedSearchList =
      Map.insert searchKey searchEntry dhtSearchList
  in
  dhtState { dhtSearchList = updatedSearchList }

\end{code}

Removing a search node removes its search entry and all associated data
structures from memory.

\begin{code}

removeSearchNode :: DhtState -> PublicKey -> DhtState
removeSearchNode dhtState@DhtState { dhtSearchList } searchKey =
  dhtState { dhtSearchList = Map.delete searchKey dhtSearchList }

\end{code}

The iteration order over the DHT state is to first process the Close List
k-buckets, then the Search List entry k-buckets. Each list itself follows the
iteration order in the k-buckets specification.

\begin{code}

foldBuckets :: (a -> KBuckets -> a) -> a -> DhtState -> a
foldBuckets f x DhtState { dhtCloseList, dhtSearchList } =
  Map.foldl (\x' -> f x' . searchKBuckets) (f x dhtCloseList) dhtSearchList


foldNodes :: (a -> NodeInfo -> a) -> a -> DhtState -> a
foldNodes =
  foldBuckets . KBuckets.foldNodes

\end{code}

Adding and removing a node (Node Info) to the state is done by adding or
removing the node to each k-bucket in the state, i.e. the close list and all
the k-buckets in the search entries.

\begin{code}

updateSearchNode :: PublicKey -> Maybe NodeInfo -> DhtState -> DhtState
updateSearchNode publicKey nodeInfo dhtState@DhtState { dhtSearchList } =
  dhtState { dhtSearchList = Map.adjust update publicKey dhtSearchList }
  where
    update entry = entry { searchNode = nodeInfo }


mapBuckets :: DhtState -> (KBuckets -> KBuckets) -> DhtState
mapBuckets dhtState@DhtState { dhtCloseList, dhtSearchList } f =
  let
    mappedKBuckets   = f dhtCloseList
    mappedSearchList = Map.map updateSearchBucket dhtSearchList
  in
  dhtState
    { dhtCloseList  = mappedKBuckets
    , dhtSearchList = mappedSearchList
    }

  where
    updateSearchBucket entry@DhtSearchEntry { searchKBuckets } =
      entry { searchKBuckets = f searchKBuckets }

\end{code}

When adding a node to the state, the search entry for the node's public key, if
it exists, is updated to contain the new Node Info. All k-buckets that already
contain the node will also be updated. See the k-buckets specification for the
update algorithm.

\begin{code}

addNode :: DhtState -> NodeInfo -> DhtState
addNode dhtState nodeInfo =
  updateSearchNode (NodeInfo.publicKey nodeInfo) (Just nodeInfo) $
    mapBuckets dhtState $ flip KBuckets.addNode nodeInfo

\end{code}

Removing a node from the state unsets the Node Info in the search entry, if
such exists. The search entry itself is not removed. All k-buckets that
contained the node will no longer contain it after removing the node from the
state. The only reference to the node's public key will be the search entry, if
it exists.

\begin{code}

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
