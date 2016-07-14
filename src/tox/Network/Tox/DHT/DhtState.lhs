\section{DHT node state}

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
module Network.Tox.DHT.DhtState where

import           Control.Applicative           ((<$>), (<*>), (<|>))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary, shrink)

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
k-buckets never contain a node info for the base key, so it must be stored
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

Adding a search key to the DHT node state creates an empty entry in the Search
Nodes list. If a search entry for the public key already existed, the "add"
operation has no effect.

\begin{code}

addSearchKey :: PublicKey -> DhtState -> DhtState
addSearchKey searchKey dhtState@DhtState { dhtSearchList } =
  dhtState { dhtSearchList = updatedSearchList }
  where
    searchEntry =
      Map.findWithDefault (emptySearchEntry searchKey) searchKey dhtSearchList
    updatedSearchList =
      Map.insert searchKey searchEntry dhtSearchList

\end{code}

Removing a search key removes its search entry and all associated data
structures from memory.

\begin{code}

removeSearchKey :: PublicKey -> DhtState -> DhtState
removeSearchKey searchKey dhtState@DhtState { dhtSearchList } =
  dhtState { dhtSearchList = Map.delete searchKey dhtSearchList }


containsSearchKey :: PublicKey -> DhtState -> Bool
containsSearchKey searchKey =
  Map.member searchKey . dhtSearchList

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

The size of the DHT state is defined to be the number of node infos it
contains. Node infos contained multiple times, e.g. as part of the close list
and as part of various search entries, are counted as many times as they
appear.

Search keys do not directly count towards the state size. The state size is
relevant to later pruning algorithms that decide when to remove a node info and
when to request a ping from stale nodes. Search keys, once added, are never
automatically pruned.

\begin{code}

size :: DhtState -> Int
size = foldNodes (flip $ const (1 +)) 0

\end{code}

The bucket count of the state is the number of k-buckets instances. An empty
state contains one k-buckets instance. For each added search key, it contains
one additional k-buckets instance. Thus, the number of search keys is one less
than the bucket count.

\begin{code}

bucketCount :: DhtState -> Int
bucketCount = foldBuckets (flip $ const (1 +)) 0


updateSearchNode :: PublicKey -> Maybe NodeInfo -> DhtState -> DhtState
updateSearchNode publicKey nodeInfo dhtState@DhtState { dhtSearchList } =
  dhtState
    { dhtSearchList = Map.adjust update publicKey dhtSearchList
    }
  where
    update entry = entry { searchNode = nodeInfo }


mapBuckets :: (KBuckets -> KBuckets) -> DhtState -> DhtState
mapBuckets f dhtState@DhtState { dhtCloseList, dhtSearchList } =
  dhtState
    { dhtCloseList  = f dhtCloseList
    , dhtSearchList = Map.map updateSearchBucket dhtSearchList
    }
  where
    updateSearchBucket entry@DhtSearchEntry { searchKBuckets } =
      entry { searchKBuckets = f searchKBuckets }

\end{code}

Adding a node info to the state is done by adding the node to each k-bucket in
the state, i.e. the close list and all the k-buckets in the search entries.

When adding a node info to the state, the search entry for the node's public
key, if it exists, is updated to contain the new node info. All k-buckets that
already contain the node info will also be updated. See the k-buckets
specification for the update algorithm.

Recall that a k-buckets instance will never contain the node info for its base
key. Thus, when adding a node info for which a search entry exists, that node
info will not be added to the search entry's k-buckets instance.

\begin{code}

addNode :: NodeInfo -> DhtState -> DhtState
addNode nodeInfo =
  updateSearchNode (NodeInfo.publicKey nodeInfo) (Just nodeInfo)
  . mapBuckets (KBuckets.addNode nodeInfo)

\end{code}

Removing a node info from the state removes it from all k-buckets. If a search
entry for the removed node's public key existed, the node info in that search
entry is unset. The search entry itself is not removed.

\begin{code}

removeNode :: PublicKey -> DhtState -> DhtState
removeNode publicKey =
  updateSearchNode publicKey Nothing
  . mapBuckets (KBuckets.removeNode publicKey)


containsNode :: PublicKey -> DhtState -> Bool
containsNode publicKey =
  foldNodes (\a x -> a || NodeInfo.publicKey x == publicKey) False


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtState where
  arbitrary =
    initialise <$> arbitrary <*> arbitrary <*> arbitrary
    where
      initialise :: KeyPair -> [NodeInfo] -> [PublicKey] -> DhtState
      initialise kp nis =
        foldl (flip addSearchKey) (foldl (flip addNode) (empty kp) nis)

  shrink dhtState =
    Maybe.maybeToList shrunkNode ++ Maybe.maybeToList shrunkSearchKey
    where
      -- Remove the first node we can find in the state.
      shrunkNode = do
        firstPK <- NodeInfo.publicKey <$> foldNodes (\a x -> a <|> Just x) Nothing dhtState
        return $ removeNode firstPK dhtState

      shrunkSearchKey = Nothing

\end{code}
