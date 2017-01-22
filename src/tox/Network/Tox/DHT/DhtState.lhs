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
import           Network.Tox.DHT.ClientList    (ClientList)
import qualified Network.Tox.DHT.ClientList    as ClientList
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

A DHT Search Entry contains a Client List with base key the searched node's
Public Key.  Once the searched node is found, it is also stored in the Search
Entry.

The maximum size of the Client List is set to 8.
(Must be the same or smaller than the bucket size of the close list to make
sure all the closest peers found will know the node being searched
(TODO(zugz): this argument is unclear.)).

A DHT node state therefore contains one Client List for each bucket index in
the Close List, and one Client List for each DHT Search Entry.
These lists are not required to be disjoint - a node may be in multiple Client
Lists simultaneously.

\begin{code}

data DhtSearchEntry = DhtSearchEntry
  { searchNode       :: Maybe NodeInfo
  , searchClientList :: ClientList
  }
  deriving (Eq, Read, Show)

searchEntryClientListSize :: Int
searchEntryClientListSize = 8

\end{code}

A Search Entry is initialised with the searched-for Public Key. The contained
Client List is initialised to be empty.

\begin{code}

emptySearchEntry :: PublicKey -> DhtSearchEntry
emptySearchEntry publicKey =
  DhtSearchEntry Nothing $ ClientList.empty publicKey searchEntryClientListSize

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
k-buckets, then the Search List entry Client Lists. Each of these follows the
iteration order in the corresponding specification.

\begin{code}

foldNodes :: (a -> NodeInfo -> a) -> a -> DhtState -> a
foldNodes f x DhtState { dhtCloseList, dhtSearchList } =
  Map.foldl
    (\x' -> ClientList.foldNodes f x' . searchClientList)
    (KBuckets.foldNodes f x dhtCloseList)
    dhtSearchList

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

Adding a Node Info to the state is done by adding the node to the Close List
and to each Search Entry in the state

When adding a node info to the state, the search entry for the node's public
key, if it exists, is updated to contain the new node info. All k-buckets and
Client Lists that already contain the node info will also be updated. See the
corresponding specifications for the update algorithms. However, a node info
will not be added to a search entry when it is the node to which the search
entry is associated (i.e. the node being search for).

\begin{code}

addNode :: NodeInfo -> DhtState -> DhtState
addNode nodeInfo =
  updateSearchNode (NodeInfo.publicKey nodeInfo) (Just nodeInfo)
  . mapBuckets (KBuckets.addNode nodeInfo)
  . mapSearchClientLists addUnlessBase
  where
    addUnlessBase clientList
      | NodeInfo.publicKey nodeInfo == ClientList.baseKey clientList =
        clientList
    addUnlessBase clientList = ClientList.addNode nodeInfo clientList

mapBuckets :: (KBuckets -> KBuckets) -> DhtState -> DhtState
mapBuckets f dhtState@DhtState { dhtCloseList } =
  dhtState
    { dhtCloseList  = f dhtCloseList
    }

mapSearchEntry :: (DhtSearchEntry -> DhtSearchEntry) -> DhtState -> DhtState
mapSearchEntry f dhtState@DhtState { dhtSearchList } =
  dhtState
    { dhtSearchList  = Map.map f dhtSearchList
    }

mapSearchClientLists :: (ClientList -> ClientList) -> DhtState -> DhtState
mapSearchClientLists f =
    mapSearchEntry $ \entry@DhtSearchEntry{ searchClientList } ->
      entry { searchClientList = f searchClientList }

updateSearchNode :: PublicKey -> Maybe NodeInfo -> DhtState -> DhtState
updateSearchNode publicKey nodeInfo dhtState@DhtState { dhtSearchList } =
  dhtState
    { dhtSearchList = Map.adjust update publicKey dhtSearchList
    }
  where
    update entry = entry { searchNode = nodeInfo }

\end{code}

Removing a node info from the state removes it from all k-buckets. If a search
entry for the removed node's public key existed, the node info in that search
entry is unset. The search entry itself is not removed.

\begin{code}

removeNode :: PublicKey -> DhtState -> DhtState
removeNode publicKey =
  updateSearchNode publicKey Nothing
  . mapBuckets (KBuckets.removeNode publicKey)
  . mapSearchClientLists (ClientList.removeNode publicKey)


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
