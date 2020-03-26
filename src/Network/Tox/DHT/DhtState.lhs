\section{DHT node state}

\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Safe                  #-}
module Network.Tox.DHT.DhtState where

import           Control.Applicative            (Applicative, Const (..),
                                                 getConst, (<$>), (<*>), (<|>))
import           Data.Functor.Identity          (Identity (..))
import           Data.List                      (nub, sortBy)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import           Data.Monoid                    (All (..), Monoid, getAll)
import           Data.Ord                       (comparing)
import           Data.Traversable               (traverse)
import           Lens.Family2                   (Lens')
import           Test.QuickCheck.Arbitrary      (Arbitrary, arbitrary, shrink)

import           Network.Tox.Crypto.Key         (PublicKey)
import           Network.Tox.Crypto.KeyPair     (KeyPair)
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import           Network.Tox.DHT.ClientList     (ClientList)
import qualified Network.Tox.DHT.ClientList     as ClientList
import           Network.Tox.DHT.Distance       (Distance)
import           Network.Tox.DHT.KBuckets       (KBuckets)
import qualified Network.Tox.DHT.KBuckets       as KBuckets
import           Network.Tox.DHT.NodeList       (NodeList)
import qualified Network.Tox.DHT.NodeList       as NodeList
import           Network.Tox.DHT.PendingReplies (PendingReplies)
import qualified Network.Tox.DHT.Stamped        as Stamped
import           Network.Tox.NodeInfo.NodeInfo  (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo  as NodeInfo
import           Network.Tox.Time               (Timestamp)


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

data ListStamp = ListStamp { listTime :: Timestamp, listBootstrappedTimes :: Int }
  deriving (Eq, Read, Show)
newListStamp :: Timestamp -> ListStamp
newListStamp t = ListStamp t 0

data DhtState = DhtState
  { dhtKeyPair        :: KeyPair
  , dhtCloseList      :: KBuckets
  , dhtSearchList     :: Map PublicKey DhtSearchEntry

  , dhtCloseListStamp :: ListStamp
  , dhtPendingReplies :: PendingReplies
  }
  deriving (Eq, Read, Show)

_dhtKeyPair :: Lens' DhtState KeyPair
_dhtKeyPair f d@DhtState{ dhtKeyPair = a } =
  (\a' -> d{ dhtKeyPair = a' }) <$> f a

_dhtCloseListStamp :: Lens' DhtState ListStamp
_dhtCloseListStamp f d@DhtState{ dhtCloseListStamp = a } =
  (\a' -> d{ dhtCloseListStamp = a' }) <$> f a

_dhtCloseList :: Lens' DhtState KBuckets
_dhtCloseList f d@DhtState{ dhtCloseList = a } =
  (\a' -> d{ dhtCloseList = a' }) <$> f a

_dhtSearchList :: Lens' DhtState (Map PublicKey DhtSearchEntry)
_dhtSearchList f d@DhtState{ dhtSearchList = a } =
  (\a' -> d{ dhtSearchList = a' }) <$> f a

_dhtPendingReplies :: Lens' DhtState PendingReplies
_dhtPendingReplies f d@DhtState{ dhtPendingReplies = a } =
  (\a' -> d{ dhtPendingReplies = a' }) <$> f a

\end{code}

A DHT node state is initialised using a Key Pair, which is stored in the state
as DHT Key Pair and as base key for the Close List. Both the Close and Search
Lists are initialised to be empty.

\begin{code}

empty :: Timestamp -> KeyPair -> DhtState
empty time keyPair =
  DhtState keyPair (KBuckets.empty $ KeyPair.publicKey keyPair)
    Map.empty (newListStamp time) Stamped.empty

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
  , searchStamp      :: ListStamp
  , searchClientList :: ClientList
  }
  deriving (Eq, Read, Show)

_searchNode :: Lens' DhtSearchEntry (Maybe NodeInfo)
_searchNode f d@DhtSearchEntry{ searchNode = a } =
  (\a' -> d{ searchNode = a' }) <$> f a

_searchStamp :: Lens' DhtSearchEntry ListStamp
_searchStamp f d@DhtSearchEntry{ searchStamp = a } =
  (\a' -> d{ searchStamp = a' }) <$> f a

_searchClientList :: Lens' DhtSearchEntry ClientList
_searchClientList f d@DhtSearchEntry{ searchClientList = a } =
  (\a' -> d{ searchClientList = a' }) <$> f a

searchEntryClientListSize :: Int
searchEntryClientListSize = 8

\end{code}

A Search Entry is initialised with the searched-for Public Key. The contained
Client List is initialised to be empty.

\begin{code}

emptySearchEntry :: Timestamp -> PublicKey -> DhtSearchEntry
emptySearchEntry time publicKey =
  DhtSearchEntry Nothing (newListStamp time) $
    ClientList.empty publicKey searchEntryClientListSize

\end{code}

\subsection{Manipulating the DHT node state}

Adding a search key to the DHT node state creates an empty entry in the Search
Nodes list. If a search entry for the public key already existed, the "add"
operation has no effect.

\begin{code}

addSearchKey :: Timestamp -> PublicKey -> DhtState -> DhtState
addSearchKey time searchKey dhtState@DhtState { dhtSearchList } =
  dhtState { dhtSearchList = updatedSearchList }
  where
    searchEntry =
      Map.findWithDefault (emptySearchEntry time searchKey) searchKey dhtSearchList
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

\input{src/tox/Network/Tox/DHT/NodeList.lhs}

The iteration order over the DHT state is to first process the Close List
k-buckets, then the Search List entry Client Lists. Each of these follows the
iteration order in the corresponding specification.

\begin{code}

traverseNodeLists :: Applicative f =>
  (forall l. NodeList l => l -> f l) -> DhtState -> f DhtState
traverseNodeLists f dhtState@DhtState{ dhtCloseList, dhtSearchList } =
  (\close' search' ->
      dhtState{ dhtCloseList = close', dhtSearchList = search' }) <$>
    f dhtCloseList <*>
    traverse traverseEntry dhtSearchList
  where
    traverseEntry entry =
      (\x -> entry{ searchClientList = x }) <$> f (searchClientList entry)

foldMapNodeLists :: Monoid m =>
  (forall l. NodeList l => l -> m) -> DhtState -> m
foldMapNodeLists f = getConst . traverseNodeLists (Const . f)

mapNodeLists :: (forall l. NodeList l => l -> l) -> DhtState -> DhtState
mapNodeLists f = runIdentity . traverseNodeLists (Identity . f)

\end{code}

A node info is considered to be contained in the DHT State if it is contained
in the Close List or in at least one of the Search Entries.

The size of the DHT state is defined to be the number of node infos it
contains, counted with multiplicity: node infos contained multiple times, e.g.
in the close list and in various search entries, are counted as many times as
they appear.  Search keys do not directly count towards the state size.  So
the size of the state is the sum of the sizes of the Close List and the Search
Entries.

The state size is relevant to later pruning algorithms that decide when to
remove a node info and when to request a ping from stale nodes. Search keys,
once added, are never automatically pruned.

\begin{code}

size :: DhtState -> Int
size = NodeList.foldNodes (flip $ const (1 +)) 0

\end{code}

Adding a Node Info to the state is done by adding the node to each Node List
in the state.

When adding a node info to the state, the search entry for the node's public
key, if it exists, is updated to contain the new node info. All k-buckets and
Client Lists that already contain the node info will also be updated. See the
corresponding specifications for the update algorithms. However, a node info
will not be added to a search entry when it is the node to which the search
entry is associated (i.e. the node being search for).

\begin{code}

addNode :: Timestamp -> NodeInfo -> DhtState -> DhtState
addNode time nodeInfo =
  updateSearchNode (NodeInfo.publicKey nodeInfo) (Just nodeInfo)
  . mapNodeLists addUnlessBase
  where
    addUnlessBase nodeList
      | NodeInfo.publicKey nodeInfo == NodeList.baseKey nodeList = nodeList
    addUnlessBase nodeList = NodeList.addNode time nodeInfo nodeList

removeNode :: PublicKey -> DhtState -> DhtState
removeNode publicKey =
  updateSearchNode publicKey Nothing
  . mapNodeLists (NodeList.removeNode publicKey)

viable :: NodeInfo -> DhtState -> Bool
viable nodeInfo = getAll . foldMapNodeLists (All . NodeList.viable nodeInfo)

traverseClientLists ::
  Applicative f => (ClientList -> f ClientList) -> DhtState -> f DhtState
traverseClientLists f = traverseNodeLists $ NodeList.traverseClientLists f

closeNodes :: PublicKey -> DhtState -> [ (Distance, NodeInfo) ]
closeNodes publicKey =
  nub . sortBy (comparing fst) . foldMapNodeLists (NodeList.closeNodes publicKey)

-- | although it is not referred to as a Node List in the spec, we make DhtState
-- an instance of NodeList so we can use the traversal and folding functions.
instance NodeList DhtState where
  addNode = addNode
  removeNode = removeNode
  viable = viable
  baseKey = KeyPair.publicKey . dhtKeyPair
  traverseClientLists = traverseClientLists
  closeNodes = closeNodes

takeClosestNodesTo :: Int -> PublicKey -> DhtState -> [ NodeInfo ]
takeClosestNodesTo n publicKey = map snd . take n . closeNodes publicKey

mapBuckets :: (KBuckets -> KBuckets) -> DhtState -> DhtState
mapBuckets f dhtState@DhtState { dhtCloseList } =
  dhtState
    { dhtCloseList = f dhtCloseList
    }

mapSearchEntry :: (DhtSearchEntry -> DhtSearchEntry) -> DhtState -> DhtState
mapSearchEntry f dhtState@DhtState { dhtSearchList } =
  dhtState
    { dhtSearchList = Map.map f dhtSearchList
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

containsNode :: PublicKey -> DhtState -> Bool
containsNode publicKey =
  NodeList.foldNodes (\a x -> a || NodeInfo.publicKey x == publicKey) False


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtState where
  arbitrary =
    initialise <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    where
      initialise :: Timestamp -> KeyPair -> [(Timestamp, NodeInfo)] -> [(Timestamp, PublicKey)] -> DhtState
      initialise time kp nis =
        foldl (flip $ uncurry addSearchKey) (foldl (flip $ uncurry NodeList.addNode) (empty time kp) nis)

  shrink dhtState =
    Maybe.maybeToList shrunkNode ++ Maybe.maybeToList shrunkSearchKey
    where
      -- Remove the first node we can find in the state.
      shrunkNode = do
        firstPK <- NodeInfo.publicKey <$> NodeList.foldNodes (\a x -> a <|> Just x) Nothing dhtState
        return $ NodeList.removeNode firstPK dhtState

      shrunkSearchKey = Nothing

\end{code}
