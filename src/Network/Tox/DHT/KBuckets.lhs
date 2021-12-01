\section{K-buckets}

K-buckets is a data structure for efficiently storing a set of nodes close to a
certain key called the base key.  The base key is constant throughout the
lifetime of a k-buckets instance.

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.DHT.KBuckets where

import           Control.Applicative           (Applicative, (<$>))
import           Data.Binary                   (Binary)
import           Data.Foldable                 (toList)
import           Data.List                     (sortBy)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (isJust)
import           Data.Ord                      (comparing)
import           Data.Traversable              (Traversable, mapAccumR,
                                                traverse)
import           Data.Word                     (Word8)
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen           (Gen)
import qualified Test.QuickCheck.Gen           as Gen

import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.DHT.ClientList    (ClientList)
import qualified Network.Tox.DHT.ClientList    as ClientList
import           Network.Tox.DHT.Distance      (Distance)
import qualified Network.Tox.DHT.Distance      as Distance
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo
import           Network.Tox.Time              (Timestamp)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A k-buckets is a map from small integers \texttt{0 <= n < 256} to Client Lists
of maximum size $k$. Each Client List is called a (k-)bucket. A k-buckets is
equipped with a base key, and each bucket has this key as its base key.
\texttt{k} is called the bucket size.  The default bucket size is 8.
A large bucket size was chosen to increase the speed at which peers are found.

\begin{code}

data KBuckets = KBuckets
  { bucketSize :: Int
  , buckets    :: Map KBucketIndex ClientList
  , baseKey    :: PublicKey
  }
  deriving (Eq, Read, Show)


defaultBucketSize :: Int
defaultBucketSize = 8


empty :: PublicKey -> KBuckets
empty = KBuckets defaultBucketSize Map.empty

\end{code}

The above number \texttt{n} is the bucket index.  It is a non-negative integer
with the range \texttt{[0, 255]}, i.e. the range of an 8 bit unsigned integer.

\begin{code}


newtype KBucketIndex = KBucketIndex Word8
  deriving (Eq, Ord, Read, Show, Num, Binary, Enum)


\end{code}

\subsection{Bucket Index}

The index of the bucket can be computed using the following function:
\texttt{bucketIndex(baseKey, nodeKey) = 255 - log\_2(distance(baseKey,
nodeKey))}.  This function is not defined when \texttt{baseKey == nodeKey},
meaning k-buckets will never contain a Node Info about the base node.

Thus, each k-bucket contains only Node Infos for whose keys the following
holds: if node with key \texttt{nodeKey} is in k-bucket with index \texttt{n},
then \texttt{bucketIndex(baseKey, nodeKey) == n}. Thus, n'th k-bucket consists
of nodes for which distance to the base node lies in range
\verb![2^n, 2^(n+1) - 1]!.

The bucket index can be efficiently computed by determining the first bit at
which the two keys differ, starting from the most significant bit.  So, if the
local DHT key starts with e.g. \texttt{0x80} and the bucketed node key starts
with \texttt{0x40}, then the bucket index for that node is 0.  If the second
bit differs, the bucket index is 1.  If the keys are almost exactly equal and
only the last bit differs, the bucket index is 255.

\begin{code}


bucketIndex :: PublicKey -> PublicKey -> Maybe KBucketIndex
bucketIndex pk1 pk2 =
  fmap (\index -> 255 - fromIntegral index) $ Distance.log2 $ Distance.xorDistance pk1 pk2


\end{code}

\subsection{Manipulating k-buckets}

TODO: this is different from kademlia's least-recently-seen eviction policy; why
the existing solution was chosen, how does it affect security, performance and
resistance to poisoning? original paper claims that preference of old live nodes
results in better persistence and resistance to basic DDoS attacks;

Any update or lookup operation on a k-buckets instance that involves a single
node requires us to first compute the bucket index for that node.  An update
involving a Node Info with \texttt{nodeKey == baseKey} has no effect.  If the
update results in an empty bucket, that bucket is removed from the map.

\begin{code}


updateBucketForKey :: KBuckets -> PublicKey -> (ClientList -> ClientList) -> KBuckets
updateBucketForKey kBuckets key f =
  case bucketIndex (baseKey kBuckets) key of
    Nothing    -> kBuckets
    Just index -> updateBucketForIndex kBuckets index f


updateBucketForIndex :: KBuckets -> KBucketIndex -> (ClientList -> ClientList) -> KBuckets
updateBucketForIndex kBuckets@KBuckets { buckets, baseKey, bucketSize } index f =
  let
    -- Find the old bucket or create a new empty one.
    updatedBucket = f $ Map.findWithDefault (ClientList.empty baseKey bucketSize) index buckets
    -- Replace old bucket with updated bucket or delete if empty.
    updatedBuckets =
      if ClientList.isEmpty updatedBucket
      then Map.delete index buckets
      else Map.insert index updatedBucket buckets
  in
  kBuckets { buckets = updatedBuckets }


\end{code}

Adding a node to, or removing a node from, a k-buckets consists of performing
the corresponding operation on the Client List bucket whose index is that of
the node's public key, except that adding a new node to a full bucket has no
effect.  A node is considered \textit{viable} for entry if the corresponding
bucket is not full.

\begin{code}

addNode :: Timestamp -> NodeInfo -> KBuckets -> KBuckets
addNode time nodeInfo kBuckets =
  updateBucketForKey kBuckets publicKey $ \clientList ->
    let
      full = ClientList.full clientList
      alreadyIn = isJust $ ClientList.lookup publicKey clientList
    in
    if not full || alreadyIn
      then ClientList.addNode time nodeInfo clientList
      else clientList
  where
    publicKey = NodeInfo.publicKey nodeInfo

removeNode :: PublicKey -> KBuckets -> KBuckets
removeNode publicKey kBuckets =
  updateBucketForKey kBuckets publicKey $ ClientList.removeNode publicKey

viable :: NodeInfo -> KBuckets -> Bool
viable nodeInfo KBuckets{ baseKey, buckets } =
  case bucketIndex baseKey $ NodeInfo.publicKey nodeInfo of
    Nothing    -> False
    Just index -> case Map.lookup index buckets of
      Nothing     -> True
      Just bucket -> not $ ClientList.full bucket

\end{code}

Iteration order of a k-buckets instance is in order of distance from the base
key.  I.e. the first node seen in iteration is the closest, and the last node
is the furthest away in terms of the distance metric.

\begin{code}

traverseClientLists ::
    Applicative f => (ClientList -> f ClientList) -> KBuckets -> f KBuckets
traverseClientLists f kBuckets@KBuckets{ buckets } =
  (\x -> kBuckets{ buckets = x }) <$> traverse f (reverseT buckets)
  where
    reverseT :: (Traversable t) => t a -> t a
    reverseT t = snd (mapAccumR (\ (x:xs) _ -> (xs, x)) (toList t) t)

closeNodes :: PublicKey -> KBuckets -> [ (Distance, NodeInfo) ]
closeNodes publicKey KBuckets{ baseKey, buckets } =
  let
    (further, at, nearer) = case bucketIndex baseKey publicKey of
      Nothing    -> (buckets, Nothing, Map.empty)
      Just index -> Map.splitLookup index buckets
    clientClose = ClientList.closeNodes publicKey
    bucketsClose = sortBy (comparing fst) . concatMap clientClose
  in
    concat
      [ maybe [] clientClose at
      , bucketsClose $ Map.elems nearer
      , bucketsClose $ Map.elems further
      ]


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


getAllNodes :: KBuckets -> [NodeInfo]
getAllNodes =
  concatMap ClientList.nodeInfos . Map.elems . buckets


genKBuckets :: PublicKey -> Gen KBuckets
genKBuckets publicKey =
  foldl (flip $ uncurry addNode) (empty publicKey) <$> Gen.listOf arbitrary


instance Arbitrary KBuckets where
  arbitrary = arbitrary >>= genKBuckets
\end{code}
