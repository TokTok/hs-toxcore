\section{K-buckets}

K-buckets is a data structure for efficiently storing a set of nodes close to a
certain key called the base key.  The base key is constant throughout the
lifetime of a k-buckets instance.

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.DHT.KBuckets where

import           Control.Applicative           ((<$>))
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Ord                      (comparing)
import           Data.Word                     (Word8)
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen           (Gen)
import qualified Test.QuickCheck.Gen           as Gen

import           Network.Tox.Crypto.Key        (PublicKey)
import qualified Network.Tox.DHT.Distance      as Distance
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A k-buckets is a map from small integers \texttt{0 <= n < 256} to a set of up
to \texttt{k} Node Infos.  The set is called a bucket.  \texttt{k} is called
the bucket size.  The default bucket size is 8.

\begin{code}


data KBuckets = KBuckets
  { bucketSize :: Int
  , buckets    :: Map KBucketIndex KBucket
  , baseKey    :: PublicKey
  }
  deriving (Eq, Read, Show)


defaultBucketSize :: Int
defaultBucketSize = 8


empty :: PublicKey -> KBuckets
empty = KBuckets defaultBucketSize Map.empty


\end{code}

The number \texttt{n} is the bucket index.  It is positive integer with the
range \texttt{[0, 255]}, i.e. the range of an 8 bit unsigned integer.

\begin{code}


newtype KBucketIndex = KBucketIndex Word8
  deriving (Eq, Ord, Read, Show, Num, Binary, Enum)


\end{code}

A bucket entry is an element of the bucket.  The bucket is an ordered set, and
the entries are sorted by \href{#distance}{distance} from the base key.  Thus,
the first (smallest) element of the set is the closest one to the base key in
that set, the last (greatest) element is the furthest away.

\begin{code}


newtype KBucket = KBucket
  { bucketNodes :: Map PublicKey KBucketEntry
  }
  deriving (Eq, Read, Show)


emptyBucket :: KBucket
emptyBucket = KBucket Map.empty


bucketIsEmpty :: KBucket -> Bool
bucketIsEmpty = Map.null . bucketNodes


data KBucketEntry = KBucketEntry
  { entryBaseKey :: PublicKey
  , entryNode    :: NodeInfo
  }
  deriving (Eq, Read, Show)

instance Ord KBucketEntry where
  compare = comparing distance
    where
      distance entry =
        Distance.xorDistance
          (entryBaseKey entry)
          (NodeInfo.publicKey $ entryNode entry)


entryPublicKey :: KBucketEntry -> PublicKey
entryPublicKey = NodeInfo.publicKey . entryNode


\end{code}

\subsection{Bucket Index}

The bucket index can be computed using the following function:
\texttt{bucketIndex(baseKey, nodeKey) = 255 - log_2(distance(baseKey,
nodeKey))}.  This function is not defined when \texttt{baseKey == nodeKey},
meaning k-buckets will never contain a Node Info about the local node.

Thus, each k-bucket contains only Node Infos for whose keys the following
holds: if node with key \texttt{nodeKey} is in k-bucket with index \texttt{n},
then \texttt{bucketIndex(baseKey, nodeKey) == n}.

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

Any update or lookup operation on a k-buckets instance that involves a single
node requires us to first compute the bucket index for that node.  An update
involving a Node Info with \texttt{nodeKey == baseKey} has no effect.  If the
update results in an empty bucket, that bucket is removed from the map.

\begin{code}


updateBucketForKey :: KBuckets -> PublicKey -> (KBucket -> KBucket) -> KBuckets
updateBucketForKey kBuckets key f =
  case bucketIndex (baseKey kBuckets) key of
    Nothing    -> kBuckets
    Just index -> updateBucketForIndex kBuckets index f


updateBucketForIndex :: KBuckets -> KBucketIndex -> (KBucket -> KBucket) -> KBuckets
updateBucketForIndex kBuckets@KBuckets { buckets } index f =
  let
    -- Find the old bucket or create a new empty one.
    updatedBucket = f $ Map.findWithDefault emptyBucket index buckets
    -- Replace old bucket with updated bucket or delete if empty.
    updatedBuckets =
      if bucketIsEmpty updatedBucket
      then Map.delete index buckets
      else Map.insert index updatedBucket buckets
  in
  kBuckets { buckets = updatedBuckets }


\end{code}

A bucket is \textit{full} when the bucket contains the maximum number of
entries configured by the bucket size.

A node is \textit{viable} for entry if the bucket is not \textit{full} or the
node's public key has a lower distance from the base key than the current entry
with the greatest distance.

If a node is \textit{viable} and the bucket is \textit{full}, the entry with
the greatest distance from the base key is removed to keep the bucket size
below the maximum configured bucket size.

Adding a node whose key already exists will result in an update of the Node
Info in the bucket.  Removing a node for which no Node Info exists in the
k-buckets has no effect.  Thus, removing a node twice is permitted and has the
same effect as removing it once.

\begin{code}


addNode :: NodeInfo -> KBuckets -> KBuckets
addNode nodeInfo kBuckets =
  updateBucketForKey kBuckets (NodeInfo.publicKey nodeInfo) $ \bucket ->
    let
      -- The new entry.
      entry = KBucketEntry (baseKey kBuckets) nodeInfo
    in
    -- Insert the entry into the bucket.
    addNodeToBucket (bucketSize kBuckets) entry bucket


addNodeToBucket :: Int -> KBucketEntry -> KBucket -> KBucket
addNodeToBucket maxSize entry =
  KBucket . truncateMap maxSize . Map.insert (entryPublicKey entry) entry . bucketNodes


truncateMap :: Ord a => Int -> Map k a -> Map k a
truncateMap maxSize m
  | Map.size m <= maxSize = m
  | otherwise =
      -- Remove the greatest element until the map is small enough again.
      truncateMap maxSize $ Map.deleteMax m


removeNodeFromBucket :: PublicKey -> KBucket -> KBucket
removeNodeFromBucket publicKey =
  KBucket . Map.delete publicKey . bucketNodes


removeNode :: PublicKey -> KBuckets -> KBuckets
removeNode publicKey kBuckets =
  updateBucketForKey kBuckets publicKey $ \bucket ->
    removeNodeFromBucket publicKey bucket

\end{code}

Iteration order of a k-buckets instance is in order of distance from the base
key.  I.e. the first node seen in iteration is the closest, and the last node
is the furthest away in terms of the distance metric.

\begin{code}

foldNodes :: (a -> NodeInfo -> a) -> a -> KBuckets -> a
foldNodes f x =
  foldl f x . concatMap (map entryNode . Map.elems . bucketNodes) . Map.elems . buckets


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


getAllNodes :: KBuckets -> [NodeInfo]
getAllNodes =
  concatMap (map entryNode . Map.elems . bucketNodes) . Map.elems . buckets


genKBuckets :: PublicKey -> Gen KBuckets
genKBuckets publicKey =
  foldl (flip addNode) (empty publicKey) <$> Gen.listOf arbitrary


instance Arbitrary KBuckets where
  arbitrary = arbitrary >>= genKBuckets
\end{code}
