\subsection{K-Buckets}

Every DHT stores a number of Node Infos of nodes that are close to its own DHT
public key.  For each \texttt{0 <= n < 256}, it stores up to \texttt{k} nodes
(currently, \texttt{k=8}). These lists are called "k-buckets".

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.DHT.KBuckets where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Word                     (Word8)
import           Network.Tox.Crypto.Key        (PublicKey)
import qualified Network.Tox.DHT.Distance      as Distance
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo (..))
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen           as Gen


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype KBucketIndex = KBucketIndex Word8
  deriving (Eq, Ord, Read, Show, Num, Binary, Enum)


data KBucketEntry = KBucketEntry
  { entryBaseKey :: PublicKey
  , entryNode    :: NodeInfo
  }
  deriving (Eq, Read, Show)

instance Ord KBucketEntry where
  compare a b =
    compare
      (distance a)
      (distance b)
    where
      distance entry =
        Distance.xorDistance (entryBaseKey entry) (publicKey $ entryNode entry)


newtype KBucket = KBucket
  { bucketNodes :: Set KBucketEntry
  }
  deriving (Eq, Read, Show)


emptyKBucket :: KBucket
emptyKBucket = KBucket Set.empty


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

The bucket index can be computed using the following function:
\texttt{bucketIndex(ownKey, nodeKey) = 255 - log_2(distance(ownKey, nodeKey))}.
This function is not defined when \texttt{ownKey == nodeKey}, meaning k-buckets
will never contain a Node Info about the local node.

\begin{code}

bucketIndex :: PublicKey -> PublicKey -> Maybe KBucketIndex
bucketIndex pk1 pk2 =
  fmap (\index -> 255 - fromIntegral index) $ Distance.log2 $ Distance.xorDistance pk1 pk2


getAllBuckets :: KBuckets -> [(KBucketIndex, [NodeInfo])]
getAllBuckets KBuckets { buckets } =
  map (\(index, bucket) -> (index, map entryNode $ Set.toList $ bucketNodes bucket)) (Map.toList buckets)


updateBucketForIndex :: KBuckets -> KBucketIndex -> (KBucket -> KBucket) -> KBuckets
updateBucketForIndex kBuckets@KBuckets { buckets } index f =
  let
    -- Find the old bucket or create a new empty one.
    updatedBucket = f $ Map.findWithDefault emptyKBucket index buckets
    -- Replace old bucket with updated bucket.
    updatedBuckets =
      if Set.null $ bucketNodes updatedBucket then
        Map.delete index buckets
      else
        Map.insert index updatedBucket buckets
  in
  kBuckets { buckets = updatedBuckets }


updateBucketForKey :: KBuckets -> PublicKey -> (KBucket -> KBucket) -> KBuckets
updateBucketForKey kBuckets key f =
  case bucketIndex (baseKey kBuckets) key of
    Nothing    -> kBuckets
    Just index -> updateBucketForIndex kBuckets index f


truncateSet :: Ord a => Int -> Set a -> Set a
truncateSet maxSize set =
  if Set.size set <= maxSize then
    set
  else
    -- Remove the greatest element until the set is small enough again.
    truncateSet maxSize $ Set.deleteMax set


addNodeToBucket :: Int -> KBucketEntry -> KBucket -> KBucket
addNodeToBucket maxSize entry bucket =
  KBucket $ truncateSet maxSize $ Set.insert entry $ bucketNodes bucket


addNode :: KBuckets -> NodeInfo -> KBuckets
addNode kBuckets nodeInfo =
  updateBucketForKey kBuckets (publicKey nodeInfo) $ \bucket ->
    let
      -- The new entry.
      entry = KBucketEntry (baseKey kBuckets) nodeInfo
    in
    -- Insert the entry into the bucket.
    addNodeToBucket (bucketSize kBuckets) entry bucket


removeNodeFromBucket :: KBucketEntry -> KBucket -> KBucket
removeNodeFromBucket entry bucket =
  KBucket $ Set.delete entry $ bucketNodes bucket


removeNode :: KBuckets -> NodeInfo -> KBuckets
removeNode kBuckets nodeInfo =
  updateBucketForKey kBuckets (publicKey nodeInfo) $ \bucket ->
    let
      -- The entry to remove.
      entry = KBucketEntry (baseKey kBuckets) nodeInfo
    in
    removeNodeFromBucket entry bucket


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary KBuckets where
  arbitrary =
    foldl addNode <$> (empty <$> arbitrary) <*> Gen.listOf arbitrary
\end{code}

Thus, each k-bucket contains only Node Infos for whose keys the following
holds: if node with key \texttt{nodeKey} is in k-bucket with index \texttt{n},
then \texttt{bucketIndex(ownKey, nodeKey) == n}.

The bucket index can be efficiently computed by determining the first bit at
which the two keys differ, starting from the most significant bit. So, if the
local DHT key starts with e.g. \texttt{0x80} and the bucketed node key starts
with \texttt{0x40}, then the bucket index for that node is 0. If the second bit
differs, the bucket index is 2. If the keys are almost exactly equal and only
the last bit differs, the bucket index is 255.
