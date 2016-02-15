{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.KBucketsExtSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Network.Tox.Crypto.Key        (PublicKey)
import qualified Network.Tox.DHT.KBuckets      as KBuckets
import qualified Network.Tox.ExternalTest      as Test
import qualified Network.Tox.ExternalTest.Test as Test (Result (..), Test (..))
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)


testKBucketNodes :: Int -> PublicKey -> [NodeInfo] -> [NodeInfo] -> Test.Result [(KBuckets.KBucketIndex, [NodeInfo])]
testKBucketNodes _ baseKey nodes removedNodes =
  let
    kBucketsAfterAdd = foldl KBuckets.addNode (KBuckets.empty baseKey) nodes
    kBucketsAfterRemove = foldl KBuckets.removeNode kBucketsAfterAdd removedNodes
    result = KBuckets.getAllBuckets kBucketsAfterRemove
  in
  Test.Success result


spec :: Spec
spec = do
  it "computes bucket indices correctly" $
    property $ \pk1 pk2 ->
      Test.run Test.KBucketIndex (pk1, pk2) $
        Test.Success $ KBuckets.bucketIndex pk1 pk2

  it "adds nodes to the correct buckets" $
    property $ \baseKey nodes ->
      Test.run Test.KBucketNodes (KBuckets.defaultBucketSize, baseKey, nodes, []) $
        testKBucketNodes KBuckets.defaultBucketSize baseKey nodes []

  it "adds and removes nodes correctly" $
    property $ \baseKey nodes toRemove ->
      let
        removedNodes =
          case nodes of
            [] -> []
            _  -> take (abs toRemove `mod` length nodes) nodes
      in
      Test.run Test.KBucketNodes (KBuckets.defaultBucketSize, baseKey, nodes, removedNodes) $
        testKBucketNodes KBuckets.defaultBucketSize baseKey nodes removedNodes
