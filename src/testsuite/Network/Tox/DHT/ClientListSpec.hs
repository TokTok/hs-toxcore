{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.ClientListSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad                 (unless, when)
import           Data.List                     (sort, sortBy)
import qualified Data.Map                      as Map
import           Data.Ord                      (comparing)
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.DHT.ClientList    (ClientList)
import qualified Network.Tox.DHT.ClientList    as ClientList
import qualified Network.Tox.DHT.Distance      as Distance
import           Network.Tox.EncodingSpec
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy ClientList)

  it "has no more than maxSize elements" $
    property $ \clientList ->
      Map.size (ClientList.nodes clientList) `shouldSatisfy` (<= ClientList.maxSize clientList)

  it "removing a node twice has no effect" $
    property $ \baseKey time nodeInfo size ->
      let
        empty        = ClientList.empty baseKey
        afterAdd     = ClientList.addNode time nodeInfo $ empty size
        afterRemove0 = ClientList.removeNode (NodeInfo.publicKey nodeInfo) afterAdd
        afterRemove1 = ClientList.removeNode (NodeInfo.publicKey nodeInfo) afterRemove0
      in
      afterRemove0 `shouldBe` afterRemove1

  it "adding a node twice has no effect" $
    property $ \baseKey time nodeInfo size ->
      let
        empty        = ClientList.empty baseKey
        afterAdd0    = ClientList.addNode time nodeInfo $ empty size
        afterAdd1    = ClientList.addNode time nodeInfo afterAdd0
      in
      afterAdd0 `shouldBe` afterAdd1

  it "adding a non-viable node has no effect" $
    property $ \clientList time nodeInfo ->
      let
        viable   = ClientList.viable nodeInfo clientList
        afterAdd = ClientList.addNode time nodeInfo clientList
      in
      unless viable $ afterAdd `shouldBe` clientList

  describe "addNode" $
    it "keeps the k nodes closest to the base key" $
      property $ \clientList time nodeInfo ->
        let
          allNodes          = (nodeInfo:) $ ClientList.nodeInfos clientList
          keptNodes         = ClientList.nodeInfos $ ClientList.addNode time nodeInfo clientList
          nodeDistance node = Distance.xorDistance (ClientList.baseKey clientList) (NodeInfo.publicKey node)
          sortNodes         = sortBy $ comparing nodeDistance
        in
        take (ClientList.maxSize clientList) (sortNodes allNodes) `shouldBe` sortNodes keptNodes

  describe "foldNodes" $
    it "iterates over nodes in order of distance from the base key" $
      property $ \clientList ->
        let
          nodes             = reverse $ ClientList.foldNodes (flip (:)) [] clientList
          nodeDistance node = Distance.xorDistance (ClientList.baseKey clientList) (NodeInfo.publicKey node)
          sortNodes         = sortBy (comparing nodeDistance)
        in
        nodes `shouldBe` sortNodes nodes
