{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtStateSpec where

import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                    (Proxy (..))

import           Network.Tox.DHT.DhtState      (DhtState)
import qualified Network.Tox.DHT.DhtState      as DhtState
import qualified Network.Tox.DHT.KBuckets      as KBuckets
import           Network.Tox.EncodingSpec
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy DhtState)

  describe "adding a node" $ do
    it "should result in a different state" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd = DhtState.addNode nodeInfo dhtState
        in
        afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd    = DhtState.addNode nodeInfo dhtState
          afterRemove = DhtState.removeNode (NodeInfo.publicKey nodeInfo) afterAdd
        in
        afterRemove `shouldBe` dhtState

    it "and adding it again does not change the state twice" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd1 = DhtState.addNode nodeInfo dhtState
          afterAdd2 = DhtState.addNode nodeInfo afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

    it "should make the state size equal to 1" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd = DhtState.addNode nodeInfo dhtState
        in
        DhtState.size afterAdd `shouldBe` 1

  describe "adding a search node" $ do
    it "should result in a different state" $
      property $ \dhtState publicKey ->
        let
          afterAdd = DhtState.addSearchKey publicKey dhtState
        in
        afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \dhtState publicKey ->
        let
          afterAdd    = DhtState.addSearchKey publicKey dhtState
          afterRemove = DhtState.removeSearchKey publicKey afterAdd
        in
        afterRemove `shouldBe` dhtState

    it "and adding it again does not change the state twice" $
      property $ \dhtState publicKey ->
        let
          afterAdd1 = DhtState.addSearchKey publicKey dhtState
          afterAdd2 = DhtState.addSearchKey publicKey afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

    it "and adding a node info for it will contain the node info twice" $
      property $ \dhtState nodeInfo ->
        let
          afterAddSearchKey =
            DhtState.addSearchKey (NodeInfo.publicKey nodeInfo) dhtState
          afterAddNode =
            DhtState.addNode nodeInfo afterAddSearchKey
        in do
        print afterAddNode
        DhtState.size afterAddNode `shouldBe` 2
