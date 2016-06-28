{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtStateSpec where

import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad                 (unless)
import           Data.Proxy                    (Proxy (..))
import           Text.Groom                    (groom)

import qualified Network.Tox.Crypto.KeyPair    as KeyPair
import           Network.Tox.DHT.DhtState      (DhtState)
import qualified Network.Tox.DHT.DhtState      as DhtState
import qualified Network.Tox.DHT.KBuckets      as KBuckets
import           Network.Tox.EncodingSpec
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy DhtState)

  it "the state can never contain itself" $
    property $ \keyPair nodeInfo ->
      let
        dhtState = DhtState.empty keyPair
        afterAdd = DhtState.addNode
          nodeInfo { NodeInfo.publicKey = KeyPair.publicKey keyPair }
          dhtState
      in
      afterAdd `shouldBe` dhtState

  it "the node can never search for itself" $
    property $ \dhtState ->
      let
        afterAdd = DhtState.addSearchKey
          (KeyPair.publicKey $ DhtState.dhtKeyPair dhtState)
          dhtState
      in
      afterAdd `shouldBe` dhtState

  describe "adding a node that was not yet contained" $ do
    it "should result in a different state" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd = DhtState.addNode nodeInfo dhtState
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd    = DhtState.addNode nodeInfo dhtState
          afterRemove = DhtState.removeNode (NodeInfo.publicKey nodeInfo) afterAdd
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          afterRemove `shouldBe` dhtState

    it "should make the state size increase by 1" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd = DhtState.addNode nodeInfo dhtState
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          DhtState.size afterAdd `shouldBe` DhtState.size dhtState

  describe "adding a node" $
    it "and adding it again does not change the state twice" $
      property $ \dhtState nodeInfo ->
        let
          afterAdd1 = DhtState.addNode nodeInfo dhtState
          afterAdd2 = DhtState.addNode nodeInfo afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

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

    it "and adding a node info for it will not add it to the search entry's k-buckets" $
      property $ \dhtState nodeInfo ->
        let
          afterAddSearchKey = DhtState.addSearchKey
            (NodeInfo.publicKey nodeInfo)
            dhtState
        in
        DhtState.size (DhtState.addNode nodeInfo afterAddSearchKey)
        `shouldBe`
        DhtState.size (DhtState.addNode nodeInfo dhtState)
