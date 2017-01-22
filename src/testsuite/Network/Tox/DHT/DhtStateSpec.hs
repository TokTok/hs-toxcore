{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtStateSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad                 (unless)
import           Data.Proxy                    (Proxy (..))

import qualified Network.Tox.Crypto.KeyPair    as KeyPair
import           Network.Tox.DHT.DhtState      (DhtState)
import qualified Network.Tox.DHT.DhtState      as DhtState
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

  describe "adding a node that was not yet contained" $ do
    it "should result in a different state" $
      property $ \keyPair nodeInfo ->
        let
          dhtState = DhtState.empty keyPair
          afterAdd = DhtState.addNode nodeInfo dhtState
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \keyPair nodeInfo ->
        let
          dhtState    = DhtState.empty keyPair
          afterAdd    = DhtState.addNode nodeInfo dhtState
          afterRemove = DhtState.removeNode (NodeInfo.publicKey nodeInfo) afterAdd
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          afterRemove `shouldBe` dhtState

  describe "adding a node" $
    it "and adding it again does not change the state twice" $
      property $ \keyPair nodeInfo ->
        let
          dhtState  = DhtState.empty keyPair
          afterAdd1 = DhtState.addNode nodeInfo dhtState
          afterAdd2 = DhtState.addNode nodeInfo afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

  describe "adding a search node" $ do
    it "should result in a different state" $
      property $ \keyPair publicKey ->
        let
          dhtState = DhtState.empty keyPair
          afterAdd = DhtState.addSearchKey publicKey dhtState
        in
        afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \keyPair publicKey ->
        let
          dhtState    = DhtState.empty keyPair
          afterAdd    = DhtState.addSearchKey publicKey dhtState
          afterRemove = DhtState.removeSearchKey publicKey afterAdd
        in
        afterRemove `shouldBe` dhtState

    it "and adding it again does not change the state twice" $
      property $ \keyPair publicKey ->
        let
          dhtState  = DhtState.empty keyPair
          afterAdd1 = DhtState.addSearchKey publicKey dhtState
          afterAdd2 = DhtState.addSearchKey publicKey afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

    it "and adding a node info for it will not add it to the search entry's Client List" $
      property $ \keyPair nodeInfo ->
        let
          dhtState = DhtState.empty keyPair
          afterAddSearchKey = DhtState.addSearchKey
            (NodeInfo.publicKey nodeInfo)
            dhtState
        in
        DhtState.size (DhtState.addNode nodeInfo afterAddSearchKey)
        `shouldBe`
        DhtState.size (DhtState.addNode nodeInfo dhtState)
