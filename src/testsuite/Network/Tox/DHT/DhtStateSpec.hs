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
    property $ \keyPair time nodeInfo ->
      let
        dhtState = DhtState.empty time keyPair
        afterAdd = DhtState.addNode time
          nodeInfo { NodeInfo.publicKey = KeyPair.publicKey keyPair }
          dhtState
      in
      afterAdd `shouldBe` dhtState

  describe "adding a node that was not yet contained" $ do
    it "should result in a different state" $
      property $ \keyPair time nodeInfo ->
        let
          dhtState = DhtState.empty time keyPair
          afterAdd = DhtState.addNode time nodeInfo dhtState
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \keyPair time nodeInfo ->
        let
          dhtState    = DhtState.empty time keyPair
          afterAdd    = DhtState.addNode time nodeInfo dhtState
          afterRemove = DhtState.removeNode (NodeInfo.publicKey nodeInfo) afterAdd
        in
        unless (DhtState.containsNode (NodeInfo.publicKey nodeInfo) dhtState) $
          afterRemove `shouldBe` dhtState

  describe "adding a node" $
    it "and adding it again does not change the state twice" $
      property $ \keyPair time nodeInfo ->
        let
          dhtState  = DhtState.empty time keyPair
          afterAdd1 = DhtState.addNode time nodeInfo dhtState
          afterAdd2 = DhtState.addNode time nodeInfo afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

  describe "adding a search node" $ do
    it "should result in a different state" $
      property $ \keyPair time publicKey ->
        let
          dhtState = DhtState.empty time keyPair
          afterAdd = DhtState.addSearchKey time publicKey dhtState
        in
        afterAdd `shouldNotBe` dhtState

    it "and removing it yields the same state" $
      property $ \keyPair time publicKey ->
        let
          dhtState    = DhtState.empty time keyPair
          afterAdd    = DhtState.addSearchKey time publicKey dhtState
          afterRemove = DhtState.removeSearchKey publicKey afterAdd
        in
        afterRemove `shouldBe` dhtState

    it "and adding it again does not change the state twice" $
      property $ \keyPair time publicKey ->
        let
          dhtState  = DhtState.empty time keyPair
          afterAdd1 = DhtState.addSearchKey time publicKey dhtState
          afterAdd2 = DhtState.addSearchKey time publicKey afterAdd1
        in
        afterAdd1 `shouldBe` afterAdd2

    it "and adding a node info for it will not add it to the search entry's Client List" $
      property $ \keyPair time nodeInfo ->
        let
          dhtState = DhtState.empty time keyPair
          afterAddSearchKey = DhtState.addSearchKey time
            (NodeInfo.publicKey nodeInfo)
            dhtState
        in
        DhtState.size (DhtState.addNode time nodeInfo afterAddSearchKey)
        `shouldBe`
        DhtState.size (DhtState.addNode time nodeInfo dhtState)
