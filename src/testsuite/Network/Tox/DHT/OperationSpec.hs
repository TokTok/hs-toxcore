{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.DHT.OperationSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad                 (mzero, when)
import           Control.Monad.Writer          (execWriterT)
import           Data.Proxy                    (Proxy (..))
import qualified Data.Map                             as Map

import           Network.Tox.Crypto.Key        (PublicKey)
import qualified Network.Tox.Crypto.KeyPair    as KeyPair
import           Network.Tox.DHT.DhtState      (DhtState)
import qualified Network.Tox.DHT.DhtState      as DhtState
import qualified Network.Tox.DHT.Operation     as Operation
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo
import qualified Network.Tox.Time              as Time

spec :: Spec
spec = do
  describe "a newly initialised DHT node" $ do
    it "contains no nodes" $
      property $ \time seed ->
        (DhtState.size $ Operation.initTestDhtState seed time) `shouldBe` 0

    it "has a search list containing initRandomSearches search entries" $
      property $ \time seed ->
        (Map.size . DhtState.dhtSearchList $ Operation.initTestDhtState seed time)
        `shouldBe` Operation.initRandomSearches

  describe "periodic nodes requests" $ do
    it "are not generated for an empty DHT State" $
      property $ \keyPair time time' seed ->
        let
          dhtState = DhtState.empty time keyPair
          requests = Operation.evalTestDhtNode seed time' dhtState . execWriterT $
            Operation.randomRequests >> Operation.checkNodes
        in
        requests `shouldBe` []

  describe "randomRequests" $ do
    it "generates a single Nodes Request to a node in the close list after randomRequestPeriod" $
      property $ \keyPair time (nodeInfos::[NodeInfo]) seed ->
        let
          dhtState       = DhtState.empty time keyPair
          afterAdd       = foldr (DhtState.addNode time) dhtState nodeInfos
          time'          = time Time.+ Operation.randomRequestPeriod
          randomRequests = Operation.evalTestDhtNode seed time' afterAdd
            . execWriterT $ Operation.randomRequests
        in
        case randomRequests of
          [] -> DhtState.size dhtState `shouldBe` 0
          Operation.RequestInfo nodeInfo publicKey : rs -> do
            rs `shouldSatisfy` null
            nodeInfo `shouldSatisfy` (`elem` nodeInfos)
            publicKey `shouldBe` KeyPair.publicKey (DhtState.dhtKeyPair dhtState)

    it "generates a Nodes Request to a node in a new search list after randomRequestPeriod" $
      property $ \time publicKey dhtState (nodeInfos::[NodeInfo]) seed ->
        let
          afterSearch       = DhtState.addSearchKey time publicKey dhtState
          afterAdd          = foldr (DhtState.addNode time) afterSearch nodeInfos
          nodeAddedToSearch = not $ all ((== publicKey) . NodeInfo.publicKey) nodeInfos
          time'             = time Time.+ Operation.randomRequestPeriod
          randomRequests    = Operation.evalTestDhtNode seed time' afterAdd
            . execWriterT $ Operation.randomRequests

          requestIsForSearch (Operation.RequestInfo nodeInfo publicKey') =
            publicKey == publicKey' && nodeInfo `elem` nodeInfos &&
              NodeInfo.publicKey nodeInfo /= publicKey
        in
        when nodeAddedToSearch $
          randomRequests `shouldSatisfy` not . all (not . requestIsForSearch)

  describe "checkNodes" $
    it "generates a Nodes Request to a newly added node after checkPeriod" $
      property $ \time dhtState nodeInfo seed ->
        let
          viable   = DhtState.viable nodeInfo dhtState
          afterAdd = DhtState.addNode time nodeInfo dhtState
          time'    = time Time.+ Operation.checkPeriod
          checks   = Operation.evalTestDhtNode seed time' afterAdd
            . execWriterT $ Operation.checkNodes
        in
        when viable $ map Operation.requestTo checks `shouldSatisfy` (nodeInfo `elem`)
