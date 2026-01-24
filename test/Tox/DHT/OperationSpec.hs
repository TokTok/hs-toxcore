{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE Trustworthy         #-}
module Tox.DHT.OperationSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad               (when)
import           Control.Monad.Identity      (runIdentity)
import           Control.Monad.Logger        (runNoLoggingT)
import           Control.Monad.Random        (evalRandT)
import           Control.Monad.State         (runStateT)
import           Control.Monad.Writer        (execWriterT)
import qualified Data.Map                    as Map

import qualified Tox.Core.Time               as Time
import qualified Tox.Crypto.Core.Keyed       as KeyedT
import qualified Tox.Crypto.Core.KeyPair     as KeyPair
import qualified Tox.DHT.DhtState            as DhtState
import           Tox.DHT.NodesResponse       (NodesResponse (..))
import qualified Tox.DHT.Operation           as Operation
import           Tox.DHT.PingPacket          (PingPacket (..))
import           Tox.DHT.RpcPacket           (RequestId (..), RpcPacket (..))
import qualified Tox.DHT.Stamped             as Stamped
import qualified Tox.Network.Core.Networked  as Networked
import qualified Tox.Network.Core.NodeInfo   as NodeInfo
import           Tox.Network.Core.NodeInfo   (NodeInfo)
import           Tox.Network.Core.Packet     (Packet (..))
import qualified Tox.Network.Core.PacketKind as PacketKind
import qualified Tox.Network.Core.TimedT     as TimedT

spec :: Spec
spec = do
  describe "a newly initialised DHT node" $ do
    it "contains no nodes" $
      property $ \time seed ->
        DhtState.size (Operation.initTestDhtState seed time) `shouldBe` 0

    it "has a search list containing initRandomSearches search entries" $
      property $ \time seed ->
        (Map.size . DhtState.dhtSearchList $ Operation.initTestDhtState seed time)
        `shouldBe` Operation.initRandomSearches

  describe "periodic nodes requests" $
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
          time'          = time `Time.addTime` Operation.randomRequestPeriod
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
          time'             = time `Time.addTime` Operation.randomRequestPeriod
          randomRequests    = Operation.evalTestDhtNode seed time' afterAdd
            . execWriterT $ Operation.randomRequests

          requestIsForSearch (Operation.RequestInfo nodeInfo publicKey') =
            publicKey == publicKey' && nodeInfo `elem` nodeInfos &&
              NodeInfo.publicKey nodeInfo /= publicKey
        in
        when nodeAddedToSearch $
          randomRequests `shouldSatisfy` any requestIsForSearch

  describe "checkNodes" $
    it "generates a Nodes Request to a newly added node after checkPeriod" $
      property $ \time dhtState nodeInfo seed ->
        let
          viable   = DhtState.viable nodeInfo dhtState
          afterAdd = DhtState.addNode time nodeInfo dhtState
          time'    = time `Time.addTime` Operation.checkPeriod
          checks   = Operation.evalTestDhtNode seed time' afterAdd
            . execWriterT $ Operation.checkNodes
        in
        when viable $ map Operation.requestTo checks `shouldSatisfy` (nodeInfo `elem`)

  describe "handleNodesResponse" $ do
    it "adds the sender to the DHT state" $ property $
      \(seed :: Operation.ArbStdGen) (time :: Time.Timestamp) (from :: NodeInfo) (rid :: RequestId) (nodes :: [NodeInfo]) ->
        let dhtState = Operation.initTestDhtState seed time
            -- Pre-add the reply expectation so checkPending succeeds
            expecting = DhtState.empty time (DhtState.dhtKeyPair dhtState)
            expectingWithReply = expecting { DhtState.dhtPendingReplies = Stamped.add time (from, rid) Map.empty }
            response = RpcPacket (NodesResponse nodes) rid
            finalState = Operation.execTestDhtNode seed time expectingWithReply (Operation.handleNodesResponse from response)
        in DhtState.containsNode (NodeInfo.publicKey from) finalState `shouldBe` True

  describe "handlePingRequest" $ do
    it "sends a ping response" $ property $
      \(seed :: Operation.ArbStdGen) (time :: Time.Timestamp) (from :: NodeInfo) (rid :: RequestId) ->
        let dhtState = Operation.initTestDhtState seed time
            ((_, _), events) = runIdentity
              . Networked.runNetworkLogged
              . (`runStateT` dhtState)
              . runNoLoggingT
              . (`evalRandT` Operation.unwrapArbStdGen seed)
              . (`TimedT.runTimedT` time)
              . (`KeyedT.evalKeyedT` Map.empty)
              $ Operation.handlePingRequest from (RpcPacket PingRequest rid)
            isPingResponse (Networked.SendPacket to (Packet kind _)) =
                to == from && kind == PacketKind.PingResponse
        in events `shouldSatisfy` any isPingResponse
