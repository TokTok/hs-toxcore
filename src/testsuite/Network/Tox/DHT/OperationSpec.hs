{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.DHT.OperationSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad                 (mzero, when)
import           Data.Proxy                    (Proxy (..))

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
  readShowSpec (Proxy :: Proxy DhtState)

  it "generates no periodic Nodes Request for an empty DhtState" $
    property $ \keyPair time time' seed ->
      let
        dhtState = DhtState.empty time keyPair
        requests = Operation.execTestOperation seed $
          Operation.randomRequests time' dhtState >>=
          Operation.pingNodes time'
      in
      requests `shouldBe` []

  describe "randomRequests" $ do
    it "generates a Nodes Request to a node in the close list after randomRequestPeriod" $
      property $ \keyPair time (nodeInfos::[NodeInfo]) seed ->
        let
          dhtState       = DhtState.empty time keyPair
          afterAdd       = foldr (DhtState.addNode time) dhtState nodeInfos
          time'          = time Time.+ Operation.randomRequestPeriod
          randomRequests = Operation.execTestOperation seed $ Operation.randomRequests time' afterAdd
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
          randomRequests    = Operation.execTestOperation seed $ Operation.randomRequests time' afterAdd

          requestIsForSearch (Operation.RequestInfo nodeInfo publicKey') =
            publicKey == publicKey' && nodeInfo `elem` nodeInfos &&
              NodeInfo.publicKey nodeInfo /= publicKey
        in
        when nodeAddedToSearch $
          randomRequests `shouldSatisfy` not . all (not . requestIsForSearch)

  describe "pingNodes" $
    it "generates a Nodes Request to a newly added node after pingPeriod" $
      property $ \time dhtState nodeInfo seed ->
        let
          viable   = DhtState.viable nodeInfo dhtState
          afterAdd = DhtState.addNode time nodeInfo dhtState
          time'    = time Time.+ Operation.pingPeriod
          pings    = Operation.execTestOperation seed $ Operation.pingNodes time' afterAdd
        in
        when viable $ map Operation.requestTo pings `shouldSatisfy` (nodeInfo `elem`)

  it "removes nodes from which we consistently fail to receive Node Responses"
    pending -- need something more precise to test
