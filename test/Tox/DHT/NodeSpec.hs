{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tox.DHT.NodeSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Identity      (runIdentity)
import           Control.Monad.Logger        (runNoLoggingT)
import           Control.Monad.Random        (evalRandT)
import           Control.Monad.State         (runStateT)
import qualified Data.Map                    as Map

import           Tox.Core.Time
import qualified Tox.Crypto.Core.Keyed       as KeyedT
import qualified Tox.Crypto.Core.KeyPair     as KP
import qualified Tox.DHT.DhtPacket           as DhtPacket
import           Tox.DHT.DhtState            as DhtState
import           Tox.DHT.Node
import           Tox.DHT.Operation
import qualified Tox.DHT.PingPacket          as Ping
import           Tox.DHT.RpcPacket
import qualified Tox.Network.Core.Encoding   as Encoding
import qualified Tox.Network.Core.Networked  as Networked
import           Tox.Network.Core.NodeInfo
import           Tox.Network.Core.Packet
import           Tox.Network.Core.PacketKind as PacketKind
import qualified Tox.Network.Core.TimedT     as TimedT

spec :: Spec
spec = do
  describe "handleIncomingPacket" $ do
    it "ignores malformed packets" $ property $
      \(seed :: ArbStdGen) (time :: Timestamp) (from :: NodeInfo) ->
        let dhtState = initTestDhtState seed time
            packet = Packet PacketKind.PingRequest "malformed"
            (_, finalState) = runTestDhtNode seed time dhtState (handleIncomingPacket from packet)
        in finalState `shouldBe` dhtState

    it "dispatches valid PingRequest" $ property $
      \(seed :: ArbStdGen) (time :: Timestamp) (from :: NodeInfo) (rid :: RequestId) ->
        let dhtState = initTestDhtState seed time
            kp = dhtKeyPair dhtState
            -- Construct a valid DhtPacket containing a PingRequest RPC
            rpc = RpcPacket Ping.PingRequest rid
            -- Use dummy nonce
            nonce = read "\"000000000000000000000000000000000000000000000000\""
            dhtPkt = DhtPacket.encode kp (KP.publicKey kp) nonce rpc
            packet = Packet PacketKind.PingRequest (Encoding.encode dhtPkt)

            ((_, _), events) = runIdentity
              . Networked.runNetworkLogged
              . (`runStateT` dhtState)
              . runNoLoggingT
              . (`evalRandT` unwrapArbStdGen seed)
              . (`TimedT.runTimedT` time)
              . (`KeyedT.evalKeyedT` Map.empty)
              $ handleIncomingPacket from packet
        in any (\(Networked.SendPacket _ (Packet kind _)) -> kind == PacketKind.PingResponse) events
