{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Tox.Onion.OperationSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Identity           (Identity, runIdentity)
import           Control.Monad.Random             (RandT, evalRandT)
import           Control.Monad.State              (MonadState, StateT, get,
                                                   gets, put, runStateT)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           System.Random                    (StdGen, mkStdGen)

import qualified Tox.Core.PingArray               as PingArray
import qualified Tox.Core.Time                    as Time
import           Tox.Core.Time                    (Timestamp)
import           Tox.Core.Timed                   (Timed (..))
import qualified Tox.Crypto.Core.Box              as Box
import           Tox.Crypto.Core.Key              (PublicKey)
import           Tox.Crypto.Core.Keyed            (Keyed (..), KeyedT,
                                                   evalKeyedT)
import qualified Tox.Crypto.Core.KeyPair          as KeyPair
import           Tox.Crypto.Core.KeyPair          (KeyPair (..))
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes (..),
                                                   randomNonce)
import qualified Tox.Network.Core.Networked       as Networked
import qualified Tox.Network.Core.NodeInfo        as NodeInfo
import           Tox.Network.Core.NodeInfo        (NodeInfo)
import           Tox.Network.Core.Packet          (Packet (..))
import qualified Tox.Network.Core.PacketKind      as PacketKind
import           Tox.Network.Core.TimedT          (TimedT, runTimedT)
import           Tox.Onion.Operation
import qualified Tox.Onion.Path                   as Path
import qualified Tox.Onion.RPC                    as RPC

newtype TestOnionNodeMonad a = TestOnionNodeMonad
  { unTestOnionNodeMonad :: KeyedT (TimedT (RandT StdGen (StateT OnionState (Networked.NetworkLogged Identity)))) a
  } deriving (Functor, Applicative, Monad, MonadState OnionState, Timed, MonadRandomBytes, Keyed, Networked.Networked)

instance OnionNodeMonad TestOnionNodeMonad where
  getOnionState = get
  putOnionState = put
  getDhtPublicKey = gets (KeyPair.publicKey . ourLongTermKeys)

runTestOnionNode :: Timestamp -> OnionState -> TestOnionNodeMonad a -> ((a, OnionState), [Networked.NetworkAction])
runTestOnionNode time s =
  runIdentity
    . Networked.runNetworkLogged
    . (`runStateT` s)
    . (`evalRandT` mkStdGen 42)
    . (`runTimedT` time)
    . (`evalKeyedT` Map.empty)
    . unTestOnionNodeMonad

spec :: Spec
spec = do
  describe "doOnion" $ do
    it "maintains paths and sends announcements" $ property $
      \(now :: Timestamp) (ourKeys :: KeyPair) (dhtNodes :: [NodeInfo]) ->
        length dhtNodes >= 3 ==>
          let s0 = initState ourKeys
              ((_, s1), events) = runTestOnionNode now s0 (doOnion dhtNodes)
              isOnionRequest (Networked.SendPacket _ (Packet kind _)) = kind == PacketKind.OnionRequest0
          in length (Path.announcePaths $ onionPaths s1) == Path.maxPaths &&
             any isOnionRequest events

  describe "handleAnnounceResponse" $ do
    it "updates announcedNodes state on successful decryption" $ property $
      \(now :: Timestamp) (ourKeys :: KeyPair) (from :: NodeInfo) (payload :: RPC.AnnounceResponsePayload) ->
        let s0 = initState ourKeys
            targetPk = NodeInfo.publicKey from
            searchKey = KeyPair.publicKey ourKeys

            -- Prepare state with a matching request in the tracker
            (sendback, s1) = runIdentity $ do
               let meta = OnionRequest searchKey targetPk
               let (sid, tracker') = PingArray.addEntry now meta 0 (requestTracker s0)
               return (sid, s0 { requestTracker = tracker' })

            -- Prepare a valid response mocked from the target node
            ((res, _), _) = runTestOnionNode now s1 $ do
              combined <- getCombinedKey (KeyPair.secretKey ourKeys) targetPk
              nonce <- randomNonce
              let enc = RPC.AnnounceResponse sendback nonce (Box.encrypt combined nonce (Box.encode payload))
              return enc

            ((_, s2), _) = runTestOnionNode now s1 (handleAnnounceResponse from res)
        in Map.member targetPk (announcedNodes s2) `shouldBe` True

  describe "sendDataRouteRequest" $ do
    it "sends an OnionRequest0 packet with wrapped data" $ property $
      \(now :: Timestamp) (ourKeys :: KeyPair) (relay :: NodeInfo) (destPk :: PublicKey) (payload :: Box.PlainText) (path :: Path.OnionPath) ->
        let s0 = initState ourKeys
            -- We need a valid path with exactly 3 nodes and 3 keys
            path' = path { Path.pathNodes = replicate 3 relay, Path.pathKeys = replicate 3 ourKeys }
            ((_, _), events) = runTestOnionNode now s0 (sendDataRouteRequest path' relay destPk payload)
            isOnionRequest (Networked.SendPacket to (Packet kind _)) =
              to == relay && kind == PacketKind.OnionRequest0
        in any isOnionRequest events
