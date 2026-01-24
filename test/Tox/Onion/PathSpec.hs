{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Tox.Onion.PathSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Identity             (Identity, runIdentity)
import           Control.Monad.Random               (RandT, evalRandT)
import           Control.Monad.State                (MonadState, StateT, get,
                                                     put, runStateT)
import           Control.Monad.Trans                (lift)
import           Data.Binary                        (encode)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           System.Random                      (StdGen, mkStdGen)

import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (Timestamp)
import           Tox.Core.Timed                     (Timed (..))
import           Tox.Crypto.Core.Key                (CombinedKey, Nonce)
import qualified Tox.Crypto.Core.Keyed              as Keyed
import           Tox.Crypto.Core.Keyed              (runNullKeyed)
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair (..))
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..))
import           Tox.Network.Core.HostAddress       (HostAddress (..))
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..),
                                                     RawPayload (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TimedT            (TimedT, runTimedT)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (UDP))
import           Tox.Onion.Path
import qualified Tox.Onion.Tunnel                   as Tunnel

newtype TestOnionMonad a = TestOnionMonad (TimedT (RandT StdGen (StateT OnionPathState Identity)) a)
  deriving (Functor, Applicative, Monad, Timed, MonadRandomBytes)

instance MonadState OnionPathState TestOnionMonad where
  get = TestOnionMonad $ lift $ lift get
  put = TestOnionMonad . lift . lift . put

instance Keyed.Keyed TestOnionMonad where
  getCombinedKey sk pk = return $ Keyed.runNullKeyed $ Keyed.getCombinedKey sk pk

instance OnionPathMonad TestOnionMonad

runTestOnion :: Timestamp -> OnionPathState -> TestOnionMonad a -> (a, OnionPathState)
runTestOnion time s (TestOnionMonad m) =
  runIdentity
    . (`runStateT` s)
    . (`evalRandT` mkStdGen 42)
    . (`runTimedT` time)
    $ m

spec :: Spec
spec = do
  describe "isPathAlive" $ do
    it "returns True for a new path" $ property $
      \(now :: Timestamp) (path :: OnionPath) ->
        let path' = path { pathExpires = now `Time.addTime` Time.seconds 1
                         , pathLastAttempt = Nothing
                         }
        in isPathAlive now path' `shouldBe` True

    it "returns False for an expired path" $ property $
      \(now :: Timestamp) (path :: OnionPath) ->
        let path' = path { pathExpires = now `Time.addTime` Time.seconds (-1) }
        in isPathAlive now path' `shouldBe` False

    it "returns False for a timed-out unconfirmed path" $ property $
      \(now :: Timestamp) (path :: OnionPath) ->
        let path' = path { pathExpires = now `Time.addTime` Time.seconds 100
                         , pathConfirmed = False
                         , pathLastAttempt = Just $ now `Time.addTime` Time.seconds (-5)
                         , pathTries = 2
                         }
      in isPathAlive now path' `shouldBe` False

  describe "relaying" $
    it "delivers a request through three relays and the response back to the sender" $ property $
      \(kpA :: KeyPair) (kpB :: KeyPair) (kpC :: KeyPair) (kp1 :: KeyPair) (kp2 :: KeyPair) (kp3 :: KeyPair)
        (retA :: CombinedKey) (retB :: CombinedKey) (retC :: CombinedKey) (nonce :: Nonce) (now :: Timestamp) ->
          let addr i = SocketAddress (IPv4 i) 33445
              node kp i = NodeInfo UDP (addr i) (KeyPair.publicKey kp)
              path = OnionPath [node kpA 2, node kpB 3, node kpC 4] [kp1, kp2, kp3] True 0 (now `Time.addTime` pathLifetime) Nothing 0
              (req0, _) = runTestOnion now (OnionPathState [] [] 0) (wrapPath path (addr 5) nonce (BS.pack [0x83, 1, 2, 3]))
              payload (Tunnel.Relayed _ pkt) = unRawPayload (packetPayload pkt)
              relay kp ret hop from bytes = runNullKeyed $ Tunnel.relayRequest kp ret nonce hop from bytes
              result = do
                toB <- relay kpA retA 1 (addr 1) (LBS.toStrict $ encode req0)
                toC <- relay kpB retB 2 (addr 2) (payload toB)
                toD@(Tunnel.Relayed dest (Packet kind _)) <- relay kpC retC 3 (addr 3) (payload toC)
                let (request, ret3) = BS.splitAt 3 (payload toD)
                backToB <- Tunnel.relayResponse retC 3 (ret3 <> BS.pack [0x84, 9, 9])
                backToA <- Tunnel.relayResponse retB 2 (payload backToB)
                atSender <- Tunnel.relayResponse retA 1 (payload backToA)
                return (dest, kind, request, atSender)
          in result `shouldBe` Just (Tunnel.ToUdp (addr 5), PacketKind.AnnounceRequest, BS.pack [1, 2, 3],
                                     Tunnel.Relayed (Tunnel.ToUdp (addr 1)) (Packet PacketKind.AnnounceResponse (RawPayload $ BS.pack [9, 9])))

  describe "maintainPaths" $ do
    it "always fills paths to maxPaths if enough nodes exist" $ property $
      \(now :: Timestamp) (nodes :: [NodeInfo]) ->
        length nodes >= 3 ==>
          let initState = OnionPathState [] [] 0
              (_, finalState) = runTestOnion now initState (maintainPaths nodes)
          in length (announcePaths finalState) == maxPaths &&
             length (searchPaths finalState) == maxPaths

  describe "pickNodes" $ do
    it "always returns exactly 3 nodes if available" $ property $
      \(nodes :: [NodeInfo]) ->
        length nodes >= 3 ==>
          let (pNodes, _) = runTestOnion (Time.fromMillis 0) (OnionPathState [] [] 0) (pickNodes nodes)
          in length pNodes == 3
