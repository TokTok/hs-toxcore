{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Tox.Network.TCP.ServerSpec (spec) where

import           Control.Monad.Identity             (Identity, runIdentity)
import           Control.Monad.Logger               (MonadLogger (..))
import           Control.Monad.Random               (RandT, evalRandT)
import           Control.Monad.State                (MonadState, StateT, get,
                                                     gets, modify, put,
                                                     runStateT)
import qualified Data.ByteString                    as BS
import           Data.List                          (partition)
import qualified Data.Map                           as Map
import           System.Random                      (StdGen, mkStdGen)
import           Test.Hspec
import           Test.QuickCheck

import           Tox.Core.Time                      (Timestamp)
import           Tox.Core.Timed                     (Timed)
import           Tox.Crypto.Core.Key                (Nonce)
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes)
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TimedT            (TimedT, runTimedT)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import           Tox.Network.TCP.Client
import           Tox.Network.TCP.Server

data World = World
  { wServer :: TcpServer
  , wSent   :: [(ClientId, BS.ByteString)]
  }

newtype TestServer a = TestServer (TimedT (StateT World (RandT StdGen Identity)) a)
  deriving (Functor, Applicative, Monad, Timed, MonadRandomBytes, MonadState World)

instance MonadLogger TestServer where
  monadLoggerLog _ _ _ _ = return ()

instance TcpServerMonad TestServer where
  getTcpServer = gets wServer
  putTcpServer s = modify $ \w -> w { wServer = s }
  serverSend client bytes = modify $ \w -> w { wSent = wSent w ++ [(client, bytes)] }
  serverClose _ = return ()

runTestServer :: Timestamp -> TcpServer -> TestServer a -> a
runTestServer now server (TestServer m) =
  fst . runIdentity . (`evalRandT` mkStdGen 42) . (`runStateT` World server []) $ runTimedT m now

-- | Give a client what the server sent it.
receive :: ClientId -> RelayConnection -> TestServer (RelayConnection, [RelayEvent])
receive client rc = do
  w <- get
  let (mine, others) = partition ((== client) . fst) (wSent w)
  put w { wSent = others }
  return $ case receiveBytes (BS.concat $ map snd mine) rc of
    Right (rc', events, _) -> (rc', events)
    Left ()                -> (rc, [])

send :: ClientId -> BS.ByteString -> RelayConnection -> TestServer RelayConnection
send client packet rc = case encodePacket packet rc of
  Just (frame, rc') -> rc' <$ clientBytes client frame
  Nothing           -> return rc

spec :: Spec
spec = describe "TCP server" $
  it "connects clients that ask for each other and tells one when the other leaves" $ property $
    \(serverKeys :: KeyPair) (a :: KeyPair) (b :: KeyPair) (tempA :: KeyPair) (tempB :: KeyPair)
     (n1 :: Nonce) (n2 :: Nonce) (n3 :: Nonce) (n4 :: Nonce) (now :: Timestamp) ->
      let relay = NodeInfo TCP (SocketAddress (read "\"127.0.0.1\"") 33445) (KeyPair.publicKey serverKeys)
          (rcA0, helloA) = newRelayConnection a tempA n1 n2 relay
          (rcB0, helloB) = newRelayConnection b tempB n3 n4 relay
          pkA = KeyPair.publicKey a
          pkB = KeyPair.publicKey b
      in runTestServer now (newTcpServer serverKeys) (do
        clientAccepted 0
        clientAccepted 1
        _ <- clientBytes 0 helloA
        _ <- clientBytes 1 helloB
        (rcA1, _) <- receive 0 rcA0
        (rcB1, _) <- receive 1 rcB0
        rcA2 <- send 0 (routingRequest pkB) rcA1
        rcB2 <- send 1 (routingRequest pkA) rcB1
        (rcA3, eventsA) <- receive 0 rcA2
        (rcB3, eventsB) <- receive 1 rcB2
        rcA4 <- maybe (return rcA3) (\packet -> send 0 packet rcA3) (routeData pkB "hi" rcA3)
        (rcB4, dataB) <- receive 1 rcB3
        let routeIds = [ i | (i, (pk, _)) <- Map.toList (rcRoutes rcA4), pk == pkB ]
        _ <- send 0 (BS.pack $ packetDisconnectNotification : routeIds) rcA4
        (_, leftB) <- receive 1 rcB4
        stillA <- Map.member 0 . tsClients <$> getTcpServer
        return (eventsA, eventsB, dataB, leftB, stillA))
      `shouldBe`
        ( [RelayRouteChanged pkB (Just RouteRegistered), RelayRouteChanged pkB (Just RouteOnline)]
        , [RelayRouteChanged pkA (Just RouteRegistered), RelayRouteChanged pkA (Just RouteOnline)]
        , [RelayData pkA "hi"]
        , [RelayRouteChanged pkA (Just RouteRegistered)]
        , True
        )
