{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tox.Transport.SecureSessionSpec where

import           Control.Monad.Logger               (MonadLogger)
import           Control.Monad.State                (MonadState)
import qualified Data.ByteString                    as BS
import           Test.Hspec

import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (Timestamp)
import           Tox.Core.Timed                     (Timed)
import qualified Tox.Crypto.Core.CombinedKey        as CombinedKey
import           Tox.Crypto.Core.Key                (CombinedKey)
import           Tox.Crypto.Core.Keyed              (Keyed)
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes)
import           Tox.Network.Core.HostAddress       (HostAddress (..))
import           Tox.Network.Core.Networked         (NetworkAction (..),
                                                     Networked)
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..),
                                                     RawPayload (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TestStack         (TestStack, runTestStack)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import           Tox.Transport.SecureSession

newtype SessionM a = SessionM
  { unSessionM :: TestStack SecureSessionState a }
  deriving (Functor, Applicative, Monad, MonadState SecureSessionState, Timed, MonadRandomBytes, Keyed, Networked, MonadLogger)

-- | Run a session action, returning the packets it sent.
runSession :: Int -> Timestamp -> SecureSessionState -> SessionM a -> (a, SecureSessionState, [Packet BS.ByteString])
runSession seed now ss m =
  let (a, ss', actions) = runTestStack seed now ss (unSessionM m)
  in (a, ss', [fmap unRawPayload pkt | SendPacket _ pkt <- actions])

data Peer = Peer
  { peerReal      :: KeyPair
  , peerDht       :: KeyPair
  , peerCookieKey :: CombinedKey
  , peerNode      :: NodeInfo
  }

newPeer :: Int -> IO Peer
newPeer port = do
  real <- KeyPair.newKeyPair
  dht <- KeyPair.newKeyPair
  cookie <- KeyPair.newKeyPair
  return Peer
    { peerReal      = real
    , peerDht       = dht
    , peerCookieKey = CombinedKey.precompute (KeyPair.secretKey cookie) (KeyPair.publicKey cookie)
    , peerNode      = NodeInfo UDP (SocketAddress (IPv4 0x7f000001) (fromIntegral port)) (KeyPair.publicKey dht)
    }

-- | The single packet an action sent.
onlyPacket :: [Packet BS.ByteString] -> IO (Packet BS.ByteString)
onlyPacket [pkt] = return pkt
onlyPacket pkts  = expectationFailure ("expected one packet, got " ++ show pkts) >> fail "unreachable"

spec :: Spec
spec = describe "SecureSession" $ do
  it "establishes a session with a peer and delivers lossless data both ways" $ do
    a <- newPeer 1
    b <- newPeer 2
    let t = Time.fromSeconds 100
        noSession = error "stateless" :: SecureSessionState

    -- A requests a cookie and B answers without keeping state.
    let (s0, _, _) = runSession 1 t noSession $
          initSession (peerReal a) (KeyPair.publicKey $ peerReal b) (peerDht a) (KeyPair.publicKey $ peerDht b) (peerNode b)
        (_, s1, sent1) = runSession 2 t s0 maintainSession
    cookieRequest <- onlyPacket sent1
    packetKind cookieRequest `shouldBe` PacketKind.CookieRequest
    let (_, _, sent2) = runSession 3 t noSession $
          handleCookieRequest (peerCookieKey b) (peerDht b) (peerNode a) (packetPayload cookieRequest)
    cookieResponse <- onlyPacket sent2

    -- A sends its handshake.  B accepts it and answers with its own.
    let (_, s2, sent3) = runSession 4 t s1 $ handlePacket (peerCookieKey a) (peerNode b) cookieResponse
    handshakeA <- onlyPacket sent3
    packetKind handshakeA `shouldBe` PacketKind.CryptoHandshake
    let (opened, _, _) = runSession 5 t noSession $
          openHandshake (peerCookieKey b) (KeyPair.secretKey $ peerReal b) (packetPayload handshakeA)
    Just oh <- return opened
    ohPeerRealPk oh `shouldBe` KeyPair.publicKey (peerReal a)
    let (b0, _, sent4) = runSession 6 t noSession $
          acceptSession (peerCookieKey b) (peerReal b) (peerDht b) (peerNode a) oh
    handshakeB <- onlyPacket sent4

    -- A takes B's handshake, then its request packet confirms the session at B.
    let (_, s3, _) = runSession 7 t s2 $ handlePacket (peerCookieKey a) (peerNode b) handshakeB
        (_, s4, sent5) = runSession 8 t s3 maintainSession
    ssStatus s3 `shouldBe` NotConfirmed
    requestA <- onlyPacket [pkt | pkt <- sent5, packetKind pkt == PacketKind.CryptoData]
    let (events1, b1, _) = runSession 9 t b0 $ handlePacket (peerCookieKey b) (peerNode a) requestA
    events1 `shouldBe` [SessionEstablished]

    -- B's data confirms the session at A and arrives intact.
    let (sent, b2, sent6) = runSession 10 t b1 $ sendLossless "\x40hello"
    dataB <- onlyPacket sent6
    let (events2, s5, _) = runSession 11 t s4 $ handlePacket (peerCookieKey a) (peerNode b) dataB
    events2 `shouldBe` [SessionEstablished, SessionLosslessData "\x40hello"]
    ssStatus s5 `shouldBe` Established

    -- A's reply tells B its data arrived.
    num <- maybe (fail "not sent") return sent
    isDelivered num b2 `shouldBe` False
    let (_, _, sent7) = runSession 12 t s5 $ sendLossless "\x40hi"
    dataA <- onlyPacket sent7
    let (_, b3, _) = runSession 13 t b2 $ handlePacket (peerCookieKey b) (peerNode a) dataA
    isDelivered num b3 `shouldBe` True

  it "takes the peer's handshake that came another way, keeping its session key" $ do
    a <- newPeer 1
    b <- newPeer 2
    let t = Time.fromSeconds 100
        noSession = error "stateless" :: SecureSessionState
        relay = NodeInfo TCP (SocketAddress (IPv4 0x7f000003) 33445) (KeyPair.publicKey $ peerDht b)
        (s0, _, _) = runSession 1 t noSession $
          initSession (peerReal a) (KeyPair.publicKey $ peerReal b) (peerDht a) (KeyPair.publicKey $ peerDht b) (peerNode b)
        (_, s1, sent1) = runSession 2 t s0 maintainSession
    cookieRequest <- onlyPacket sent1
    let (_, _, sent2) = runSession 3 t noSession $
          handleCookieRequest (peerCookieKey b) (peerDht b) (peerNode a) (packetPayload cookieRequest)
    cookieResponse <- onlyPacket sent2
    let (_, s2, sent3) = runSession 4 t s1 $ handlePacket (peerCookieKey a) (peerNode b) cookieResponse
    handshakeA <- onlyPacket sent3
    awaitsHandshake s2 `shouldBe` True

    -- B answers A's handshake, and A gets B's answer through a relay.
    let (openedA, _, _) = runSession 5 t noSession $
          openHandshake (peerCookieKey b) (KeyPair.secretKey $ peerReal b) (packetPayload handshakeA)
    Just ohA <- return openedA
    let (b0, _, sent4) = runSession 6 t noSession $
          acceptSession (peerCookieKey b) (peerReal b) (peerDht b) (peerNode a) ohA
    handshakeB <- onlyPacket sent4
    let (openedB, _, _) = runSession 7 t noSession $
          openHandshake (peerCookieKey a) (KeyPair.secretKey $ peerReal a) (packetPayload handshakeB)
    Just ohB <- return openedB
    let (_, s3, _) = runSession 8 t s2 $ adoptHandshake (peerCookieKey a) relay ohB
    (ssStatus s3, ssPeerNodeInfo s3) `shouldBe` (NotConfirmed, relay)

    -- A's keys are the ones B has: its request packet confirms the session at B.
    let (_, _, sent5) = runSession 9 t s3 maintainSession
    requestA <- onlyPacket [pkt | pkt <- sent5, packetKind pkt == PacketKind.CryptoData]
    let (events, _, _) = runSession 10 t b0 $ handlePacket (peerCookieKey b) (peerNode a) requestA
    events `shouldBe` [SessionEstablished]

  it "rejects a handshake whose cookie expired" $ do
    a <- newPeer 1
    b <- newPeer 2
    let noSession = error "stateless" :: SecureSessionState
        (s0, _, _) = runSession 1 (Time.fromSeconds 0) noSession $
          initSession (peerReal a) (KeyPair.publicKey $ peerReal b) (peerDht a) (KeyPair.publicKey $ peerDht b) (peerNode b)
        (cookie, _, _) = runSession 2 (Time.fromSeconds 0) noSession $
          createCookie (peerCookieKey b) 0 (KeyPair.publicKey $ peerReal a) (KeyPair.publicKey $ peerDht a)
        (handshake, _, _) = runSession 3 (Time.fromSeconds 0) s0 $ createHandshake (peerCookieKey a) cookie
        open now = let (opened, _, _) = runSession 4 (Time.fromSeconds now) noSession $
                         openHandshake (peerCookieKey b) (KeyPair.secretKey $ peerReal b) (unRawPayload $ packetPayload handshake)
                   in opened
    fmap ohPeerRealPk (open 15) `shouldBe` Just (KeyPair.publicKey $ peerReal a)
    open 16 `shouldBe` Nothing
