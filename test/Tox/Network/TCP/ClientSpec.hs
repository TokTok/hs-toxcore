{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Network.TCP.ClientSpec (spec) where

import qualified Crypto.Saltine.Class               as Sodium
import           Data.Binary.Put                    (putWord16be, runPut)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Test.Hspec
import           Test.QuickCheck

import qualified Tox.Crypto.Core.Box                as Box
import           Tox.Crypto.Core.Box                (PlainText (..))
import qualified Tox.Crypto.Core.CombinedKey        as CombinedKey
import           Tox.Crypto.Core.Key                (Nonce, unKey)
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair (..))
import qualified Tox.Crypto.Core.Nonce              as Nonce
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import           Tox.Network.TCP.Client

-- | What a relay sends after accepting our handshake, sealed as a relay does.
relayBytes :: KeyPair -> KeyPair -> KeyPair -> KeyPair -> Nonce -> Nonce -> [BS.ByteString] -> BS.ByteString
relayBytes ourDht ourTemp relay relayTemp handshakeNonce sentNonce packets =
  Sodium.encode (unKey handshakeNonce) <> sealed handshakeKey handshakeNonce hello <> frames sentNonce packets
  where
    handshakeKey = CombinedKey.precompute (KeyPair.secretKey relay) (KeyPair.publicKey ourDht)
    sessionKey = CombinedKey.precompute (KeyPair.secretKey relayTemp) (KeyPair.publicKey ourTemp)
    hello = Sodium.encode (unKey $ KeyPair.publicKey relayTemp) <> Sodium.encode (unKey sentNonce)
    sealed key nonce = Box.unCipherText . Box.encrypt key nonce . PlainText
    frames _ [] = BS.empty
    frames nonce (p : ps) =
      let c = sealed sessionKey nonce p
      in LBS.toStrict (runPut $ putWord16be $ fromIntegral $ BS.length c) <> c <> frames (Nonce.increment nonce) ps

-- | Split bytes at the given offsets.
chunks :: [Int] -> BS.ByteString -> [BS.ByteString]
chunks _ bs | BS.null bs = []
chunks (n : ns) bs = let (a, b) = BS.splitAt (max 1 n) bs in a : chunks ns b
chunks [] bs = [bs]

spec :: Spec
spec = describe "relay connection" $ do
  it "decodes the relay's packets however the stream is split" $ property $
    \(ourDht :: KeyPair) (ourTemp :: KeyPair) (relayKeys :: KeyPair) (relayTemp :: KeyPair) (peer :: KeyPair)
     (n1 :: Nonce) (n2 :: Nonce) (n3 :: Nonce) (n4 :: Nonce) (Positive size :: Positive Int) (sizes :: [Positive Int]) ->
      let relay = NodeInfo UDP (SocketAddress (read "\"127.0.0.1\"") 33445) (KeyPair.publicKey relayKeys)
          (rc, _) = newRelayConnection ourDht ourTemp n1 n2 relay
          peerPk = Sodium.encode (unKey $ KeyPair.publicKey peer)
          packets = [BS.pack [packetRoutingResponse, 16] <> peerPk, BS.pack [packetConnectNotification, 16], "\x10\x1a hello"]
          bytes = relayBytes ourDht ourTemp relayKeys relayTemp n3 n4 packets
          feed (Right (c, events, _)) chunk = case receiveBytes chunk c of
            Right (c', events', replies) -> Right (c', events ++ events', replies)
            Left () -> Left ()
          feed (Left ()) _ = Left ()
          result = foldl feed (Right (rc, [], [])) (chunks (map getPositive sizes ++ repeat size) bytes)
      in fmap (\(_, events, _) -> events) result `shouldBe` Right
           [ RelayRouteChanged (KeyPair.publicKey peer) (Just RouteRegistered)
           , RelayRouteChanged (KeyPair.publicKey peer) (Just RouteOnline)
           , RelayData (KeyPair.publicKey peer) "\x1a hello"
           ]

  it "sends no more out of band than relays accept" $ property $ \(peer :: KeyPair) ->
    let send n = BS.length <$> oobSend (KeyPair.publicKey peer) (BS.replicate n 0)
    in (send maxOobDataLength, send (maxOobDataLength + 1)) `shouldBe` (Just (33 + maxOobDataLength), Nothing)
