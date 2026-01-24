{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Tox.Onion.OperationSpec (spec) where

import           Test.Hspec

import           Control.Monad.Logger             (MonadLogger)
import           Control.Monad.State              (MonadState, gets, modify)
import           Data.Binary                      (encode)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Map                         as Map
import           Data.Word                        (Word64)

import qualified Tox.Core.Time                    as Time
import           Tox.Core.Timed                   (Timed)
import qualified Tox.Crypto.Core.Box              as Box
import qualified Tox.Crypto.Core.CombinedKey      as CombinedKey
import           Tox.Crypto.Core.Key              (PublicKey)
import           Tox.Crypto.Core.Keyed            (Keyed)
import qualified Tox.Crypto.Core.KeyPair          as KeyPair
import           Tox.Crypto.Core.KeyPair          (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)
import qualified Tox.Crypto.Core.Nonce            as Nonce
import           Tox.DHT.DhtPacket                (DhtPacket (..))
import           Tox.DHT.DhtRequestPacket         (DhtRequestPacket (..))
import           Tox.Network.Core.HostAddress     (HostAddress (..))
import           Tox.Network.Core.Networked       (NetworkAction (..),
                                                   Networked)
import           Tox.Network.Core.NodeInfo        (NodeInfo (..))
import           Tox.Network.Core.SocketAddress   (SocketAddress (..))
import           Tox.Network.Core.TestStack       (TestStack, runTestStack)
import           Tox.Onion.Operation
import qualified Tox.Onion.RPC                    as RPC
import qualified Tox.Onion.Tunnel                 as Tunnel

data TestNode = TestNode
  { tnOnion   :: OnionState
  , tnDhtKeys :: KeyPair
  }

newtype TestOnionNodeMonad a = TestOnionNodeMonad
  { unTestOnionNodeMonad :: TestStack TestNode a
  } deriving (Functor, Applicative, Monad, MonadState TestNode, Timed, MonadRandomBytes, Keyed, Networked, MonadLogger)

instance OnionNodeMonad TestOnionNodeMonad where
  getOnionState = gets tnOnion
  putOnionState s = modify $ \t -> t { tnOnion = s }
  getDhtKeyPair = gets tnDhtKeys
  getCloseNodes _ _ = return []
  getCloseNodesFor _ _ _ = return []

-- | Run an action on the node, returning the packets it sent.
runNode :: TestNode -> TestOnionNodeMonad a -> (a, TestNode, [(SocketAddress, BS.ByteString)])
runNode node m =
  let (a, node', actions) = runTestStack 42 (Time.fromSeconds 1000) node (unTestOnionNodeMonad m)
  in (a, node', [ (address to, LBS.toStrict $ encode pkt) | SendPacket to pkt <- actions ])

newNode :: IO TestNode
newNode = do
  real <- KeyPair.newKeyPair
  dht <- KeyPair.newKeyPair
  onion <- initState real
  return $ TestNode onion dht

lastRelay :: SocketAddress
lastRelay = SocketAddress (IPv4 0x0100007f) 33445

-- | Send an announce request to the node as a client would, returning the
-- decrypted response and the node.
announceTo :: TestNode -> KeyPair -> PublicKey -> PublicKey -> PublicKey -> BS.ByteString
           -> IO (RPC.AnnounceResponsePayload, TestNode)
announceTo node sender pingId searchKey dataPk ret = do
  nonce <- Nonce.newNonce
  let shared = CombinedKey.precompute (KeyPair.secretKey sender) (KeyPair.publicKey $ tnDhtKeys node)
      payload = RPC.AnnounceRequestPayload pingId searchKey dataPk 7
      request = RPC.AnnounceRequest nonce (KeyPair.publicKey sender) (Box.encrypt shared nonce (Box.encode payload))
      ((), node', sent) = runNode node $ handleAnnounceRequest lastRelay (LBS.toStrict (encode request) <> ret)
  case sent of
    [(to, bytes)] | to == lastRelay, Just (0x8c, rest) <- BS.uncons bytes
                  , BS.take (Tunnel.returnSize 3) rest == ret
                  , Just (0x84, responseBytes) <- BS.uncons (BS.drop (Tunnel.returnSize 3) rest)
                  , Just response <- Box.decode (Box.PlainText responseBytes)
                  , Just plain <- Box.decrypt shared (RPC.announceResponseNonce response) (RPC.announceResponseEncryptedPayload response)
                  , Just result <- Box.decode plain -> do
      RPC.announceResponseSendbackData response `shouldBe` 7
      return (result, node')
    _ -> expectationFailure ("unexpected response: " ++ show sent) >> fail "unreachable"

spec :: Spec
spec = do
  describe "DHT public key over the DHT" $
    it "reaches us from a friend in a DHT request packet" $ do
      node <- newNode
      friendReal <- KeyPair.newKeyPair
      friendDht <- KeyPair.newKeyPair
      let ourReal = ourRealKeys (tnOnion node)
          withFriend = node { tnOnion = (tnOnion node) { onionFriends = Map.singleton (KeyPair.publicKey friendReal) friend } }
          friend = OnionFriend (tnDhtKeys node) Map.empty Nothing Nothing Nothing 0 False 0 Nothing
          announcement = BS.singleton 156 <> LBS.toStrict (encode (5 :: Word64)) <> LBS.toStrict (encode $ KeyPair.publicKey friendDht)
      innerNonce <- Nonce.newNonce
      outerNonce <- Nonce.newNonce
      let realShared = CombinedKey.precompute (KeyPair.secretKey friendReal) (KeyPair.publicKey ourReal)
          inner = LBS.toStrict (encode $ KeyPair.publicKey friendReal) <> LBS.toStrict (encode innerNonce)
            <> Box.unCipherText (Box.encrypt realShared innerNonce (Box.PlainText announcement))
          dhtShared = CombinedKey.precompute (KeyPair.secretKey friendDht) (KeyPair.publicKey $ tnDhtKeys node)
          request = DhtRequestPacket (KeyPair.publicKey $ tnDhtKeys node) $
            DhtPacket (KeyPair.publicKey friendDht) outerNonce (Box.encrypt dhtShared outerNonce (Box.PlainText $ BS.cons 156 inner))
          (events, _, _) = runNode withFriend $ handleDhtRequest (LBS.toStrict $ encode request)
      events `shouldBe` [FriendDhtPkFound (KeyPair.publicKey friendReal) (KeyPair.publicKey friendDht) []]

  describe "announce node" $
    it "stores an announcement made with its ping id and routes data to the announcer" $ do
      node0 <- newNode
      announcer <- KeyPair.newKeyPair
      searcher <- KeyPair.newKeyPair
      dataKeys <- KeyPair.newKeyPair
      let ret = BS.replicate (Tunnel.returnSize 3) 3
          announcerPk = KeyPair.publicKey announcer
          dataPk = KeyPair.publicKey dataKeys

      (first, node1) <- announceTo node0 announcer zeroKey announcerPk dataPk ret
      RPC.announceResponseIsStored first `shouldBe` 0

      (second, node2) <- announceTo node1 announcer (RPC.announceResponsePingId first) announcerPk dataPk ret
      RPC.announceResponseIsStored second `shouldBe` 2

      (search, node3) <- announceTo node2 searcher zeroKey announcerPk zeroKey (BS.replicate (Tunnel.returnSize 3) 4)
      RPC.announceResponseIsStored search `shouldBe` 1
      RPC.announceResponsePingId search `shouldBe` dataPk

      let forwarded = BS.replicate (24 + 32 + 20) 5
          dataRequest = LBS.toStrict (encode announcerPk) <> forwarded <> BS.replicate (Tunnel.returnSize 3) 6
          ((), _, sent) = runNode node3 $ handleDataRequest dataRequest
      sent `shouldBe` [(lastRelay, BS.singleton 0x8c <> ret <> BS.singleton 0x86 <> forwarded)]
