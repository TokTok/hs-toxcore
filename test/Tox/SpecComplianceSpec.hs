{-# LANGUAGE OverloadedStrings #-}
module Tox.SpecComplianceSpec (spec) where

import           Data.Binary                        (encode)
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Put                    as Put
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Word                          (Word8)
import           Test.Hspec

import           Control.Monad.Identity             (runIdentity)
import           Control.Monad.Random               (evalRandT)
import           Control.Monad.Validate             (runValidate)
import qualified Crypto.Saltine.Class               as Sodium
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust)
import           System.Random                      (mkStdGen)
import qualified Tox.Core.Time                      as Time
import qualified Tox.Crypto.Core.Box                as Box
import           Tox.Crypto.Core.Key                (Key (..))
import qualified Tox.Crypto.Core.Keyed              as KeyedT
import           Tox.Crypto.Core.KeyPair            (KeyPair (..))
import qualified Tox.DHT.DhtPacket                  as DhtPacket
import           Tox.Network.Core.HostAddress       (HostAddress (..))
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import           Tox.Network.Core.PortNumber        (PortNumber (..))
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Onion.Path                     as Path
import qualified Tox.Onion.RPC                      as RPC
import qualified Tox.Onion.Tunnel                   as Tunnel
import qualified Tox.Transport.SecureSession        as SecureSession

spec :: Spec
spec = do
  describe "Tox Binary Spec Compliance" $ do

    let nullPk = Key . fromJust . Sodium.decode $ BS.replicate 32 0
        nullNonce = Key . fromJust . Sodium.decode $ BS.replicate 24 0
        nullCk = Key . fromJust . Sodium.decode $ BS.replicate 32 0
        nullTime = Time.fromMillis 0

    it "NodeInfo (IPv4) must be exactly 39 bytes" $ do
      let addr = SocketAddress (IPv4 0) (PortNumber 33445)
          ni = NodeInfo UDP addr nullPk
      LBS.length (encode ni) `shouldBe` 39

    it "AnnounceRequestPayload must be exactly 104 bytes" $ do
      let payload = RPC.AnnounceRequestPayload nullPk nullPk nullPk 0
      LBS.length (encode payload) `shouldBe` 104

    it "PingRequest (DHT) must be exactly 82 bytes (RPC wrapped)" $ do
      let innerPkt = DhtPacket.DhtPacket nullPk nullNonce (either (error . show) id $ runValidate $ Box.cipherText $ BS.replicate 25 0) -- 1 byte payload + 8 byte ReqId + 16 MAC
          pkt = Packet PacketKind.PingRequest innerPkt
          bs = LBS.toStrict $ Put.runPut $ do
            Binary.put (packetKind pkt)
            Binary.put (packetPayload pkt)
      BS.length bs `shouldBe` 82

    it "NodesRequest (DHT) must be exactly 113 bytes (RPC wrapped)" $ do
      let innerPkt = DhtPacket.DhtPacket nullPk nullNonce (either (error . show) id $ runValidate $ Box.cipherText $ BS.replicate 56 0) -- 32 byte payload + 8 byte ReqId + 16 MAC
          pkt = Packet PacketKind.NodesRequest innerPkt
          bs = LBS.toStrict $ Put.runPut $ do
            Binary.put (packetKind pkt)
            Binary.put (packetPayload pkt)
      BS.length bs `shouldBe` 113

    it "CookieRequest must be exactly 145 bytes (including Kind)" $ do
      let cri = SecureSession.CookieRequestInner nullPk (BS.replicate 32 0) 0
          encrypted = Box.encrypt nullCk nullNonce (Box.encode cri)
          cr = SecureSession.CookieRequest nullPk nullNonce encrypted
          pkt = Packet PacketKind.CookieRequest cr
      -- Kind(1) + PK(32) + Nonce(24) + Encrypted(88) = 145
      LBS.length (encode (pkt :: Packet SecureSession.CookieRequest)) `shouldBe` 145

    it "CipherText must be exactly its data length (prefix-less)" $ do
      let mCt = runValidate (Box.cipherText $ BS.replicate 100 0)
      case mCt of
        Left _   -> expectationFailure "Failed to create ciphertext"
        Right ct -> LBS.length (encode ct) `shouldBe` 100

    it "OnionRequestPayload must be exactly 156 bytes" $ do
      let innerPkt = DhtPacket.DhtPacket nullPk nullNonce (either (error . show) id $ runValidate $ Box.cipherText $ BS.replicate 48 0)
          p3 = Tunnel.OnionRequestPayload (Tunnel.OnionIPPort (SocketAddress (IPv4 0) (PortNumber 0))) nullPk (either (error . show) id $ runValidate $ Box.cipherText $ BS.singleton 0x83 <> LBS.toStrict (encode innerPkt))
      LBS.length (Binary.encode p3) `shouldBe` 156

    it "OnionRequest0 produced by wrapPath must be exactly 403 bytes" $ do
      let nullSk = Key . fromJust . Sodium.decode $ BS.replicate 32 0
          ourKp = KeyPair nullSk nullPk
          path = Path.OnionPath (replicate 3 (NodeInfo UDP (SocketAddress (IPv4 0) (PortNumber 0)) nullPk)) (replicate 3 ourKp) False 0 nullTime Nothing 0
          destAddr = SocketAddress (IPv4 0) (PortNumber 0)
          nonce = nullNonce

          -- Standard AnnounceRequest envelope: Kind (0x83) + Nonce (24) + PK (32) + CipherText(120) = 177 bytes
          innerData = LBS.toStrict $ Put.runPut $ do
            Binary.put (0x83 :: Word8)
            Binary.put nullNonce
            Binary.put nullPk
            Put.putByteString (BS.replicate 120 0)

          p3 = Tunnel.OnionRequestPayloadFinal (Tunnel.OnionIPPort destAddr) innerData
      LBS.length (Binary.encode p3) `shouldBe` (19 + 177)

      let wrap = Path.wrapPath path destAddr nonce innerData
          res = runIdentity $ evalRandT (KeyedT.evalKeyedT wrap Map.empty) (mkStdGen 42)
          pkt = Packet PacketKind.OnionRequest0 res
      LBS.length (encode (pkt :: Packet Tunnel.OnionRequest0)) `shouldBe` 403
