{-# LANGUAGE Trustworthy #-}
module Network.NetworkSpec where

import           Test.Hspec

import           Control.Monad                   (unless)
import           Data.Binary                     (Binary)
import qualified Data.Binary                     as Binary (get, put)
import qualified Data.Binary.Get                 as Binary (runGet)
import qualified Data.Binary.Put                 as Binary (runPut)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base16          as Base16
import qualified Data.ByteString.Lazy            as LazyByteString
import qualified Network.Socket                  as Socket hiding (recvFrom,
                                                            sendTo)
import qualified Network.Socket.ByteString       as Socket
import           Network.Tox.Crypto.Key          (Nonce, PublicKey)
import           Network.Tox.Crypto.KeyPair      (KeyPair (..))
import           Network.Tox.Crypto.Text         (PlainText (..))
import qualified Network.Tox.Crypto.Text         as Text
import qualified Network.Tox.DHT.DhtPacket       as DhtPacket
import           Network.Tox.DHT.NodesRequest    (NodesRequest (..))
import           Network.Tox.DHT.NodesResponse   (NodesResponse (..))
import           Network.Tox.DHT.RpcPacket       (RequestId (..),
                                                  RpcPacket (..))
import           Network.Tox.Protocol.Packet     (Packet (..))
import           Network.Tox.Protocol.PacketKind (PacketKind)
import qualified Network.Tox.Protocol.PacketKind as PacketKind


toxPort :: Socket.PortNumber
toxPort = 33445


zeroNonce :: Nonce
zeroNonce = read "\"010203040506070809080706050403020102030405060708\""

dhtKeyPair :: KeyPair
dhtKeyPair = read "KeyPair { secretKey = \"4c37a62d0a72a83b9e26390e1b5e5024dc3cbd19745c999c09525fefc074bcdc\", publicKey = \"ad8611f27b1b11dbc7704025316102371088db8f97af54a9a4dfc5a6a8cd0f47\" }"

dhtPublicKey :: PublicKey
KeyPair _ dhtPublicKey = dhtKeyPair


targetKey :: PublicKey
targetKey = read "\"788236D34978D1D5BD822F0A5BEBD2C53C64CC31CD3149350EE27D4D9A2F9B6B\""
--targetKey = read "\"ee8b801a3aed8dc3907fcedea76e4beb3a0db5f16a9a9e1a07e8d9788746f51e\""

targetAddr :: Socket.SockAddr
targetAddr =
  Socket.SockAddrInet toxPort targetIp
  where
    -- 178.62.250.138
    targetIp = 178 * 0x00000001 + 62 * 0x00000100 + 250 * 0x00010000 + 138 * 0x01000000


makePacket :: Binary payload => KeyPair -> Nonce -> PacketKind -> payload -> ByteString
makePacket keyPair nonce kind payload =
  let
    packet = Packet kind $ DhtPacket.encode keyPair targetKey nonce $ RpcPacket payload (RequestId 0)
  in
  LazyByteString.toStrict $ Binary.runPut $ Binary.put packet


decryptPacket :: KeyPair -> ByteString -> Maybe (PlainText NodesResponse)
decryptPacket keyPair =
  DhtPacket.decrypt keyPair . packetPayload . Binary.runGet Binary.get . LazyByteString.fromStrict


sendPacket :: Binary payload => Socket.Socket -> KeyPair -> Nonce -> Socket.SockAddr -> PacketKind -> payload -> IO ()
sendPacket sock keyPair nonce target kind payload =
  let packet = makePacket keyPair nonce kind payload in
  do
    putStrLn $ "Send: " ++ show (Base16.encode packet)
    sentBytes <- Socket.sendTo sock packet target
    putStrLn $ "Sent " ++ show sentBytes ++ " bytes"


dhtLoop :: Socket.Socket -> IO ()
dhtLoop sock = do
  putStrLn "Waiting for packet..."
  (msg, client) <- Socket.recvFrom sock 1500
  putStrLn $ "Recv: " ++ show (Base16.encode msg) ++ " from " ++ show client
  print (decryptPacket dhtKeyPair msg)
  print (decryptPacket dhtKeyPair msg >>= Text.decode)
  --dhtLoop sock
  return ()


spec :: Spec
spec =
  unless True $
    it "should receive ping responses" $
      -- Set up socket.
      Socket.withSocketsDo $ do
        sock <- Socket.socket Socket.AF_INET Socket.Datagram 0
        Socket.bindSocket sock (Socket.SockAddrInet (toxPort + 1) Socket.iNADDR_ANY)

        --sendPacket sock dhtKeyPair zeroNonce targetAddr PacketKind.PingRequest PingRequest
        sendPacket sock dhtKeyPair zeroNonce targetAddr PacketKind.NodesRequest $ NodesRequest dhtPublicKey

        dhtLoop sock
