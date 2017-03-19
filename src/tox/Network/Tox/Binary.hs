{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Tox.Binary
  ( typeName
  , encode, encodeC, encodeS
  , decode, decodeC, decodeS
  ) where

import           Control.Applicative                    ((<$>))
import           Control.Monad                          ((>=>))
import           Data.Binary                            (Binary, get, put)
import           Data.ByteString                        (ByteString)
import           Data.MessagePack                       (MessagePack,
                                                         fromObject, toObject)
import qualified Data.MessagePack                       as MessagePack
import           Data.Proxy                             (Proxy (..))
import           Data.Typeable                          (Typeable)
import qualified Data.Typeable                          as Typeable
import           Data.Word                              (Word64)
import           Network.MessagePack.Client             (Client)
import qualified Network.MessagePack.Client             as Client
import           Network.MessagePack.Server             (Server)
import qualified Network.MessagePack.Server             as Server

import qualified Network.Tox.Encoding                   as Encoding

import qualified Network.Tox.Crypto.Box                 as T
import qualified Network.Tox.Crypto.Key                 as T
import qualified Network.Tox.Crypto.KeyPair             as T
import qualified Network.Tox.DHT.DhtPacket              as T
import qualified Network.Tox.DHT.DhtRequestPacket       as T
import qualified Network.Tox.DHT.NodesRequest           as T
import qualified Network.Tox.DHT.NodesResponse          as T
import qualified Network.Tox.DHT.PingPacket             as T
import qualified Network.Tox.DHT.RpcPacket              as T
import qualified Network.Tox.NodeInfo.HostAddress       as T
import qualified Network.Tox.NodeInfo.NodeInfo          as T
import qualified Network.Tox.NodeInfo.PortNumber        as T
import qualified Network.Tox.NodeInfo.SocketAddress     as T
import qualified Network.Tox.NodeInfo.TransportProtocol as T
import qualified Network.Tox.Protocol.Packet            as T
import qualified Network.Tox.Protocol.PacketKind        as T


typeName :: Typeable a => Proxy a -> String
typeName (Proxy :: Proxy a) =
  show . Typeable.typeOf $ (undefined :: a)


data KnownType
  = CipherText        T.CipherText
  | DhtPacket         T.DhtPacket
  | DhtRequestPacket  T.DhtRequestPacket
  | HostAddress       T.HostAddress
  | Word64            Word64
  | Key               T.PublicKey
  | KeyPair           T.KeyPair
  | NodeInfo          T.NodeInfo
  | NodesRequest      T.NodesRequest
  | NodesResponse     T.NodesResponse
  | Packet            (T.Packet Word64)
  | PacketKind        T.PacketKind
  | PingPacket        T.PingPacket
  | PlainText         T.PlainText
  | PortNumber        T.PortNumber
  | RpcPacket         (T.RpcPacket Word64)
  | SocketAddress     T.SocketAddress
  | TransportProtocol T.TransportProtocol


knownTypeToObject :: KnownType -> MessagePack.Object
knownTypeToObject = \case
  CipherText        x -> toObject x
  DhtPacket         x -> toObject x
  DhtRequestPacket  x -> toObject x
  HostAddress       x -> toObject x
  Word64            x -> toObject x
  Key               x -> toObject x
  KeyPair           x -> toObject x
  NodeInfo          x -> toObject x
  NodesRequest      x -> toObject x
  NodesResponse     x -> toObject x
  Packet            x -> toObject x
  PacketKind        x -> toObject x
  PingPacket        x -> toObject x
  PlainText         x -> toObject x
  PortNumber        x -> toObject x
  RpcPacket         x -> toObject x
  SocketAddress     x -> toObject x
  TransportProtocol x -> toObject x


knownTypeEncode :: KnownType -> ByteString
knownTypeEncode = \case
  CipherText        x -> encode x
  DhtPacket         x -> encode x
  DhtRequestPacket  x -> encode x
  HostAddress       x -> encode x
  Word64            x -> encode x
  Key               x -> encode x
  KeyPair           x -> encode x
  NodeInfo          x -> encode x
  NodesRequest      x -> encode x
  NodesResponse     x -> encode x
  Packet            x -> encode x
  PacketKind        x -> encode x
  PingPacket        x -> encode x
  PlainText         x -> encode x
  PortNumber        x -> encode x
  RpcPacket         x -> encode x
  SocketAddress     x -> encode x
  TransportProtocol x -> encode x



--------------------------------------------------------------------------------
--
-- :: decode
--
--------------------------------------------------------------------------------


decode :: Binary a => ByteString -> Maybe a
decode = Encoding.decode

decodeC :: forall a. (Typeable a, MessagePack a)
        => ByteString -> Client (Maybe a)
decodeC = Client.call "Binary.decode" $ typeName (Proxy :: Proxy a)

decodeS :: Server.Method IO
decodeS = Server.method "Binary.decode"
  (Server.MethodDocs
    [ Server.MethodVal "typeName" "String"
    , Server.MethodVal "encoded" "ByteString"
    ] $ Server.MethodVal "value" "a")
  decodeKnownType

  where
    decodeKnownType :: String -> ByteString -> Server (Maybe MessagePack.Object)
    decodeKnownType = \case
      "CipherText"        -> go CipherText
      "DhtPacket"         -> go DhtPacket
      "DhtRequestPacket"  -> go DhtRequestPacket
      "HostAddress"       -> go HostAddress
      "Word64"            -> go Word64
      "Key PublicKey"     -> go Key
      "KeyPair"           -> go KeyPair
      "NodeInfo"          -> go NodeInfo
      "NodesRequest"      -> go NodesRequest
      "NodesResponse"     -> go NodesResponse
      "Packet Word64"     -> go Packet
      "PacketKind"        -> go PacketKind
      "PingPacket"        -> go PingPacket
      "PlainText"         -> go PlainText
      "PortNumber"        -> go PortNumber
      "RpcPacket Word64"  -> go RpcPacket
      "SocketAddress"     -> go SocketAddress
      "TransportProtocol" -> go TransportProtocol
      tycon               -> fail $ "unknown type: " ++ tycon

    go f = return . fmap (knownTypeToObject . f) . Encoding.decode


--------------------------------------------------------------------------------
--
-- :: encode
--
--------------------------------------------------------------------------------


encode :: Binary a => a -> ByteString
encode = Encoding.encode

encodeC :: forall a. (Typeable a, MessagePack a)
        => a -> Client ByteString
encodeC x = Client.call "Binary.encode" (show $ Typeable.typeOf x) x

encodeS :: Server.Method IO
encodeS = Server.method "Binary.encode"
  (Server.MethodDocs
    [ Server.MethodVal "typeName" "String"
    , Server.MethodVal "value" "a"
    ] $ Server.MethodVal "encoded" "ByteString")
  encodeKnownType

  where
    encodeKnownType :: String -> MessagePack.Object -> Server ByteString
    encodeKnownType = \case
      "CipherText"        -> go CipherText
      "DhtPacket"         -> go DhtPacket
      "DhtRequestPacket"  -> go DhtRequestPacket
      "HostAddress"       -> go HostAddress
      "Word64"            -> go Word64
      "Key PublicKey"     -> go Key
      "KeyPair"           -> go KeyPair
      "NodeInfo"          -> go NodeInfo
      "NodesRequest"      -> go NodesRequest
      "NodesResponse"     -> go NodesResponse
      "Packet Word64"     -> go Packet
      "PacketKind"        -> go PacketKind
      "PingPacket"        -> go PingPacket
      "PlainText"         -> go PlainText
      "PortNumber"        -> go PortNumber
      "RpcPacket Word64"  -> go RpcPacket
      "SocketAddress"     -> go SocketAddress
      "TransportProtocol" -> go TransportProtocol
      tycon               -> fail $ "unknown type: " ++ tycon

    go f = fmap (knownTypeEncode . f) . fromObject
