{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Tox.Binary where

import           Control.Applicative                    ((<$>))
import           Data.Binary                            (Binary)
import           Data.ByteString                        (ByteString)
import           Data.Proxy                             (Proxy (..))
import           Data.Typeable                          (Typeable)
import qualified Data.Typeable                          as Typeable
import           Data.Word                              (Word64)

import qualified Network.Tox.Encoding                   as Encoding
import qualified Network.Tox.RPC                        as RPC

import qualified Network.Tox.Crypto.Box                 as T
import qualified Network.Tox.Crypto.Key                 as T
import qualified Network.Tox.Crypto.KeyPair             as T
import qualified Network.Tox.DHT.DhtPacket              as T
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
  Typeable.tyConName . Typeable.typeRepTyCon . Typeable.typeOf $ (undefined :: a)


--------------------------------------------------------------------------------
--
-- :: decode
--
--------------------------------------------------------------------------------


decode :: Binary a => ByteString -> Maybe a
decode = Encoding.decode

decodeC :: forall a. (Typeable a, RPC.MessagePack a)
        => ByteString -> RPC.Client (Maybe a)
decodeC = RPC.call "Binary.decode" $ typeName (Proxy :: Proxy a)

decodeM :: (Monad m, RPC.MessagePack a, Binary a) => Proxy a -> ByteString -> m (Maybe RPC.Object)
decodeM (Proxy :: Proxy a) bs =
  return $ RPC.toObject <$> (decode bs :: Maybe a)

decodeF :: String -> ByteString -> RPC.Server (Maybe RPC.Object)
decodeF "CipherText"        = decodeM (Proxy :: Proxy T.CipherText        )
decodeF "DhtPacket"         = decodeM (Proxy :: Proxy T.DhtPacket         )
decodeF "HostAddress"       = decodeM (Proxy :: Proxy T.HostAddress       )
decodeF "Word64"            = decodeM (Proxy :: Proxy Word64              )
decodeF "Key"               = decodeM (Proxy :: Proxy T.PublicKey         )
decodeF "KeyPair"           = decodeM (Proxy :: Proxy T.KeyPair           )
decodeF "NodeInfo"          = decodeM (Proxy :: Proxy T.NodeInfo          )
decodeF "NodesRequest"      = decodeM (Proxy :: Proxy T.NodesRequest      )
decodeF "NodesResponse"     = decodeM (Proxy :: Proxy T.NodesResponse     )
decodeF "Packet"            = decodeM (Proxy :: Proxy (T.Packet Word64)   )
decodeF "PacketKind"        = decodeM (Proxy :: Proxy T.PacketKind        )
decodeF "PingPacket"        = decodeM (Proxy :: Proxy T.PingPacket        )
decodeF "PlainText"         = decodeM (Proxy :: Proxy T.PlainText         )
decodeF "PortNumber"        = decodeM (Proxy :: Proxy T.PortNumber        )
decodeF "RpcPacket"         = decodeM (Proxy :: Proxy (T.RpcPacket Word64))
decodeF "SocketAddress"     = decodeM (Proxy :: Proxy T.SocketAddress     )
decodeF "TransportProtocol" = decodeM (Proxy :: Proxy T.TransportProtocol )
decodeF tycon = const $ fail $ "unsupported type in decode: " ++ tycon

decodeS :: RPC.Method IO
decodeS = RPC.method "Binary.decode" decodeF


--------------------------------------------------------------------------------
--
-- :: encode
--
--------------------------------------------------------------------------------


encode :: Binary a => a -> ByteString
encode = Encoding.encode

encodeC :: forall a. (Typeable a, RPC.MessagePack a)
        => a -> RPC.Client ByteString
encodeC = RPC.call "Binary.encode" $ typeName (Proxy :: Proxy a)

encodeM :: (Monad m, RPC.MessagePack a, Binary a)
        => Proxy a -> RPC.Object -> m ByteString
encodeM (Proxy :: Proxy a) obj =
  case (RPC.fromObject obj :: Maybe a) of
    Nothing -> fail $ "failed to decode from object: " ++ show obj
    Just a  -> return $ encode a

encodeF :: String -> RPC.Object -> RPC.Server ByteString
encodeF "CipherText"        = encodeM (Proxy :: Proxy T.CipherText        )
encodeF "DhtPacket"         = encodeM (Proxy :: Proxy T.DhtPacket         )
encodeF "HostAddress"       = encodeM (Proxy :: Proxy T.HostAddress       )
encodeF "Word64"            = encodeM (Proxy :: Proxy Word64              )
encodeF "Key"               = encodeM (Proxy :: Proxy T.PublicKey         )
encodeF "KeyPair"           = encodeM (Proxy :: Proxy T.KeyPair           )
encodeF "NodeInfo"          = encodeM (Proxy :: Proxy T.NodeInfo          )
encodeF "NodesRequest"      = encodeM (Proxy :: Proxy T.NodesRequest      )
encodeF "NodesResponse"     = encodeM (Proxy :: Proxy T.NodesResponse     )
encodeF "Packet"            = encodeM (Proxy :: Proxy (T.Packet Word64)   )
encodeF "PacketKind"        = encodeM (Proxy :: Proxy T.PacketKind        )
encodeF "PingPacket"        = encodeM (Proxy :: Proxy T.PingPacket        )
encodeF "PlainText"         = encodeM (Proxy :: Proxy T.PlainText         )
encodeF "PortNumber"        = encodeM (Proxy :: Proxy T.PortNumber        )
encodeF "RpcPacket"         = encodeM (Proxy :: Proxy (T.RpcPacket Word64))
encodeF "SocketAddress"     = encodeM (Proxy :: Proxy T.SocketAddress     )
encodeF "TransportProtocol" = encodeM (Proxy :: Proxy T.TransportProtocol )
encodeF tycon = const $ fail $ "unsupported type in encode: " ++ tycon

encodeS :: RPC.Method IO
encodeS = RPC.method "Binary.encode" encodeF
