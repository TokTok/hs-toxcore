{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
module Network.Tox.Binary
  ( typeName
  , encode
  , decode
  ) where

import           Data.Binary                            (Binary)
import           Data.ByteString                        (ByteString)
import           Data.MessagePack                       (MessagePack,
                                                         fromObject, toObject)
import qualified Data.MessagePack                       as MessagePack
import           Data.Proxy                             (Proxy (..))
import           Data.Typeable                          (Typeable)
import qualified Data.Typeable                          as Typeable
import           Data.Word                              (Word64)

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



--------------------------------------------------------------------------------
--
-- :: decode
--
--------------------------------------------------------------------------------


decode :: Binary a => ByteString -> Maybe a
decode = Encoding.decode


--------------------------------------------------------------------------------
--
-- :: encode
--
--------------------------------------------------------------------------------


encode :: Binary a => a -> ByteString
encode = Encoding.encode
