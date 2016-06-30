{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Tox.Binary where

import           Control.Applicative           ((<$>))
import           Data.Binary                   (Binary)
import           Data.ByteString               (ByteString)
import           Data.Proxy                    (Proxy (..))
import           Data.Typeable                 (Typeable)
import qualified Data.Typeable                 as Typeable

import qualified Network.Tox.Encoding          as Encoding
import qualified Network.Tox.NodeInfo.NodeInfo as T
import qualified Network.Tox.RPC               as RPC


typeName :: Typeable a => Proxy a -> String
typeName = Typeable.tyConName . Typeable.typeRepTyCon . Typeable.typeRep


--------------------------------------------------------------------------------
--
-- :: decode
--
--------------------------------------------------------------------------------


decode :: Binary a => ByteString -> Maybe a
decode = Encoding.decode

decodeC :: forall a. (Typeable a, Binary a, RPC.MessagePack a)
        => ByteString -> RPC.Client (Maybe a)
decodeC = RPC.call "Binary.decode" $ typeName (Proxy :: Proxy a)

decodeM :: (RPC.MessagePack a, Binary a) => Proxy a -> ByteString -> RPC.Server (Maybe RPC.Object)
decodeM (Proxy :: Proxy a) bs =
  return $ RPC.toObject <$> (decode bs :: Maybe a)

decodeF :: String -> ByteString -> RPC.Server (Maybe RPC.Object)
decodeF "Int"      = decodeM (Proxy :: Proxy Int       )
decodeF "NodeInfo" = decodeM (Proxy :: Proxy T.NodeInfo)
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

encodeC :: forall a. (Typeable a, Binary a, RPC.MessagePack a)
        => a -> RPC.Client ByteString
encodeC = RPC.call "Binary.encode" $ typeName (Proxy :: Proxy a)

encodeM :: (RPC.MessagePack a, Binary a) => Proxy a -> RPC.Object -> RPC.Server ByteString
encodeM (Proxy :: Proxy a) obj =
  case (RPC.fromObject obj :: Maybe a) of
    Nothing -> fail $ "failed to decode from object: " ++ show obj
    Just a  -> return $ encode a

encodeF :: String -> RPC.Object -> RPC.Server ByteString
encodeF "Int"      = encodeM (Proxy :: Proxy Int)
encodeF "NodeInfo" = encodeM (Proxy :: Proxy T.NodeInfo)
encodeF tycon = const $ fail $ "unsupported type in encode: " ++ tycon

encodeS :: RPC.Method IO
encodeS = RPC.method "Binary.encode" encodeF
