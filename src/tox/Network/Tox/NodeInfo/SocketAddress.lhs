\section{Socket Address}

A Socket Address is a pair of Host Address and Port Number.  Together with a
Transport Protocol, it is sufficient information to address a network port on
any internet host.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE Trustworthy        #-}
module Network.Tox.NodeInfo.SocketAddress where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Binary                            (Binary, get, put)
import qualified Data.Binary.Bits.Get                   as Bits (runBitGet)
import qualified Data.Binary.Bits.Put                   as Bits (runBitPut)
import qualified Data.Binary.Get                        as Binary (Get)
import qualified Data.Binary.Put                        as Binary (Put)
import           Data.MessagePack                       (MessagePack)
import           Data.Typeable                          (Typeable)
import           GHC.Generics                           (Generic)
import qualified Network.Socket                         as Socket
import           Network.Tox.Encoding                   (bitGet, bitPut)
import           Network.Tox.NodeInfo.HostAddress       (HostAddress (..))
import qualified Network.Tox.NodeInfo.HostAddress       as HostAddress
import           Network.Tox.NodeInfo.PortNumber        (PortNumber)
import           Network.Tox.NodeInfo.TransportProtocol (TransportProtocol)
import           Test.QuickCheck.Arbitrary              (Arbitrary, arbitrary)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data SocketAddress = SocketAddress HostAddress PortNumber
  deriving (Eq, Show, Read, Generic, Typeable)

instance Binary SocketAddress
instance MessagePack SocketAddress


putSocketAddress :: TransportProtocol -> SocketAddress -> Binary.Put
putSocketAddress protocol (SocketAddress hostAddress portNumber) =
  let (putAddressFamily, putHostAddress) = HostAddress.putHostAddress hostAddress in
  do
    Bits.runBitPut $ do
      bitPut protocol -- first bit = protocol
      putAddressFamily -- 7 bits = address family
    putHostAddress
    put portNumber


getSocketAddress :: Binary.Get (TransportProtocol, SocketAddress)
getSocketAddress = do
  (protocol, getHostAddress) <- Bits.runBitGet $ do
    protocol <- bitGet
    getHostAddress <- HostAddress.getHostAddressGetter
    return (protocol, getHostAddress)
  hostAddress <- getHostAddress
  portNumber <- get
  return (protocol, SocketAddress hostAddress portNumber)


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary SocketAddress where
  arbitrary =
    SocketAddress <$> arbitrary <*> arbitrary
\end{code}
