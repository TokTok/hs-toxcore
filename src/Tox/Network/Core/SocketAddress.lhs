\section{Socket Address}

A Socket Address is a pair of Host Address and Port Number.  Together with a
Transport Protocol, it is sufficient information to address a network port on
any internet host.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StrictData         #-}
module Tox.Network.Core.SocketAddress where

import           Data.Binary                        (Binary, get, put)
import qualified Data.Binary.Get                    as Binary (Get)
import qualified Data.Binary.Put                    as Binary (Put)
import           Data.MessagePack                   (MessagePack)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Test.QuickCheck.Arbitrary          (Arbitrary, arbitrary)
import qualified Tox.Core.Bits.Get                  as Bits (runBitGet)
import qualified Tox.Core.Bits.Put                  as Bits (runBitPut)
import           Tox.Network.Core.Encoding          (bitGet, bitPut)
import qualified Tox.Network.Core.HostAddress       as HostAddress
import           Tox.Network.Core.HostAddress       (HostAddress (..))
import           Tox.Network.Core.PortNumber        (PortNumber)
import           Tox.Network.Core.TransportProtocol (TransportProtocol)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data SocketAddress = SocketAddress HostAddress PortNumber
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Binary SocketAddress
instance MessagePack SocketAddress


isLan :: SocketAddress -> Bool
isLan (SocketAddress host _) = HostAddress.isLan host


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
