\section{Host Address}

A Host Address is either an IPv4 or an IPv6 address.  The binary representation
of an IPv4 address is a Big Endian 32 bit unsigned integer (4 bytes).  For an
IPv6 address, it is a Big Endian 128 bit unsigned integer (16 bytes).  The
binary representation of a Host Address is a 7 bit unsigned integer specifying
the address family (2 for IPv4, 10 for IPv6), followed by the address itself.

Thus, when packed together with the Transport Protocol, the first bit of the
packed byte is the protocol and the next 7 bits are the address family.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE Trustworthy        #-}
module Network.Tox.NodeInfo.HostAddress where

import           Control.Applicative       ((<$>))
import           Control.Arrow             ((&&&))
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary (get, put)
import qualified Data.Binary.Bits.Get      as Bits
import qualified Data.Binary.Bits.Put      as Bits
import qualified Data.Binary.Get           as Bytes
import qualified Data.Binary.Put           as Bytes
import           Data.Bits                 (shiftL, shiftR, (.&.), (.|.))
import qualified Data.IP                   as IP
import           Data.List                 as List
import           Data.List.Split           as List
import           Data.Maybe                (listToMaybe, mapMaybe)
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word16, Word8)
import           GHC.Generics              (Generic)
import qualified Network.Socket            as Socket (HostAddress, HostAddress6)
import           Numeric                   (readHex, showHex)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen       as Gen
import           Text.Read                 (readPrec)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data HostAddress
  = IPv4 Socket.HostAddress
  | IPv6 Socket.HostAddress6
  deriving (Eq, Generic, Typeable)

instance Binary HostAddress
instance MessagePack HostAddress


instance Show HostAddress where
  show (IPv4 addr) = show $ IP.fromHostAddress  addr
  show (IPv6 addr) = show $ IP.fromHostAddress6 addr


instance Read HostAddress where
  readPrec = (<$> readPrec) $ \case
    IP.IPv4 ipv4 -> IPv4 $ IP.toHostAddress  ipv4
    IP.IPv6 ipv6 -> IPv6 $ IP.toHostAddress6 ipv6


getHostAddressGetter :: Bits.BitGet (Bytes.Get HostAddress)
getHostAddressGetter =
  Bits.getWord8 7 >>= \case
    2  -> return $ IPv4 <$> Binary.get
    10 -> return $ IPv6 <$> Binary.get
    n  -> fail $ "Invalid address family: " ++ show n


putAddressFamily :: HostAddress -> Bits.BitPut ()
putAddressFamily (IPv4 _) = Bits.putWord8 7 2
putAddressFamily (IPv6 _) = Bits.putWord8 7 10


putHostAddressValue :: HostAddress -> Bytes.Put
putHostAddressValue (IPv4 addr) = Binary.put addr
putHostAddressValue (IPv6 addr) = Binary.put addr


putHostAddress :: HostAddress -> (Bits.BitPut (), Bytes.Put)
putHostAddress = putAddressFamily &&& putHostAddressValue


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary HostAddress where
  arbitrary =
    Gen.oneof
      [ IPv4 <$> arbitrary
      , IPv6 <$> arbitrary
      ]
\end{code}
