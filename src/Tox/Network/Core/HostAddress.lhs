\section{Host Address}

A Host Address is either an IPv4 or an IPv6 address.  The binary representation
of an IPv4 address is a Big Endian 32 bit unsigned integer (4 bytes).  For an
IPv6 address, it is a Big Endian 128 bit unsigned integer (16 bytes).  The
binary representation of a Host Address is a 7 bit unsigned integer specifying
the address family (2 for IPv4, 10 for IPv6), followed by the address itself.

Thus, when packed together with the Transport Protocol, the first bit of the
packed byte is the protocol and the next 7 bits are the address family.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Tox.Network.Core.HostAddress where

import           Control.Arrow             ((&&&))
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary (get, put)
import qualified Data.Binary.Get           as Bytes
import qualified Data.Binary.Put           as Bytes
import qualified Data.IP                   as IP
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word8)
import           GHC.Generics              (Generic)
import qualified Network.Socket            as Socket (HostAddress, HostAddress6,
                                                      hostAddressToTuple,
                                                      tupleToHostAddress)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen       as Gen
import           Text.Read                 (readMaybe, readPrec)
import qualified Tox.Core.Bits.Get         as Bits
import qualified Tox.Core.Bits.Put         as Bits


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data HostAddress
  = IPv4 Socket.HostAddress
  | IPv6 Socket.HostAddress6
  deriving (Eq, Ord, Generic, Typeable)

instance Binary HostAddress
instance MessagePack HostAddress


instance Show HostAddress where
  show (IPv4 addr) = show . show . IP.fromHostAddress  $ addr
  show (IPv6 addr) = show . show . IP.fromHostAddress6 $ addr


instance Read HostAddress where
  readPrec = do
    str <- readPrec
    case readMaybe str of
      Nothing             -> fail "HostAddress"
      Just (IP.IPv4 ipv4) -> return . IPv4 . IP.toHostAddress  $ ipv4
      Just (IP.IPv6 ipv6) -> return . IPv6 . IP.toHostAddress6 $ ipv6


getHostAddressGetter :: Bits.BitGet (Bytes.Get HostAddress)
getHostAddressGetter =
  Bits.getWord8 7 >>= \case
    2  -> return $ IPv4 <$> getIPv4
    10 -> return $ IPv6 <$> Binary.get
    n  -> fail $ "Invalid address family: " ++ show n


putAddressFamily :: HostAddress -> Bits.BitPut ()
putAddressFamily (IPv4 _) = Bits.putWord8 7 2
putAddressFamily (IPv6 _) = Bits.putWord8 7 10


putHostAddressValue :: HostAddress -> Bytes.Put
putHostAddressValue (IPv4 addr) = putIPv4 addr
putHostAddressValue (IPv6 addr) = Binary.put addr

-- | Whether the address is this machine's: 127.0.0.0/8 or ::1, also as an
-- IPv4-mapped IPv6 address.
isLoopback :: HostAddress -> Bool
isLoopback (IPv4 addr) = let (a, _, _, _) = Socket.hostAddressToTuple addr in a == 127
isLoopback (IPv6 (0, 0, 0, 1)) = True
isLoopback (IPv6 (0, 0, 0xffff, v4)) = v4 `div` 0x1000000 == 127
isLoopback (IPv6 _) = False

-- | Whether the address is on a local network, as toxcore decides which LAN
-- discovery packets to take.
isLan :: HostAddress -> Bool
isLan addr | isLoopback addr = True
isLan (IPv4 addr) = isLanIPv4 $ Socket.hostAddressToTuple addr
isLan (IPv6 (a, _, _, d))
  | a `div` 0x1000000 == 0xff && (a `div` 0x10000) `mod` 0x100 < 3 && d `mod` 0x100 == 1 = True
  | a `div` 0x400000 == 0xfe80 `div` 0x40 = True
isLan (IPv6 (0, 0, 0xffff, v4)) =
  isLanIPv4 (fromIntegral $ v4 `div` 0x1000000, fromIntegral $ v4 `div` 0x10000, fromIntegral $ v4 `div` 0x100, fromIntegral v4)
isLan (IPv6 _) = False

isLanIPv4 :: (Word8, Word8, Word8, Word8) -> Bool
isLanIPv4 (a, b, c, _) =
  a == 10
    || (a == 172 && b >= 16 && b <= 31)
    || (a == 192 && b == 168)
    || (a == 169 && b == 254 && c /= 0 && c /= 255)
    || (a == 100 && b >= 64 && b < 128)

-- | IPv4 addresses are sent as their four octets in order.  A
-- 'Socket.HostAddress' holds them in memory order, not as a number.
putIPv4 :: Socket.HostAddress -> Bytes.Put
putIPv4 addr = mapM_ Bytes.putWord8 [a, b, c, d]
  where
    (a, b, c, d) = Socket.hostAddressToTuple addr

getIPv4 :: Bytes.Get Socket.HostAddress
getIPv4 = do
  a <- Bytes.getWord8
  b <- Bytes.getWord8
  c <- Bytes.getWord8
  d <- Bytes.getWord8
  return $ Socket.tupleToHostAddress (a, b, c, d)


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
