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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Trustworthy   #-}
module Network.Tox.NodeInfo.HostAddress where

import           Control.Applicative       ((<$>))
import           Control.Arrow             ((&&&))
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary (get, put)
import qualified Data.Binary.Bits.Get      as Bits
import qualified Data.Binary.Bits.Put      as Bits
import qualified Data.Binary.Get           as Bytes
import qualified Data.Binary.Put           as Bytes
import           Data.Bits                 (shiftL, shiftR, (.&.), (.|.))
import           Data.List                 as List
import           Data.List.Split           as List
import           Data.Maybe                (listToMaybe, mapMaybe)
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
  deriving (Eq, Generic)

instance Binary HostAddress
instance ToJSON HostAddress
instance FromJSON HostAddress


word8sToIPv4 :: (Word8, Word8, Word8, Word8) -> Socket.HostAddress
word8sToIPv4 (byte0, byte1, byte2, byte3) =
  let
    bytes = zip [0..] $ map (fromInteger . toInteger) [byte0, byte1, byte2, byte3]
    shifted = map (\(index, byte) -> byte `shiftL` (8 * index)) bytes
    word = foldl (.|.) 0 shifted
  in
  word


ipv4ToWord8s :: Socket.HostAddress -> (Word8, Word8, Word8, Word8)
ipv4ToWord8s addr =
  ( toWord8   (addr .&. 0x000000ff)
  , toWord8 $ (addr .&. 0x0000ff00) `shiftR` 8
  , toWord8 $ (addr .&. 0x00ff0000) `shiftR` (8 * 2)
  , toWord8 $ (addr .&. 0xff000000) `shiftR` (8 * 3)
  )
  where
    toWord8 = fromInteger . toInteger


word16sToIPv6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Socket.HostAddress6
word16sToIPv6 (word0, word1, word2, word3, word4, word5, word6, word7) =
  ( pairToWord32 (word0, word1)
  , pairToWord32 (word2, word3)
  , pairToWord32 (word4, word5)
  , pairToWord32 (word6, word7)
  )

  where
    pairToWord32 (lo, hi) = toWord32 lo .|. (toWord32 hi `shiftL` 16)
    toWord32 = fromInteger . toInteger


ipv6ToWord16s :: Socket.HostAddress6 -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
ipv6ToWord16s (addr0, addr1, addr2, addr3) =
  ( toWord16   (addr0 .&. 0x0000ffff)
  , toWord16 $ (addr0 .&. 0xffff0000) `shiftR` 16
  , toWord16   (addr1 .&. 0x0000ffff)
  , toWord16 $ (addr1 .&. 0xffff0000) `shiftR` 16
  , toWord16   (addr2 .&. 0x0000ffff)
  , toWord16 $ (addr2 .&. 0xffff0000) `shiftR` 16
  , toWord16   (addr3 .&. 0x0000ffff)
  , toWord16 $ (addr3 .&. 0xffff0000) `shiftR` 16
  )
  where
    toWord16 = fromInteger . toInteger


instance Show HostAddress where
  show (IPv4 addr) =
    let (byte0, byte1, byte2, byte3) = ipv4ToWord8s addr in
    show $ List.intercalate "." $ map show [byte0, byte1, byte2, byte3]

  show (IPv6 addr) =
    let (word0, word1, word2, word3, word4, word5, word6, word7) = ipv6ToWord16s addr in
    show $ List.intercalate ":" $ map (`showHex` "") [word0, word1, word2, word3, word4, word5, word6, word7]


instance Read HostAddress where
  readPrec = do
    text <- readPrec
    if ':' `elem` text then
      case mapMaybe (listToMaybe . map fst . readHex) $ List.splitOn ":" text of
        [word0, word1, word2, word3, word4, word5, word6, word7] ->
          return $ IPv6 $ word16sToIPv6 (word0, word1, word2, word3, word4, word5, word6, word7)
        _ ->
          fail $ "Could not parse as IP address: " ++ text
    else
      case map read $ List.splitOn "." text of
        [byte0, byte1, byte2, byte3] ->
          return $ IPv4 $ word8sToIPv4 (byte0, byte1, byte2, byte3)
        _ ->
          fail $ "Could not parse as IP address: " ++ text


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
