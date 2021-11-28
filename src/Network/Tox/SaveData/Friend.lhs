\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Network.Tox.SaveData.Friend where

import           Data.Binary               (Binary (..))
import qualified Data.Binary.Get           as Get
import qualified Data.Binary.Put           as Put
import qualified Data.ByteString           as BS
import           Data.Monoid               ((<>))
import           Data.Word                 (Word32, Word64, Word8)
import           Network.Tox.Crypto.Key    (PublicKey)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
\end{code}

Friend:

The integers in this structure are stored in Big Endian format.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{1}    & \texttt{uint8\_t} Status \\
  \texttt{32}   & Long term public key \\
  \texttt{1024} & Friend request message as a byte string \\
  \texttt{1}    & PADDING \\
  \texttt{2}    & \texttt{uint16\_t} Size of the friend request message \\
  \texttt{128}  & Name as a byte string \\
  \texttt{2}    & \texttt{uint16\_t} Size of the name \\
  \texttt{1007} & Status message as a byte string \\
  \texttt{1}    & PADDING \\
  \texttt{2}    & \texttt{uint16\_t} Size of the status message \\
  \texttt{1}    & \texttt{uint8\_t} User status (see also: \texttt{USERSTATUS}) \\
  \texttt{3}    & PADDING \\
  \texttt{4}    & \texttt{uint32\_t} Nospam (only used for sending a friend request) \\
  \texttt{8}    & \texttt{uint64\_t} Last seen time \\
\end{tabular}

Status can be one of:

\begin{tabular}{l|l}
  Status & Meaning \\
  \hline
  0      & Not a friend \\
  1      & Friend added \\
  2      & Friend request sent \\
  3      & Confirmed friend \\
  4      & Friend online \\
\end{tabular}

\begin{code}

data Friend = Friend
    { status        :: Word8
    , publicKey     :: PublicKey
    , friendRequest :: BS.ByteString
    , name          :: BS.ByteString
    , statusMessage :: BS.ByteString
    , userStatus    :: Word8
    , nospam        :: Word32
    , lastSeenTime  :: Word64
    }
    deriving (Eq, Show, Read)

maxFriendRequestLen :: Int
maxFriendRequestLen = 1024

maxNameLen :: Int
maxNameLen = 128

maxStatusMessageLen :: Int
maxStatusMessageLen = 1007

instance Binary Friend where
    get = do
        status           <- Get.getWord8
        publicKey        <- get
        friendRequest'   <- Get.getByteString maxFriendRequestLen
        _                <- Get.getWord8
        friendRequestLen <- Get.getWord16be
        name'            <- Get.getByteString maxNameLen
        nameLen          <- Get.getWord16be
        statusMessage'   <- Get.getByteString maxStatusMessageLen
        _                <- Get.getWord8
        statusMessageLen <- Get.getWord16be
        userStatus       <- Get.getWord8
        _                <- Get.getByteString 3
        nospam           <- Get.getWord32be
        lastSeenTime     <- Get.getWord64be

        let friendRequest = BS.take (fromIntegral friendRequestLen) friendRequest'
        let name = BS.take (fromIntegral nameLen) name'
        let statusMessage = BS.take (fromIntegral statusMessageLen) statusMessage'

        return Friend{..}

    put Friend {..} = do
        let friendRequestLen = BS.length friendRequest
        let friendRequest' = friendRequest
                <> BS.replicate (maxFriendRequestLen - friendRequestLen) 0

        let nameLen = BS.length name
        let name' = name
                <> BS.replicate (maxNameLen - nameLen) 0

        let statusMessageLen = BS.length statusMessage
        let statusMessage' = statusMessage
                <> BS.replicate (maxStatusMessageLen - statusMessageLen) 0

        Put.putWord8            status
        put                     publicKey
        Put.putByteString       friendRequest'
        Put.putWord8            0
        Put.putWord16be         (fromIntegral friendRequestLen)
        Put.putByteString       name'
        Put.putWord16be         (fromIntegral nameLen)
        Put.putByteString       statusMessage'
        Put.putWord8            0
        Put.putWord16be         (fromIntegral statusMessageLen)
        Put.putWord8            userStatus
        Put.putByteString       "\0\0\0"
        Put.putWord32be         nospam
        Put.putWord64be         lastSeenTime

instance Arbitrary Friend where
    arbitrary = Friend
        <$> arbitrary
        <*> arbitrary
        <*> (BS.pack . take maxFriendRequestLen <$> arbitrary)
        <*> (BS.pack . take maxNameLen <$> arbitrary)
        <*> (BS.pack . take maxStatusMessageLen <$> arbitrary)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

\end{code}
