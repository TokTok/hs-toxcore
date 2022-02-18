\subsection{Conferences (0x14)}

This section contains a list of saved conferences.

\begin{code}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
module Network.Tox.SaveData.Conferences where

import           Data.Binary               (Binary (..))
import qualified Data.Binary.Get           as Get
import qualified Data.Binary.Put           as Put
import qualified Data.ByteString           as BS
import           Data.MessagePack          (MessagePack)
import           Data.Word                 (Word16, Word32, Word64, Word8)
import           GHC.Generics              (Generic)
import           Network.Tox.Crypto.Key    (PublicKey)
import qualified Network.Tox.SaveData.Util as Util
import           Test.QuickCheck.Arbitrary (Arbitrary (..), genericShrink)
import qualified Test.QuickCheck.Arbitrary as Arbitrary

\end{code}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & List of conferences \\
\end{tabular}

\begin{code}

newtype Conferences = Conferences [Conference]
    deriving (Eq, Show, Read, Generic)

instance MessagePack Conferences

instance Binary Conferences where
    get = Conferences <$> Util.getList
    put (Conferences xs) = mapM_ put xs

instance Arbitrary Conferences where
    arbitrary = Conferences <$> arbitrary
    shrink    = genericShrink

\end{code}

Conference:

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{1}    & \texttt{uint8\_t} Groupchat type \\
  \texttt{32}   & Groupchat id \\
  \texttt{4}    & \texttt{uint32\_t} Message number \\
  \texttt{2}    & \texttt{uint16\_t} Lossy message number \\
  \texttt{2}    & \texttt{uint16\_t} Peer number \\
  \texttt{4}    & \texttt{uint32\_t} Number of peers \\
  \texttt{1}    & \texttt{uint8\_t} Title length \\
  \texttt{?}    & Title \\
  \texttt{?}    & List of peers \\
\end{tabular}

All peers other than the saver are saved, including frozen peers. On reload,
they all start as frozen.

\begin{code}

maxTitleLen :: Int
maxTitleLen = 128

data Conference = Conference
    { conferenceType     :: Word8
    , conferenceId       :: BS.ByteString
    , messageNumber      :: Word32
    , lossyMessageNumber :: Word16
    , selfPeerNumber     :: Word16
    , title              :: BS.ByteString
    , peers              :: [Peer]
    }
    deriving (Eq, Show, Read, Generic)

instance MessagePack Conference

instance Binary Conference where
    get = do
        conferenceType     <- Get.getWord8
        conferenceId       <- Get.getByteString 32
        messageNumber      <- Get.getWord32le
        lossyMessageNumber <- Get.getWord16le
        selfPeerNumber     <- Get.getWord16le
        peerCount          <- Get.getWord32le
        titleLength        <- Get.getWord8
        title              <- Get.getByteString (fromIntegral titleLength)
        peers              <- mapM (const get) [1..peerCount]
        return Conference{..}

    put Conference{..} = do
        Put.putWord8      conferenceType
        Put.putByteString conferenceId
        Put.putWord32le   messageNumber
        Put.putWord16le   lossyMessageNumber
        Put.putWord16le   selfPeerNumber
        Put.putWord32le   (fromIntegral $ length peers)
        Put.putWord8      (fromIntegral $ BS.length title)
        Put.putByteString title
        mapM_ put peers


instance Arbitrary Conference where
    arbitrary = Conference
        <$> arbitrary
        <*> (BS.pack <$> Arbitrary.vector 32)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack . take maxTitleLen <$> arbitrary)
        <*> arbitrary

\end{code}

Peer:

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{32}   & Long term public key \\
  \texttt{32}   & DHT public key \\
  \texttt{2}    & \texttt{uint16\_t} Peer number \\
  \texttt{8}    & \texttt{uint64\_t} Last active timestamp \\
  \texttt{1}    & \texttt{uint8\_t} Name length \\
  \texttt{?}    & Name \\
\end{tabular}

\begin{code}

maxNameLen :: Int
maxNameLen = 128

data Peer = Peer
    { publicKey      :: PublicKey
    , dhtPublicKey   :: PublicKey
    , peerNumber     :: Word16
    , lastActiveTime :: Word64
    , name           :: BS.ByteString
    }
    deriving (Eq, Show, Read, Generic)

instance MessagePack Peer

instance Binary Peer where
    get = do
        publicKey      <- get
        dhtPublicKey   <- get
        peerNumber     <- Get.getWord16le
        lastActiveTime <- Get.getWord64le
        nameLength     <- Get.getWord8
        name           <- Get.getByteString (fromIntegral nameLength)
        return Peer{..}

    put Peer{..} = do
        put               publicKey
        put               dhtPublicKey
        Put.putWord16le   peerNumber
        Put.putWord64le   lastActiveTime
        Put.putWord8      (fromIntegral $ BS.length name)
        Put.putByteString name

instance Arbitrary Peer where
    arbitrary = Peer
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack . take maxNameLen <$> arbitrary)

\end{code}
