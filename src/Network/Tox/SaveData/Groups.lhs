\subsection{Groups (0x14)}

This section contains a list of saved conferences.

\begin{code}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
module Network.Tox.SaveData.Groups where

import qualified Crypto.Saltine.Class       as Sodium (encode)
import           Data.Binary                (Binary (..))
import qualified Data.Binary.Get            as Get
import qualified Data.Binary.Put            as Put
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as Base16
import           Data.MessagePack           (MessagePack, defaultConfig,
                                             toObject)
import           Data.MessagePack.Arbitrary ()
import           Data.MessagePack.Types     (Object, fromObject)
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           GHC.Generics               (Generic)
import           Network.Tox.Crypto.Key     (PublicKey, Signature)
import qualified Network.Tox.SaveData.Util  as Util
import qualified Test.QuickCheck.Arbitrary  as Arbitrary
import           Test.QuickCheck.Arbitrary  (Arbitrary (..), genericShrink)

\end{code}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & List of conferences \\
\end{tabular}

\begin{code}

newtype Groups = Groups [Group]
    deriving (Eq, Show, Read, Generic, Arbitrary)

instance MessagePack Groups

instance Binary Groups where
    get = do
        obj <- get
        fromObject obj

    put gs = put $ toObject defaultConfig gs

\end{code}

Group:

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

data Group = Group
    { groupStateValues :: StateValues
    , groupStateBin    :: StateBin
    , groupTopicInfo   :: TopicInfo
    , groupModList     :: ModList
    , groupKeys        :: Keys
    , groupSelfInfo    :: SelfInfo
    , groupSavedPeers  :: (Int, String)
    } deriving (Eq, Show, Read, Generic)

instance MessagePack Group

instance Arbitrary Group where
    arbitrary = Group
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure (0, "")

data StateValues = StateValues
    { connectionState :: Bool
    , groupNameLen    :: Word16
    , privacyState    :: Word8
    , maxPeers        :: Word16
    , passwordLength  :: Word16
    , version         :: Word32
    , topicLock       :: Word32
    , voiceState      :: Word8
    } deriving (Eq, Show, Read, Generic)

instance MessagePack StateValues

instance Arbitrary StateValues where
    arbitrary = StateValues
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

data StateBin = StateBin
    { signature        :: Signature
    , founderPublicKey :: Signature
    , groupName        :: BS.ByteString
    , password         :: BS.ByteString
    , modListHash      :: PublicKey
    } deriving (Eq, Show, Read, Generic)

instance MessagePack StateBin

instance Arbitrary StateBin where
    arbitrary = StateBin
        <$> arbitrary
        <*> arbitrary
        <*> (BS.pack <$> (Arbitrary.vector =<< arbitrary))
        <*> (BS.pack <$> (Arbitrary.vector =<< arbitrary))
        <*> arbitrary

data TopicInfo = TopicInfo
    { topicVersion   :: Word32
    , topicLength    :: Word16
    , topicChecksum  :: Word8
    , topic          :: BS.ByteString
    , topicPublicKey :: PublicKey
    , topicSignature :: Signature
    } deriving (Eq, Show, Read, Generic)

instance MessagePack TopicInfo

instance Arbitrary TopicInfo where
    arbitrary = TopicInfo
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack <$> (Arbitrary.vector =<< arbitrary))
        <*> arbitrary
        <*> arbitrary

data ModList = ModList
    { modList :: [PublicKey]
    } deriving (Eq, Show, Read, Generic)

instance MessagePack ModList where
    toObject cfg (ModList mods) = toObject cfg $ (length mods, BS.concat $ map Sodium.encode mods)

instance Arbitrary ModList where
    arbitrary = ModList
        <$> arbitrary

data Keys = Keys
    { chatPublicKey :: Signature
    , chatSecretKey :: BS.ByteString
    , selfPublicKey :: Signature
    , selfSecretKey :: BS.ByteString
    } deriving (Eq, Show, Read, Generic)

instance MessagePack Keys

instance Arbitrary Keys where
    arbitrary = Keys
        <$> arbitrary
        <*> (BS.pack <$> Arbitrary.vector 96)
        <*> arbitrary
        <*> (BS.pack <$> Arbitrary.vector 96)

data SelfInfo = SelfInfo
    { selfNickLength :: Word16
    , selfRole       :: Word8
    , selfStatus     :: Word8
    , selfNick       :: BS.ByteString
    } deriving (Eq, Show, Read, Generic)

instance MessagePack SelfInfo

instance Arbitrary SelfInfo where
    arbitrary = do
        nick <- BS.pack <$> (Arbitrary.vector =<< arbitrary)
        SelfInfo
            <$> (pure . fromIntegral . BS.length $ nick)
            <*> arbitrary
            <*> arbitrary
            <*> pure nick

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

\end{code}
