\chapter{State Format}

\begin{code}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Network.Tox.SaveData
    ( SaveData (..)
    , Section (..)
    , NospamKeys (..)
    , Friends (..)
    , Bytes (..)
    ) where
\end{code}

The reference Tox implementation uses a custom binary format to save the state
of a Tox client between restarts. This format is far from perfect and will be
replaced eventually. For the sake of maintaining compatibility down the road,
it is documented here.

The binary encoding of all integer types in the state format is a fixed-width
byte sequence with the integer encoded in Little Endian unless stated otherwise.

\begin{code}

import           Control.Arrow                    (second)
import           Control.Monad                    (when)
import           Data.Binary                      (Binary (..))
import           Data.Binary.Get                  (Get)
import qualified Data.Binary.Get                  as Get
import           Data.Binary.Put                  (Put)
import qualified Data.Binary.Put                  as Put
import qualified Data.ByteString.Lazy             as LBS
import           Data.MessagePack                 (MessagePack)
import           Data.Word                        (Word16, Word32, Word8)
import           GHC.Generics                     (Generic)
import           Network.Tox.Crypto.Key           (PublicKey, SecretKey)
import           Network.Tox.Crypto.KeyPair       (KeyPair (..))
import qualified Network.Tox.Crypto.KeyPair       as KeyPair
import           Network.Tox.SaveData.Bytes       (Bytes)
import           Network.Tox.SaveData.Conferences (Conferences)
import           Network.Tox.SaveData.DHT         (DHT)
import           Network.Tox.SaveData.Friend      (Friend)
import           Network.Tox.SaveData.Groups      (Groups)
import           Network.Tox.SaveData.Nodes       (Nodes)
import qualified Network.Tox.SaveData.Util        as Util
import           Test.QuickCheck.Arbitrary        (Arbitrary (..),
                                                   genericShrink)
import qualified Test.QuickCheck.Gen              as Gen

\end{code}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{4}    & Zeroes \\
  \texttt{4}    & \texttt{uint32\_t} (0x15ED1B1F) \\
  \texttt{?}    & List of sections \\
\end{tabular}

\begin{code}

saveDataMagic :: Word32
saveDataMagic = 0x15ED1B1F

newtype SaveData = SaveData [Section]
    deriving (Eq, Show, Read, Generic)

instance MessagePack SaveData

instance Binary SaveData where
    get = do
        zeroes <- Get.getWord32le
        when (zeroes /= 0) $
            fail $ "savedata should start with 32 zero-bits, but got "
                ++ show zeroes

        magic <- Get.getWord32le
        when (magic /= saveDataMagic) $
            fail $ "wrong magic number for savedata: "
                ++ show magic ++ " != " ++ show saveDataMagic

        SaveData <$> getSections

    put (SaveData sections) = do
        Put.putWord32le 0
        Put.putWord32le saveDataMagic
        putSections sections

instance Arbitrary SaveData where
    arbitrary = SaveData . (++ [SectionEOF]) <$> arbitrary
    shrink    = filter (\(SaveData ss) -> SectionEOF `elem` ss) . genericShrink

\end{code}

\section{Sections}

The core of the state format consists of a list of sections. Every section has
its type and length specified at the beginning. In some cases, a section only
contains one item and thus takes up the entire length of the section. This is
denoted with '?'.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{4}    & \texttt{uint32\_t} Length of this section \\
  \texttt{2}    & \texttt{uint16\_t} Section type \\
  \texttt{2}    & \texttt{uint16\_t} (0x01CE) \\
  \texttt{?}    & Section \\
\end{tabular}

\begin{code}

sectionMagic :: Word16
sectionMagic = 0x01CE

\end{code}

Section types:

\begin{tabular}{l|l}
  Name          & Value \\
  \hline
  NospamKeys    & 0x01 \\
  DHT           & 0x02 \\
  Friends       & 0x03 \\
  Name          & 0x04 \\
  StatusMessage & 0x05 \\
  Status        & 0x06 \\
  Groups        & 0x07 \\
  TcpRelays     & 0x0A \\
  PathNodes     & 0x0B \\
  Conferences   & 0x14 \\
  EOF           & 0xFF \\
\end{tabular}

\begin{code}

getSections :: Get [Section]
getSections = go
  where
    go = do
        (len, ty) <- Util.getSectionHeader sectionMagic

        let load f = (:) <$> (f <$> Get.isolate (fromIntegral len) get) <*> go

        case ty of
            0x01 -> load SectionNospamKeys
            0x02 -> load SectionDHT
            0x03 -> load SectionFriends
            0x04 -> load SectionName
            0x05 -> load SectionStatusMessage
            0x06 -> load SectionStatus
            0x07 -> load SectionGroups
            0x0A -> load SectionTcpRelays
            0x0B -> load SectionPathNodes
            0x14 -> load SectionConferences
            0xFF -> return [SectionEOF]
            _    -> fail $ show ty

putSections :: [Section] -> Put
putSections = mapM_ go
  where
    go section = do
        let (ty, bytes) = second Put.runPut $ putSection section

        Util.putSectionHeader sectionMagic (fromIntegral $ LBS.length bytes) ty
        Put.putLazyByteString bytes

    putSection = \case
        SectionNospamKeys    x -> (0x01, put x)
        SectionDHT           x -> (0x02, put x)
        SectionFriends       x -> (0x03, put x)
        SectionName          x -> (0x04, put x)
        SectionStatusMessage x -> (0x05, put x)
        SectionStatus        x -> (0x06, put x)
        SectionGroups        x -> (0x07, put x)
        SectionTcpRelays     x -> (0x0A, put x)
        SectionPathNodes     x -> (0x0B, put x)
        SectionConferences   x -> (0x14, put x)
        SectionEOF             -> (0xFF, return ())

\end{code}

Not every section listed above is required to be present in order to restore
from a state file. Only NospamKeys is required.

\subsection{Nospam and Keys (0x01)}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{4}    & \texttt{uint32\_t} Nospam \\
  \texttt{32}   & Long term public key \\
  \texttt{32}   & Long term secret key \\
\end{tabular}

\input{src/Network/Tox/SaveData/DHT.lhs}

\subsection{Friends (0x03)}

This section contains a list of friends. A friend can either be a peer we've
sent a friend request to or a peer we've accepted a friend request from.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & List of friends \\
\end{tabular}

\input{src/Network/Tox/SaveData/Friend.lhs}

\subsection{Name (0x04)}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & Name as a UTF-8 encoded string \\
\end{tabular}

\subsection{Status Message (0x05)}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & Status message as a UTF-8 encoded string \\
\end{tabular}

\subsection{Status (0x06)}

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{1}    & \texttt{uint8\_t} User status (see also: \texttt{USERSTATUS}) \\
\end{tabular}

\subsection{Tcp Relays (0x0A)}

This section contains a list of TCP relays.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & List of TCP relays \\
\end{tabular}

The structure of a TCP relay is the same as \texttt{Node Info}. Note: this
means that the integers stored in these nodes are stored in Big Endian as well.

\subsection{Path Nodes (0x0B)}

This section contains a list of path nodes used for onion routing.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & List of path nodes \\
\end{tabular}

The structure of a path node is the same as \texttt{Node Info}. Note: this
means that the integers stored in these nodes are stored in Big Endian as well.

\input{src/Network/Tox/SaveData/Conferences.lhs}

\subsection{EOF (0xFF)}

This section indicates the end of the state file. This section doesn't have any
content and thus its length is 0.

\begin{code}

data Section
    = SectionNospamKeys NospamKeys
    | SectionDHT DHT
    | SectionFriends Friends
    | SectionName Bytes
    | SectionStatusMessage Bytes
    | SectionStatus Word8
    | SectionGroups Groups
    | SectionTcpRelays Nodes
    | SectionPathNodes Nodes
    | SectionConferences Conferences
    | SectionEOF
    deriving (Eq, Show, Read, Generic)

instance MessagePack Section

instance Arbitrary Section where
    arbitrary = Gen.oneof
        [ SectionNospamKeys <$> arbitrary
        , SectionDHT <$> arbitrary
        , SectionFriends <$> arbitrary
        , SectionName <$> arbitrary
        , SectionStatusMessage <$> arbitrary
        , SectionStatus <$> arbitrary
        , SectionGroups <$> arbitrary
        , SectionTcpRelays <$> arbitrary
        , SectionPathNodes <$> arbitrary
        , SectionConferences <$> arbitrary
        ]
    shrink = genericShrink

data NospamKeys = NospamKeys
    { nospam    :: Word32
    , publicKey :: PublicKey
    , secretKey :: SecretKey
    }
    deriving (Eq, Show, Read, Generic)

instance MessagePack NospamKeys

instance Binary NospamKeys where
    get = NospamKeys
        <$> Get.getWord32le
        <*> get
        <*> get

    put NospamKeys{..} = do
        Put.putWord32le nospam
        put publicKey
        put secretKey

instance Arbitrary NospamKeys where
    arbitrary = do
        KeyPair sk pk <- KeyPair.fromSecretKey <$> arbitrary
        NospamKeys
            <$> arbitrary
            <*> pure pk
            <*> pure sk
    shrink = genericShrink

newtype Friends = Friends [Friend]
    deriving (Eq, Show, Read, Generic)

instance MessagePack Friends

instance Binary Friends where
    get = Friends <$> Util.getList
    put (Friends xs) = mapM_ put xs

instance Arbitrary Friends where
    arbitrary = Friends <$> arbitrary
    shrink = genericShrink

\end{code}
