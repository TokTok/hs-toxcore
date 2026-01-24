\subsection{Groups (0x07)}

This section contains a list of saved NGC groups, as a MessagePack array with
one 7-element array per group.

\begin{code}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
module Tox.Persistence.Groups where

import           Control.Monad.Validate             (refute)
import qualified Crypto.Saltine.Class               as Sodium (decode, encode)
import           Data.Binary                        (Binary (..))
import qualified Data.Binary.Get                    as Get
import qualified Data.Binary.Put                    as Put
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing,
                                                     listToMaybe)
import           Data.MessagePack                   (MessagePack (..),
                                                     defaultConfig, toObject)
import           Data.MessagePack.Arbitrary         ()
import           Data.MessagePack.Types             (Object (..), fromObject)
import           Data.Word                          (Word16, Word32, Word8)
import           GHC.Generics                       (Generic)
import qualified Test.QuickCheck.Arbitrary          as Arbitrary
import           Test.QuickCheck.Arbitrary          (Arbitrary (..),
                                                     genericShrink)
import qualified Test.QuickCheck.Gen                as Gen
import           Tox.Crypto.Core.Key                (PublicKey, Signature)
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo)
import qualified Tox.Network.Core.SocketAddress     as SocketAddress
import           Tox.Network.Core.SocketAddress     (SocketAddress)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))

\end{code}

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

Extended keys are an encryption key followed by a signing key: 64 bytes for
public keys and 96 for secret keys. A chat secret key is all zeros for anyone
but the founder.

\begin{code}

data Group = Group
    { groupStateValues :: StateValues
    , groupStateBin    :: StateBin
    , groupTopicInfo   :: TopicInfo
    , groupModList     :: ModList
    , groupKeys        :: Keys
    , groupSelfInfo    :: SelfInfo
    , groupSavedPeers  :: SavedPeers
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
        <*> arbitrary
    shrink = genericShrink

data StateValues = StateValues
    { manuallyDisconnected :: Bool
    , groupNameLen         :: Word16
    , privacyState         :: Word8
    , maxPeers             :: Word16
    , passwordLength       :: Word16
    , version              :: Word32
    , topicLock            :: Word32
    , voiceState           :: Word8
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
    shrink = genericShrink

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
    shrink = genericShrink

data TopicInfo = TopicInfo
    { topicVersion   :: Word32
    , topicLength    :: Word16
    , topicChecksum  :: Word16
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
    shrink = genericShrink

\end{code}

The moderators' signing keys, as a count and their concatenation, which is nil
when there are none.

\begin{code}

newtype ModList = ModList
    { modList :: [PublicKey]
    } deriving (Eq, Show, Read, Generic)

instance MessagePack ModList where
    toObject cfg (ModList mods) =
        toObject cfg (length mods, binOrNil . BS.concat $ map Sodium.encode mods)

    fromObjectWith cfg obj = do
        (len, catMods) <- fromObjectWith cfg obj
        case splitMods len (fromBinOrNil catMods) of
            Just mods -> return $ ModList mods
            Nothing   -> refute "mod list decoding failed"
      where
        splitMods :: Word16 -> Maybe BS.ByteString -> Maybe [PublicKey]
        splitMods 0 _ = Just []
        splitMods len catMods = do
            (modKey, rest) <- BS.splitAt 32 <$> catMods
            (:) <$> Sodium.decode modKey <*> splitMods (len - 1) (Just rest)

instance Arbitrary ModList where
    arbitrary = ModList
        <$> arbitrary
    shrink = genericShrink

binOrNil :: BS.ByteString -> Object
binOrNil bs
    | BS.null bs = ObjectNil
    | otherwise  = ObjectBin bs

fromBinOrNil :: Object -> Maybe BS.ByteString
fromBinOrNil = \case
    ObjectNil    -> Just BS.empty
    ObjectBin bs -> Just bs
    _            -> Nothing

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
    shrink = genericShrink

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
        SelfInfo (fromIntegral $ BS.length nick)
            <$> arbitrary
            <*> arbitrary
            <*> pure nick
    shrink = genericShrink

\end{code}

Saved peers are the peers to reconnect to after loading, as their packed size
and the packed peers, which are nil when there are none. Each peer is its
address, a TCP relay in packed node format, and its encryption key. Either the
address or the relay can be left out.

\begin{code}

data SavedPeer = SavedPeer
    { savedAddress   :: Maybe SocketAddress
    , savedRelay     :: Maybe NodeInfo
    , savedPublicKey :: PublicKey
    } deriving (Eq, Show, Read, Generic)

newtype SavedPeers = SavedPeers
    { savedPeers :: [SavedPeer]
    } deriving (Eq, Show, Read, Generic)

instance MessagePack SavedPeers where
    toObject cfg (SavedPeers peers) =
        let packed = LBS.toStrict . Put.runPut $ mapM_ putSavedPeer peers in
        toObject cfg (BS.length packed, binOrNil packed)

    fromObjectWith cfg obj = do
        (len, packed) <- fromObjectWith cfg obj
        case fromBinOrNil packed of
            Just bs | BS.length bs == fromIntegral (len :: Word16) -> SavedPeers <$> getSavedPeers bs
            _ -> refute "saved peers decoding failed"
      where
        getSavedPeers = maybe (refute "saved peers decoding failed") return . parseSavedPeers

putSavedPeer :: SavedPeer -> Put.Put
putSavedPeer (SavedPeer addr relay pk) = do
    mapM_ (SocketAddress.putSocketAddress UDP) addr
    mapM_ put relay
    put pk

\end{code}

Nothing marks which parts a peer has, so we read them as toxcore does: an
address if there is one, then a relay if there is one, then the key. A key
that starts with a TCP address family reads as a relay, which toxcore gets
wrong too.

\begin{code}

parseSavedPeers :: BS.ByteString -> Maybe [SavedPeer]
parseSavedPeers bs = from 0
  where
    end = BS.length bs
    from i = readings Map.! i
    readings = Map.fromList [ (i, firstReading i) | i <- [0 .. end] ]
    firstReading i
      | i == end = Just []
      | otherwise = listToMaybe
          [ SavedPeer addr relay pk : rest
          | (addr, afterAddr) <- optionalPart getAddress i
          , (relay, afterRelay) <- optionalPart getRelay afterAddr
          , isJust addr || isJust relay
          , Just (pk, next) <- [run get afterRelay]
          , Just rest <- [from next]
          ]
    getAddress = do
        (protocol, addr) <- SocketAddress.getSocketAddress
        if protocol == UDP then return addr else fail "not a UDP address"
    getRelay = do
        relay <- get
        if NodeInfo.protocol relay == TCP then return relay else fail "not a TCP relay"
    optionalPart g i = [ (Just x, next) | Just (x, next) <- [run g i] ] ++ [(Nothing, i)]
    run :: Get.Get a -> Int -> Maybe (a, Int)
    run g i = case Get.runGetOrFail g (LBS.fromStrict $ BS.drop i bs) of
        Right (_, used, x) -> Just (x, i + fromIntegral used)
        Left _             -> Nothing

instance Arbitrary SavedPeers where
    arbitrary = SavedPeers <$> Gen.listOf (do
        let tcpRelay = (\ni -> ni { NodeInfo.protocol = TCP }) <$> arbitrary
        (addr, relay) <- Gen.oneof
            [ (,) . Just <$> arbitrary <*> Gen.oneof [pure Nothing, Just <$> tcpRelay]
            , (,) Nothing . Just <$> tcpRelay
            ]
        let readsAsRelay pk = isNothing relay && BS.head (Sodium.encode pk) `elem` [130, 138]
        SavedPeer addr relay <$> arbitrary `Gen.suchThat` (not . readsAsRelay))
    shrink (SavedPeers peers) = SavedPeers <$> Arbitrary.shrinkList (const []) peers

\end{code}
