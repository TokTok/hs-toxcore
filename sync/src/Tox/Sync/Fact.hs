{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Facts: what Tox says about the account, its friends, its chats and the
-- people in them, as values of subjects. A wish is a fact a device wants to
-- be true.
module Tox.Sync.Fact
  ( Subject (..)
  , Value (..)
  , UserStatus (..)
  , Connection (..)
  , Role (..)
  , Privacy (..)
  , VoiceState (..)
  , CallState (..)
  , valueKind
  , isKept
  , isWishable
  , subject
  , value
  , getSubject
  , getValue
    -- * Sets of facts
  , Facts
  , noFacts
  , factsOf
  , allFacts
  , setFact
  , updateFacts
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Text               (Text)
import           Data.Word               (Word32, Word64)

import           Tox.Sync.Id
import qualified Tox.Sync.MessagePack    as MP

data Subject
  = AccountSubject
  | FriendSubject PublicKey
  | ChatSubject ChatId
  | PeerSubject ChatId PublicKey
    -- ^ Someone in a chat.
  deriving (Eq, Ord, Show)

data UserStatus = Online | Away | Busy
  deriving (Eq, Show, Enum, Bounded)

data Connection = NoConnection | Tcp | Udp
  deriving (Eq, Show, Enum, Bounded)

data Role = Founder | Moderator | User | Observer
  deriving (Eq, Show, Enum, Bounded)

data Privacy = Public | Private
  deriving (Eq, Show, Enum, Bounded)

data VoiceState = VoiceEveryone | VoiceModerators | VoiceFounder
  deriving (Eq, Show, Enum, Bounded)

-- | A friend's call, as the home device has it.
data CallState
  = NoCall
  | Ringing Bool
    -- ^ At the home device, for a device to answer, with video or not.
  | Answered
    -- ^ Going on, through one of the account's devices.
  deriving (Eq, Show)

-- | A subject has at most one value of each kind, its tag on the wire.
data Value
  = Name Text
  | StatusMessage Text
  | Status UserStatus
  | ConnectionValue Connection
  | Typing Bool
  | Topic Text
  | RoleValue Role
  | PrivacyValue Privacy
  | PeerLimit Word32
  | Password Text
    -- ^ Empty for none.
  | TopicLock Bool
  | VoiceStateValue VoiceState
  | Joined Bool
  | SelfName Text
    -- ^ Our name in a chat, where it has its own.
  | Present Bool
  | Address BS.ByteString
  | CallValue CallState
  | CallRelay Bool
    -- ^ The home device connects devices to the account's calls.
  | SelfKey PublicKey
    -- ^ Our key in a group, which is its own.
  | Avatar (Maybe BlobId)
    -- ^ None for no avatar.
  | UnknownValue Word64
    -- ^ From a newer version, or with a Tox value it doesn't know.
  deriving (Eq, Show)

valueKind :: Value -> Word64
valueKind = \case
  Name _            -> 0
  StatusMessage _   -> 1
  Status _          -> 2
  ConnectionValue _ -> 3
  Typing _          -> 4
  Topic _           -> 5
  RoleValue _       -> 6
  PrivacyValue _    -> 7
  PeerLimit _       -> 8
  Password _        -> 9
  TopicLock _       -> 10
  VoiceStateValue _ -> 11
  Joined _          -> 12
  SelfName _        -> 13
  Present _         -> 14
  Address _         -> 15
  CallValue _       -> 16
  CallRelay _       -> 17
  SelfKey _         -> 18
  Avatar _          -> 19
  UnknownValue kind -> kind

-- | Facts are kept as events where they change seldom enough to be history.
-- The rest are live, sent only within a session.
isKept :: Subject -> Value -> Bool
isKept s v = case (s, v) of
  (PeerSubject _ _, _)        -> False
  (_, ConnectionValue _)      -> False
  (_, Typing _)               -> False
  (_, Present _)              -> False
  (_, CallValue _)            -> False
  (_, CallRelay _)            -> False
  (FriendSubject _, Status _) -> False
  _                           -> True

-- | Whether a device may wish for a value: the home device can make Tox
-- match it.
isWishable :: Subject -> Value -> Bool
isWishable s v = case (s, v) of
  (AccountSubject, Name _)                       -> True
  (AccountSubject, StatusMessage _)              -> True
  (AccountSubject, Status _)                     -> True
  (AccountSubject, Avatar _)                     -> True
  (ChatSubject (ConferenceChat _), Name _)       -> True
  (ChatSubject (ConferenceChat _), Joined False) -> True
  (ChatSubject (GroupChat _), Topic _)           -> True
  (ChatSubject (GroupChat _), PrivacyValue _)    -> True
  (ChatSubject (GroupChat _), PeerLimit _)       -> True
  (ChatSubject (GroupChat _), Password _)        -> True
  (ChatSubject (GroupChat _), TopicLock _)       -> True
  (ChatSubject (GroupChat _), VoiceStateValue _) -> True
  (ChatSubject (GroupChat _), Joined False)      -> True
  (ChatSubject (GroupChat _), SelfName _)        -> True
  (PeerSubject (GroupChat _) _, RoleValue _)     -> True
  (PeerSubject (GroupChat _) _, Present False)   -> True
  _                                              -> False

-- | The account is a bare tag, other subjects are @[tag, payload]@.
subject :: Subject -> B.Builder
subject = \case
  AccountSubject -> MP.uint 0
  FriendSubject (PublicKey pk) -> MP.array [MP.uint 1, MP.bin pk]
  ChatSubject c -> MP.array [MP.uint 2, chat c]
  PeerSubject c (PublicKey pk) -> MP.array [MP.uint 3, MP.array [chat c, MP.bin pk]]

-- | @[kind, value]@. Unknown values are never written: stores keep the
-- bytes they received.
value :: Value -> B.Builder
value v = MP.array [MP.uint (valueKind v), payload]
  where
    enum :: Enum e => e -> B.Builder
    enum = MP.uint . fromIntegral . fromEnum
    payload = case v of
      Name t                    -> MP.str t
      StatusMessage t           -> MP.str t
      Status s                  -> enum s
      ConnectionValue c         -> enum c
      Typing b                  -> MP.bool b
      Topic t                   -> MP.str t
      RoleValue r               -> enum r
      PrivacyValue p            -> enum p
      PeerLimit n               -> MP.uint (fromIntegral n)
      Password t                -> MP.str t
      TopicLock b               -> MP.bool b
      VoiceStateValue s         -> enum s
      Joined b                  -> MP.bool b
      SelfName t                -> MP.str t
      Present b                 -> MP.bool b
      Address a                 -> MP.bin a
      CallValue NoCall          -> MP.uint 0
      CallValue (Ringing video) -> MP.array [MP.uint 1, MP.bool video]
      CallValue Answered        -> MP.uint 2
      CallRelay b               -> MP.bool b
      SelfKey (PublicKey pk)    -> MP.bin pk
      Avatar blob               -> MP.opt ((\(BlobId b) -> MP.bin b) <$> blob)
      UnknownValue _            -> error "Unknown values are not written"

getSubject :: MP.Get Subject
getSubject = MP.variant known unknown
  where
    known = \case
      1 -> Just . FriendSubject . PublicKey <$> MP.field MP.getKey
      2 -> Just . ChatSubject <$> MP.field getChat
      3 -> Just <$> MP.field (MP.fields (PeerSubject <$> MP.field getChat <*> (PublicKey <$> MP.field MP.getKey)))
      _ -> pure Nothing
    unknown 0 = pure AccountSubject
    unknown _ = fail "Unknown subject"

-- | Tox's value of an enum, or nothing for one this version doesn't know,
-- as one a newer version gave a payload.
toxValue :: forall e. (Enum e, Bounded e) => MP.Fields (Maybe e)
toxValue = MP.field (MP.variant (const (Just Nothing <$ MP.end)) (pure . known))
  where
    known n
      | n <= fromIntegral (fromEnum (maxBound :: e)) = Just (toEnum (fromIntegral n))
      | otherwise = Nothing

getValue :: MP.Get Value
getValue = MP.variant known (pure . UnknownValue)
  where
    known :: Word64 -> MP.Fields (Maybe Value)
    known = \case
      0 -> Just . Name <$> MP.field MP.getStr
      1 -> Just . StatusMessage <$> MP.field MP.getStr
      2 -> fmap Status <$> toxValue
      3 -> fmap ConnectionValue <$> toxValue
      4 -> Just . Typing <$> MP.field MP.getBool
      5 -> Just . Topic <$> MP.field MP.getStr
      6 -> fmap RoleValue <$> toxValue
      7 -> fmap PrivacyValue <$> toxValue
      8 -> MP.field MP.getUint >>= \n ->
        if n <= fromIntegral (maxBound :: Word32) then pure (Just (PeerLimit (fromIntegral n))) else fail "Peer limit out of range"
      9 -> Just . Password <$> MP.field MP.getStr
      10 -> Just . TopicLock <$> MP.field MP.getBool
      11 -> fmap VoiceStateValue <$> toxValue
      12 -> Just . Joined <$> MP.field MP.getBool
      13 -> Just . SelfName <$> MP.field MP.getStr
      14 -> Just . Present <$> MP.field MP.getBool
      15 -> Just . Address <$> MP.field MP.getBin
      16 -> fmap CallValue <$> MP.field getCall
      17 -> Just . CallRelay <$> MP.field MP.getBool
      18 -> Just . SelfKey . PublicKey <$> MP.field MP.getKey
      19 -> Just . Avatar <$> MP.optional (BlobId <$> MP.getKey)
      _ -> pure Nothing
    getCall = MP.variant
      (\case
        1 -> Just . Just . Ringing <$> MP.field MP.getBool
        _ -> pure Nothing)
      (\case
        0 -> pure (Just NoCall)
        2 -> pure (Just Answered)
        _ -> pure Nothing)

-- | What each subject has, of each kind of value.
newtype Facts = Facts (Map Subject (Map Word64 Value))
  deriving (Eq, Show)

noFacts :: Facts
noFacts = Facts Map.empty

factsOf :: Subject -> Facts -> [Value]
factsOf s (Facts m) = maybe [] Map.elems (Map.lookup s m)

allFacts :: Facts -> [(Subject, Value)]
allFacts (Facts m) = [(s, v) | (s, vs) <- Map.toList m, v <- Map.elems vs]

setFact :: Subject -> Value -> Facts -> Facts
setFact s v (Facts m) = Facts (Map.insertWith Map.union s (Map.singleton (valueKind v) v) m)

-- | Takes in live facts: @Present False@ drops a subject.
updateFacts :: [(Subject, Value)] -> Facts -> Facts
updateFacts updates facts = foldl step facts updates
  where
    step (Facts m) (s, Present False) = Facts (Map.delete s m)
    step f (s, v)                     = setFact s v f
