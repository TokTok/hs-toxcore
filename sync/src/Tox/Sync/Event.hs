{-# LANGUAGE LambdaCase #-}
-- | Events: the immutable, content-addressed records all account state is
-- derived from.
module Tox.Sync.Event
  ( Event (..)
  , Content (..)
  , MessageKind (..)
  , ChatKind (..)
  , eventVersion
  , contentTag
  , encodeEvent
  , decodeEvent
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import           Data.Int                (Int64)
import           Data.Text               (Text)
import           Data.Word               (Word32, Word64)

import           Tox.Sync.Fact
import           Tox.Sync.Id
import qualified Tox.Sync.MessagePack    as MP
import           Tox.Sync.Setting

eventVersion :: Word64
eventVersion = 3

-- | Tox's message types.
data MessageKind = Normal | Action
  deriving (Eq, Show)

-- | What an invite's chat is.
data ChatKind = GroupKind | ConferenceKind
  deriving (Eq, Show)

-- | Conversation kinds, in a conversation's graph, have tags from 0, account
-- kinds from 64. A message's bytes are as Tox carries them: only clients
-- showing them decode them.
data Content
  = Received (Maybe PublicKey) Text BS.ByteString MessageKind
    -- ^ From whom in a group or conference, their name then, the message.
  | Send Text BS.ByteString MessageKind
    -- ^ Any device's "please send this", with our name then.
  | Sent EventId
    -- ^ Toxcore took the 'Send'.
  | Delivered EventId
  | Notice Text
  | File Text (Maybe BS.ByteString) Text Word64 Bool (Maybe BlobId)
    -- ^ Sender's name, Tox's file ID, file name, size, whether it comes in,
    -- and from a device the blob it asks the home device to send.
  | FileEnded EventId Bool (Maybe BlobId)
    -- ^ Whether it ended well, and then the blob the home device holds.
  | FileAnswered EventId Bool
    -- ^ A device accepts or declines a file.
  | CallStarted Text Bool Bool
    -- ^ Sender's name, incoming, video.
  | CallEnded EventId (Maybe Word64) Bool
    -- ^ How long it was connected in ms, if it was, and whether it rang here
    -- and was declined.
  | ReceivedPrivately PublicKey Text BS.ByteString MessageKind
    -- ^ A message to the account alone in a group: from whom, their name
    -- then, the message.
  | SendPrivately PublicKey Text BS.ByteString MessageKind
    -- ^ "Please send this to them alone", with our name then.
  | WantFriend PublicKey (Maybe Text) (Maybe Word32)
    -- ^ With the request message and nospam a friend request goes with.
  | UnwantFriend PublicKey
  | ReadUpTo GraphId EventId Word64
    -- ^ Read up to the event, whose rank it gives.
  | FriendRequest PublicKey Text
  | MergeChat ChatId PublicKey ChatId PublicKey Bool
  | RelayBot ChatId PublicKey Bool
  | FactContent Subject Value
  | Wish Subject Value
  | ChatInvite PublicKey ChatKind Text BS.ByteString
    -- ^ From the friend, the chat's kind, its name (none for a
    -- conference), and what Tox joins it with.
  | AnswerInvite EventId Bool (Maybe Text)
    -- ^ Join or decline, with a group's password.
  | InviteJoined EventId (Maybe ChatId) Bool
    -- ^ The chat joined, or none where Tox refused, and whether for a
    -- missing or wrong password.
  | SettingContent Scope (Kept Setting)
  | ClearSetting Scope Word32
  | Device PublicKey Text
    -- ^ One of the account's devices, by name.
  | DeviceGone PublicKey
  | NewChat ChatKind Text Privacy
    -- ^ Asks the home to found a group, with its privacy, or a conference,
    -- by name.
  | JoinChat BS.ByteString (Maybe Text)
    -- ^ Asks the home to join a public group by its chat ID and password.
  | InviteToChat ChatId PublicKey
  | ChatJoined EventId (Maybe ChatId)
    -- ^ What the home made of a 'NewChat' or 'JoinChat': the chat, or none
    -- where Tox refused.
  | UnknownContent Word64
    -- ^ From a newer version: stored and passed on, but not understood.
  deriving (Eq, Show)

contentTag :: Content -> Word64
contentTag = \case
  Received {}       -> 0
  Send {}           -> 1
  Sent _            -> 2
  Delivered _       -> 3
  Notice _          -> 4
  File {}           -> 5
  FileEnded {}      -> 6
  FileAnswered {}   -> 7
  CallStarted {}    -> 8
  CallEnded {}      -> 9
  ReceivedPrivately {} -> 10
  SendPrivately {}  -> 11
  WantFriend {}     -> 64
  UnwantFriend _    -> 65
  ReadUpTo {}       -> 66
  FriendRequest {}  -> 67
  MergeChat {}      -> 68
  RelayBot {}       -> 69
  FactContent {}    -> 70
  Wish {}           -> 71
  ChatInvite {}     -> 72
  AnswerInvite {}   -> 73
  InviteJoined {}   -> 74
  SettingContent {} -> 75
  ClearSetting {}   -> 76
  Device {}         -> 77
  DeviceGone _      -> 78
  NewChat {}        -> 79
  JoinChat {}       -> 80
  InviteToChat {}   -> 81
  ChatJoined {}     -> 82
  UnknownContent t  -> t

data Event = Event
  { eventGraph   :: GraphId
  , eventAuthor  :: PublicKey
  , eventParents :: [EventId]
  , eventRank    :: Word64
  , eventTime    :: Int64
    -- ^ Milliseconds since the Unix epoch, by the author's clock.
  , eventContent :: Content
  } deriving (Eq, Show)

key :: PublicKey -> B.Builder
key (PublicKey k) = MP.bin k

eid :: EventId -> B.Builder
eid (EventId i) = MP.bin i

blob :: Maybe BlobId -> B.Builder
blob = MP.opt . fmap (\(BlobId b) -> MP.bin b)

kindOf :: MessageKind -> B.Builder
kindOf Normal = MP.uint 0
kindOf Action = MP.uint 1

-- | @[tag, [fields]]@, or @[tag, field]@ for one with a single field.
content :: Content -> B.Builder
content c = MP.array [MP.uint (contentTag c), payload]
  where
    fields = MP.array
    payload = case c of
      Received from sender message kind ->
        fields [MP.opt (key <$> from), MP.str sender, MP.bin message, kindOf kind]
      Send sender message kind -> fields [MP.str sender, MP.bin message, kindOf kind]
      Sent i -> eid i
      Delivered i -> eid i
      Notice t -> MP.str t
      File sender fileId name size incoming b ->
        fields [MP.str sender, MP.opt (MP.bin <$> fileId), MP.str name, MP.uint size, MP.bool incoming, blob b]
      FileEnded file ok b -> fields [eid file, MP.bool ok, blob b]
      FileAnswered file accepted -> fields [eid file, MP.bool accepted]
      CallStarted sender incoming video -> fields [MP.str sender, MP.bool incoming, MP.bool video]
      CallEnded call duration declined -> fields [eid call, MP.opt (MP.uint <$> duration), MP.bool declined]
      WantFriend pk message nospam ->
        fields [key pk, MP.opt (MP.str <$> message), MP.opt (MP.uint . fromIntegral <$> nospam)]
      UnwantFriend pk -> key pk
      ReadUpTo (GraphId g) i rank -> fields [MP.bin g, eid i, MP.uint rank]
      FriendRequest pk message -> fields [key pk, MP.str message]
      MergeChat c' bot into intoBot merged -> fields [chat c', key bot, chat into, key intoBot, MP.bool merged]
      RelayBot c' bot relays -> fields [chat c', key bot, MP.bool relays]
      FactContent s v -> fields [subject s, value v]
      Wish s v -> fields [subject s, value v]
      ChatInvite from kind name invite ->
        fields [key from, MP.uint (if kind == GroupKind then 0 else 1), MP.str name, MP.bin invite]
      AnswerInvite invite join password -> fields [eid invite, MP.bool join, MP.opt (MP.str <$> password)]
      InviteJoined invite c' needsPassword -> fields [eid invite, MP.opt (chat <$> c'), MP.bool needsPassword]
      SettingContent s setting -> fields [scope s, keptSetting setting]
      ClearSetting s kind -> fields [scope s, MP.uint (fromIntegral kind)]
      ReceivedPrivately from sender message kind -> fields [key from, MP.str sender, MP.bin message, kindOf kind]
      SendPrivately to sender message kind -> fields [key to, MP.str sender, MP.bin message, kindOf kind]
      Device pk name -> fields [key pk, MP.str name]
      DeviceGone pk -> key pk
      NewChat kind name privacy ->
        fields [MP.uint (if kind == GroupKind then 0 else 1), MP.str name, MP.uint (if privacy == Public then 0 else 1)]
      JoinChat chatId password -> fields [MP.bin chatId, MP.opt (MP.str <$> password)]
      InviteToChat c' friend -> fields [chat c', key friend]
      ChatJoined intent c' -> fields [eid intent, MP.opt (chat <$> c')]
      UnknownContent _ -> error "Unknown content is not written"

getEventId :: MP.Get EventId
getEventId = EventId <$> MP.getKey

getPublicKey :: MP.Get PublicKey
getPublicKey = PublicKey <$> MP.getKey

getBlob :: MP.Fields (Maybe BlobId)
getBlob = MP.optional (BlobId <$> MP.getKey)

getKind :: MP.Get MessageKind
getKind = (\k -> if k == 1 then Action else Normal) <$> MP.getUint

getWord32 :: MP.Get Word32
getWord32 = MP.getUint >>= \n ->
  if n <= fromIntegral (maxBound :: Word32) then pure (fromIntegral n) else fail "Out of range"

getContent :: MP.Get Content
getContent = MP.variant known (pure . UnknownContent)
  where
    f = MP.field
    struct = fmap Just . f . MP.fields
    known :: Word64 -> MP.Fields (Maybe Content)
    known = \case
      0 -> struct $ Received <$> MP.optional getPublicKey <*> f MP.getStr <*> f MP.getBin <*> f getKind
      1 -> struct $ Send <$> f MP.getStr <*> f MP.getBin <*> f getKind
      2 -> Just . Sent <$> f getEventId
      3 -> Just . Delivered <$> f getEventId
      4 -> Just . Notice <$> f MP.getStr
      5 -> struct $ File <$> f MP.getStr <*> MP.optional MP.getKey <*> f MP.getStr <*> f MP.getUint
                         <*> f MP.getBool <*> getBlob
      6 -> struct $ FileEnded <$> f getEventId <*> f MP.getBool <*> getBlob
      7 -> struct $ FileAnswered <$> f getEventId <*> f MP.getBool
      8 -> struct $ CallStarted <$> f MP.getStr <*> f MP.getBool <*> f MP.getBool
      9 -> struct $ CallEnded <$> f getEventId <*> MP.optional MP.getUint <*> MP.laterFlag
      64 -> struct $ WantFriend <$> f getPublicKey <*> MP.optional MP.getStr <*> MP.optional getWord32
      65 -> Just . UnwantFriend <$> f getPublicKey
      66 -> struct $ ReadUpTo <$> f (GraphId <$> MP.getKey) <*> f getEventId <*> f MP.getUint
      67 -> struct $ FriendRequest <$> f getPublicKey <*> f MP.getStr
      68 -> struct $ MergeChat <$> f getChat <*> f getPublicKey <*> f getChat <*> f getPublicKey <*> f MP.getBool
      69 -> struct $ RelayBot <$> f getChat <*> f getPublicKey <*> f MP.getBool
      70 -> struct $ FactContent <$> f getSubject <*> f getValue
      71 -> struct $ Wish <$> f getSubject <*> f getValue
      -- A kind of chat this version doesn't know shows nothing.
      72 -> f $ MP.fields $ do
        from <- f getPublicKey
        kind <- f MP.getUint
        name <- f MP.getStr
        invite <- f MP.getBin
        pure $ case kind of
          0 -> Just (ChatInvite from GroupKind name invite)
          1 -> Just (ChatInvite from ConferenceKind name invite)
          _ -> Nothing
      73 -> struct $ AnswerInvite <$> f getEventId <*> f MP.getBool <*> MP.optional MP.getStr
      74 -> struct $ InviteJoined <$> f getEventId <*> MP.optional getChat <*> MP.laterFlag
      75 -> struct $ SettingContent <$> f getScope <*> f getKeptSetting
      76 -> struct $ ClearSetting <$> f getScope <*> f getWord32
      10 -> struct $ ReceivedPrivately <$> f getPublicKey <*> f MP.getStr <*> f MP.getBin <*> f getKind
      11 -> struct $ SendPrivately <$> f getPublicKey <*> f MP.getStr <*> f MP.getBin <*> f getKind
      77 -> struct $ Device <$> f getPublicKey <*> f MP.getStr
      78 -> Just . DeviceGone <$> f getPublicKey
      -- Of a kind or privacy this version doesn't know, it founds nothing.
      79 -> f $ MP.fields $ do
        kind <- f MP.getUint
        name <- f MP.getStr
        privacy <- f MP.getUint
        pure $ NewChat <$> lookup kind [(0, GroupKind), (1, ConferenceKind)] <*> pure name
                       <*> lookup privacy [(0, Public), (1, Private)]
      80 -> struct $ JoinChat <$> f MP.getKey <*> MP.optional MP.getStr
      81 -> struct $ InviteToChat <$> f getChat <*> f getPublicKey
      82 -> struct $ ChatJoined <$> f getEventId <*> MP.optional getChat
      _ -> pure Nothing

-- | @[version, event]@. Its hash is the event's ID.
encodeEvent :: Event -> BS.ByteString
encodeEvent e = MP.encode $ MP.array
  [ MP.uint eventVersion
  , MP.array
      [ (\(GraphId g) -> MP.bin g) (eventGraph e)
      , key (eventAuthor e)
      , MP.array (map eid (eventParents e))
      , MP.uint (eventRank e)
      , MP.sint (eventTime e)
      , content (eventContent e)
      ]
  ]

-- | Decoded exactly: its bytes name it.
decodeEvent :: BS.ByteString -> Either String Event
decodeEvent = MP.decodeExact $ MP.tuple $ do
  version <- MP.field MP.getUint
  if version /= eventVersion then fail "Unknown event version" else MP.field $ MP.fields $ do
    graph <- MP.field (GraphId <$> MP.getKey)
    author <- MP.field getPublicKey
    parents <- MP.field (MP.list getEventId)
    rank <- MP.field MP.getUint
    time <- MP.field MP.getSint
    if time < -maxTime || time > maxTime then fail "Time out of range" else
      Event graph author parents rank time <$> MP.field getContent
