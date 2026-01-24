{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Runs the cases in test/conformance, rs-toxcore-c's sync/conformance as
-- tools/update-sync-conformance copies them, so this library encodes,
-- decodes and syncs as every other implementation does.
module Tox.Sync.ConformanceSpec (spec) where

import           Control.Monad          (forM_, unless)
import           Data.Aeson             (Value (..), (.=))
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KeyMap
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy   as BL
import           Data.Either            (fromRight)
import           Data.Int               (Int64)
import           Data.IORef
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isJust, mapMaybe)
import           Data.Scientific        (toBoundedInteger)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Vector
import           Test.Hspec             hiding (after, before)

import           Tox.Sync.Account
import           Tox.Sync.Event
import qualified Tox.Sync.Fact          as Fact
import           Tox.Sync.Fact          hiding (Value)
import           Tox.Sync.Frame
import           Tox.Sync.Id
import           Tox.Sync.Pairing
import           Tox.Sync.Protocol
import           Tox.Sync.Replica
import           Tox.Sync.Session
import           Tox.Sync.Setting
import           Tox.Sync.Store

-- | More than any session takes.
maxRounds :: Int
maxRounds = 1000

spec :: Spec
spec = do
  cases "codec" codec
  cases "state" state
  cases "store" store'
  cases "session" session

-- What a case found wrong, by what it checked.
type Check = IORef [String]

cases :: String -> (Value -> [(Text, Check -> IO ())]) -> Spec
cases file run = describe file $ runIO (Aeson.eitherDecodeFileStrict ("test/conformance/" <> file <> ".json")) >>= \case
  Left e -> it "reads" (expectationFailure e)
  Right json -> forM_ (run json) $ \(name, test) -> it (Text.unpack name) $ do
    failures <- newIORef []
    test failures
    readIORef failures >>= \fs -> unless (null fs) (expectationFailure (unlines (reverse fs)))

check :: Check -> String -> Value -> Value -> IO ()
check failures what expected actual = unless (expected == actual) $
  modifyIORef failures (\fs -> (what <> "\n  expected " <> shown expected <> "\n  actual   " <> shown actual) : fs)
  where shown = Text.unpack . Text.decodeUtf8 . BL.toStrict . Aeson.encode

sorted :: [Value] -> Value
sorted = Array . Vector.fromList . sortOn Aeson.encode

-- JSON access

(!) :: Value -> Text -> Value
Object o ! k = fromMaybe Null (KeyMap.lookup (Key.fromText k) o)
_ ! _        = Null

items :: Value -> [Value]
items (Array a) = Vector.toList a
items _         = []

text :: Value -> Text
text (String t) = t
text v          = error ("Not text: " <> show v)

int :: (Integral a, Bounded a) => Value -> a
int (Number n) | Just i <- toBoundedInteger n = i
int v = error ("Not an integer: " <> show v)

flag :: Value -> Bool
flag (Bool b) = b
flag v        = error ("Not a flag: " <> show v)

bytes :: Value -> BS.ByteString
bytes v = either error id (Base16.decode (Text.encodeUtf8 (text v)))

hex :: BS.ByteString -> Value
hex = String . Text.decodeUtf8 . Base16.encode

optional' :: (Value -> a) -> Value -> Maybe a
optional' _ Null = Nothing
optional' f v    = Just (f v)

-- | A fieldless tag or a one-key object.
variant :: Value -> (Text, Value)
variant (String t) = (t, Null)
variant (Object o) | [(k, v)] <- KeyMap.toList o = (Key.toText k, v)
variant v = error ("Not a variant: " <> show v)

tagged :: Text -> Value -> Value
tagged k v = Aeson.object [Key.fromText k .= v]

obj :: [(Text, Value)] -> Value
obj = Aeson.object . map (\(k, v) -> Key.fromText k .= v)

-- Values to and from JSON

pk :: Value -> PublicKey
pk = PublicKey . bytes

pkJson :: PublicKey -> Value
pkJson (PublicKey k) = hex k

eventId :: Value -> EventId
eventId = EventId . bytes

idJson :: EventId -> Value
idJson (EventId i) = hex i

ids :: Value -> [EventId]
ids = map eventId . items

idsJson :: [EventId] -> Value
idsJson = Array . Vector.fromList . map idJson

graph :: Value -> GraphId
graph = GraphId . bytes

graphJson :: GraphId -> Value
graphJson (GraphId g) = hex g

blobJson :: Maybe BlobId -> Value
blobJson = maybe Null (\(BlobId b) -> hex b)

blob :: Value -> Maybe BlobId
blob = optional' (BlobId . bytes)

chatOf :: Value -> ChatId
chatOf v = case variant v of
  ("group", i)      -> GroupChat (bytes i)
  ("conference", i) -> ConferenceChat (bytes i)
  other             -> error ("Not a chat: " <> show other)

chatJson :: ChatId -> Value
chatJson (GroupChat i)      = tagged "group" (hex i)
chatJson (ConferenceChat i) = tagged "conference" (hex i)

subjectOf :: Value -> Subject
subjectOf v = case variant v of
  ("account", _) -> AccountSubject
  ("friend", k)  -> FriendSubject (pk k)
  ("chat", c)    -> ChatSubject (chatOf c)
  ("peer", p)    -> PeerSubject (chatOf (p ! "chat")) (pk (p ! "pk"))
  other          -> error ("Not a subject: " <> show other)

subjectJson :: Subject -> Value
subjectJson = \case
  AccountSubject  -> "account"
  FriendSubject k -> tagged "friend" (pkJson k)
  ChatSubject c   -> tagged "chat" (chatJson c)
  PeerSubject c k -> tagged "peer" (obj [("chat", chatJson c), ("pk", pkJson k)])

-- | Tox's values by their names, in order.
named :: (Enum e, Bounded e) => [Text] -> Value -> e
named names v = case lookup (text v) (zip names [minBound ..]) of
  Just e  -> e
  Nothing -> error ("Not a name: " <> show v)

nameOf :: Enum e => [Text] -> e -> Value
nameOf names e = String (names !! fromEnum e)

statuses, connections, roles, privacies, voiceStates, encodings :: [Text]
statuses = ["online", "away", "busy"]
connections = ["none", "tcp", "udp"]
roles = ["founder", "moderator", "user", "observer"]
privacies = ["public", "private"]
voiceStates = ["everyone", "moderators", "founder"]
encodings = ["original", "qoi"]

valueOf :: Value -> Fact.Value
valueOf j = case variant j of
  ("name", v) -> Name (text v)
  ("status_message", v) -> StatusMessage (text v)
  ("status", v) -> Status (named statuses v)
  ("connection", v) -> ConnectionValue (named connections v)
  ("typing", v) -> Typing (flag v)
  ("topic", v) -> Topic (text v)
  ("role", v) -> RoleValue (named roles v)
  ("privacy", v) -> PrivacyValue (named privacies v)
  ("peer_limit", v) -> PeerLimit (int v)
  ("password", v) -> Password (text v)
  ("topic_lock", v) -> TopicLock (flag v)
  ("voice_state", v) -> VoiceStateValue (named voiceStates v)
  ("joined", v) -> Joined (flag v)
  ("self_name", v) -> SelfName (text v)
  ("present", v) -> Present (flag v)
  ("address", v) -> Address (bytes v)
  ("call", v) -> CallValue $ case variant v of
    ("none", _)     -> NoCall
    ("ringing", r)  -> Ringing (flag (r ! "video"))
    ("answered", _) -> Answered
    other           -> error ("Not a call: " <> show other)
  ("call_relay", v) -> CallRelay (flag v)
  ("self_key", v) -> SelfKey (pk v)
  ("avatar", v) -> Avatar (blob v)
  other -> error ("Not a value: " <> show other)

valueJson :: Fact.Value -> Value
valueJson = \case
  Name t -> tagged "name" (String t)
  StatusMessage t -> tagged "status_message" (String t)
  Status s -> tagged "status" (nameOf statuses s)
  ConnectionValue c -> tagged "connection" (nameOf connections c)
  Typing b -> tagged "typing" (Bool b)
  Topic t -> tagged "topic" (String t)
  RoleValue r -> tagged "role" (nameOf roles r)
  PrivacyValue p -> tagged "privacy" (nameOf privacies p)
  PeerLimit n -> tagged "peer_limit" (Aeson.toJSON n)
  Password t -> tagged "password" (String t)
  TopicLock b -> tagged "topic_lock" (Bool b)
  VoiceStateValue s -> tagged "voice_state" (nameOf voiceStates s)
  Joined b -> tagged "joined" (Bool b)
  SelfName t -> tagged "self_name" (String t)
  Present b -> tagged "present" (Bool b)
  Address a -> tagged "address" (hex a)
  CallValue c -> tagged "call" $ case c of
    NoCall    -> "none"
    Ringing v -> tagged "ringing" (obj [("video", Bool v)])
    Answered  -> "answered"
  CallRelay b -> tagged "call_relay" (Bool b)
  SelfKey k -> tagged "self_key" (pkJson k)
  Avatar b -> tagged "avatar" (blobJson b)
  UnknownValue k -> tagged "unknown" (Aeson.toJSON k)

factJson :: (Subject, Fact.Value) -> Value
factJson (s, v) = Array (Vector.fromList [subjectJson s, valueJson v])

factOf :: Value -> (Subject, Fact.Value)
factOf v = case items v of
  [s, x] -> (subjectOf s, valueOf x)
  _      -> error ("Not a fact: " <> show v)

scopeOf :: Value -> Scope
scopeOf v = case variant v of
  ("account", _)      -> AccountScope
  ("conversation", g) -> ConversationScope (graph g)
  other               -> error ("Not a scope: " <> show other)

scopeJson :: Scope -> Value
scopeJson = \case
  AccountScope        -> "account"
  ConversationScope g -> tagged "conversation" (graphJson g)
  UnknownScope t      -> tagged "unknown" (Aeson.toJSON t)

settingOf :: Value -> Setting
settingOf v = case variant v of
  ("mute", m)           -> Mute (optional' int (m ! "until")) (flag (m ! "mentions"))
  ("picture_format", f) -> PictureFormat (named encodings f)
  other                 -> error ("Not a setting: " <> show other)

settingJson :: Setting -> Value
settingJson = \case
  Mute end mentions -> tagged "mute" (obj [("until", maybe Null Aeson.toJSON end), ("mentions", Bool mentions)])
  PictureFormat f -> tagged "picture_format" (nameOf encodings f)
  UnknownSetting k -> tagged "unknown" (Aeson.toJSON k)

kindOf :: Value -> MessageKind
kindOf "action" = Action
kindOf "normal" = Normal
kindOf v        = error ("Not a message kind: " <> show v)

kindJson :: MessageKind -> Value
kindJson Action = "action"
kindJson Normal = "normal"

chatKindOf :: Value -> ChatKind
chatKindOf "group"      = GroupKind
chatKindOf "conference" = ConferenceKind
chatKindOf v            = error ("Not a chat kind: " <> show v)

chatKindJson :: ChatKind -> Value
chatKindJson GroupKind      = "group"
chatKindJson ConferenceKind = "conference"

contentOf :: Value -> Content
contentOf j = case variant j of
  ("received", c) -> Received (optional' pk (c ! "from")) (text (c ! "sender")) (bytes (c ! "message")) (kindOf (c ! "kind"))
  ("send", c) -> Send (text (c ! "sender")) (bytes (c ! "message")) (kindOf (c ! "kind"))
  ("sent", c) -> Sent (eventId (c ! "intent"))
  ("delivered", c) -> Delivered (eventId (c ! "intent"))
  ("notice", c) -> Notice (text (c ! "text"))
  ("file", c) -> File (text (c ! "sender")) (optional' bytes (c ! "file_id")) (text (c ! "name")) (int (c ! "size"))
                   (flag (c ! "incoming")) (blob (c ! "blob"))
  ("file_ended", c) -> FileEnded (eventId (c ! "file")) (flag (c ! "ok")) (blob (c ! "blob"))
  ("file_answered", c) -> FileAnswered (eventId (c ! "file")) (flag (c ! "accepted"))
  ("call", c) -> CallStarted (text (c ! "sender")) (flag (c ! "incoming")) (flag (c ! "video"))
  ("call_ended", c) -> CallEnded (eventId (c ! "call")) (optional' int (c ! "duration")) (flag (c ! "declined"))
  ("want_friend", c) -> WantFriend (pk (c ! "pk")) (optional' text (c ! "request_message")) (optional' int (c ! "nospam"))
  ("unwant_friend", c) -> UnwantFriend (pk (c ! "pk"))
  ("read_up_to", c) -> ReadUpTo (graph (c ! "graph")) (eventId (c ! "event")) (int (c ! "rank"))
  ("friend_request", c) -> FriendRequest (pk (c ! "pk")) (text (c ! "message"))
  ("merge_chat", c) -> MergeChat (chatOf (c ! "chat")) (pk (c ! "bot")) (chatOf (c ! "into")) (pk (c ! "into_bot")) (flag (c ! "merged"))
  ("relay_bot", c) -> RelayBot (chatOf (c ! "chat")) (pk (c ! "bot")) (flag (c ! "relays"))
  ("fact", c) -> FactContent (subjectOf (c ! "subject")) (valueOf (c ! "value"))
  ("want", c) -> Wish (subjectOf (c ! "subject")) (valueOf (c ! "value"))
  ("chat_invite", c) -> ChatInvite (pk (c ! "from")) (chatKindOf (c ! "kind")) (text (c ! "name")) (bytes (c ! "invite"))
  ("answer_invite", c) -> AnswerInvite (eventId (c ! "invite")) (flag (c ! "join")) (optional' text (c ! "password"))
  ("invite_joined", c) -> InviteJoined (eventId (c ! "invite")) (optional' chatOf (c ! "chat")) (flag (c ! "needs_password"))
  ("setting", c) -> SettingContent (scopeOf (c ! "scope")) (kept (settingOf (c ! "setting")))
  ("clear_setting", c) -> ClearSetting (scopeOf (c ! "scope")) (int (c ! "kind"))
  ("received_privately", c) -> ReceivedPrivately (pk (c ! "from")) (text (c ! "sender")) (bytes (c ! "message")) (kindOf (c ! "kind"))
  ("send_privately", c) -> SendPrivately (pk (c ! "to")) (text (c ! "sender")) (bytes (c ! "message")) (kindOf (c ! "kind"))
  ("device", c) -> Device (pk (c ! "pk")) (text (c ! "name"))
  ("device_gone", c) -> DeviceGone (pk (c ! "pk"))
  ("new_chat", c) -> NewChat (chatKindOf (c ! "kind")) (text (c ! "name")) (named privacies (c ! "privacy"))
  ("join_chat", c) -> JoinChat (bytes (c ! "chat_id")) (optional' text (c ! "password"))
  ("invite_to_chat", c) -> InviteToChat (chatOf (c ! "chat")) (pk (c ! "friend"))
  ("chat_joined", c) -> ChatJoined (eventId (c ! "intent")) (optional' chatOf (c ! "chat"))
  other -> error ("Not content: " <> show other)

contentJson :: Content -> Value
contentJson = \case
  Received from sender message kind -> tagged "received" $
    obj [("from", maybe Null pkJson from), ("sender", String sender), ("message", hex message), ("kind", kindJson kind)]
  Send sender message kind -> tagged "send" $
    obj [("sender", String sender), ("message", hex message), ("kind", kindJson kind)]
  Sent i -> tagged "sent" (obj [("intent", idJson i)])
  Delivered i -> tagged "delivered" (obj [("intent", idJson i)])
  Notice t -> tagged "notice" (obj [("text", String t)])
  File sender fileId name size incoming b -> tagged "file" $ obj
    [ ("sender", String sender), ("file_id", maybe Null hex fileId), ("name", String name)
    , ("size", Aeson.toJSON size), ("incoming", Bool incoming), ("blob", blobJson b) ]
  FileEnded file ok b -> tagged "file_ended" (obj [("file", idJson file), ("ok", Bool ok), ("blob", blobJson b)])
  FileAnswered file accepted -> tagged "file_answered" (obj [("file", idJson file), ("accepted", Bool accepted)])
  CallStarted sender incoming video -> tagged "call" $
    obj [("sender", String sender), ("incoming", Bool incoming), ("video", Bool video)]
  CallEnded call duration declined -> tagged "call_ended" $
    obj [("call", idJson call), ("duration", maybe Null Aeson.toJSON duration), ("declined", Bool declined)]
  WantFriend k message nospam -> tagged "want_friend" $
    obj [("pk", pkJson k), ("request_message", maybe Null String message), ("nospam", maybe Null Aeson.toJSON nospam)]
  UnwantFriend k -> tagged "unwant_friend" (obj [("pk", pkJson k)])
  ReadUpTo g i rank -> tagged "read_up_to" (obj [("graph", graphJson g), ("event", idJson i), ("rank", Aeson.toJSON rank)])
  FriendRequest k message -> tagged "friend_request" (obj [("pk", pkJson k), ("message", String message)])
  MergeChat c bot into intoBot merged -> tagged "merge_chat" $ obj
    [ ("chat", chatJson c), ("bot", pkJson bot), ("into", chatJson into), ("into_bot", pkJson intoBot)
    , ("merged", Bool merged) ]
  RelayBot c bot relays -> tagged "relay_bot" (obj [("chat", chatJson c), ("bot", pkJson bot), ("relays", Bool relays)])
  FactContent s v -> tagged "fact" (obj [("subject", subjectJson s), ("value", valueJson v)])
  Wish s v -> tagged "want" (obj [("subject", subjectJson s), ("value", valueJson v)])
  ChatInvite from kind name invite -> tagged "chat_invite" $
    obj [("from", pkJson from), ("kind", chatKindJson kind), ("name", String name), ("invite", hex invite)]
  AnswerInvite invite join password -> tagged "answer_invite" $
    obj [("invite", idJson invite), ("join", Bool join), ("password", maybe Null String password)]
  InviteJoined invite c needsPassword -> tagged "invite_joined" $
    obj [("invite", idJson invite), ("chat", maybe Null chatJson c), ("needs_password", Bool needsPassword)]
  SettingContent s setting -> tagged "setting" $
    obj [("scope", scopeJson s), ("setting", settingJson (keptValue setting))]
  ClearSetting s kind -> tagged "clear_setting" (obj [("scope", scopeJson s), ("kind", Aeson.toJSON kind)])
  ReceivedPrivately from sender message kind -> tagged "received_privately" $
    obj [("from", pkJson from), ("sender", String sender), ("message", hex message), ("kind", kindJson kind)]
  SendPrivately to sender message kind -> tagged "send_privately" $
    obj [("to", pkJson to), ("sender", String sender), ("message", hex message), ("kind", kindJson kind)]
  Device k name -> tagged "device" (obj [("pk", pkJson k), ("name", String name)])
  DeviceGone k -> tagged "device_gone" (obj [("pk", pkJson k)])
  NewChat kind name privacy -> tagged "new_chat" $
    obj [("kind", chatKindJson kind), ("name", String name), ("privacy", nameOf privacies privacy)]
  JoinChat chatId password -> tagged "join_chat" (obj [("chat_id", hex chatId), ("password", maybe Null String password)])
  InviteToChat c k -> tagged "invite_to_chat" (obj [("chat", chatJson c), ("friend", pkJson k)])
  ChatJoined intent c -> tagged "chat_joined" (obj [("intent", idJson intent), ("chat", maybe Null chatJson c)])
  UnknownContent tag -> tagged "unknown" (Aeson.toJSON tag)

eventOf :: Value -> Event
eventOf e = Event (graph (e ! "graph")) (pk (e ! "author")) (ids (e ! "parents")) (int (e ! "rank"))
  (int (e ! "time")) (contentOf (e ! "content"))

eventJson :: Event -> Value
eventJson e = obj
  [ ("graph", graphJson (eventGraph e)), ("author", pkJson (eventAuthor e)), ("parents", idsJson (eventParents e))
  , ("rank", Aeson.toJSON (eventRank e)), ("time", Aeson.toJSON (eventTime e)), ("content", contentJson (eventContent e)) ]

decoded :: Either String Value -> Value
decoded = fromRight "error"

messageOf :: Value -> SessionMessage
messageOf j = case variant j of
  ("hello", m) -> Hello (int (m ! "version")) (int (m ! "features"))
  ("summary", m) -> Summary [(graph g, HeadsDigest (bytes d)) | entry <- items m, [g, d] <- [items entry]]
  ("heads", m) -> Heads (graph (m ! "graph")) (ids (m ! "heads"))
  ("want", m) -> Want (ids m)
  ("events", m) -> Events (map bytes (items m))
  ("history", m) -> History (graph (m ! "graph")) (ids (m ! "before")) (int (m ! "limit"))
  ("want_blob", m) -> WantBlob (BlobId (bytes m))
  ("no_blob", m) -> NoBlob (BlobId (bytes m))
  ("facts", m) -> Facts (map factOf (items (m ! "facts"))) (flag (m ! "all"))
  ("call", m) -> Call (pk (m ! "friend")) $ case variant (m ! "action") of
    ("connect", c) -> Connect (flag (c ! "video"))
    ("hang_up", _) -> HangUp
    other          -> error ("Not a call action: " <> show other)
  other -> error ("Not a message: " <> show other)

messageJson :: SessionMessage -> Value
messageJson = \case
  Hello version features -> tagged "hello" (obj [("version", Aeson.toJSON version), ("features", Aeson.toJSON features)])
  Summary graphs -> tagged "summary" (Array (Vector.fromList [Array (Vector.fromList [graphJson g, hex d]) | (g, HeadsDigest d) <- graphs]))
  Heads g hs -> tagged "heads" (obj [("graph", graphJson g), ("heads", idsJson hs)])
  Want wanted -> tagged "want" (idsJson wanted)
  Events es -> tagged "events" (Array (Vector.fromList (map hex es)))
  History g before limit -> tagged "history" (obj [("graph", graphJson g), ("before", idsJson before), ("limit", Aeson.toJSON limit)])
  WantBlob (BlobId b) -> tagged "want_blob" (hex b)
  NoBlob (BlobId b) -> tagged "no_blob" (hex b)
  Facts facts everything -> tagged "facts" (obj [("facts", Array (Vector.fromList (map factJson facts))), ("all", Bool everything)])
  Call friend action -> tagged "call" $ obj
    [ ("friend", pkJson friend)
    , ("action", case action of
        Connect video -> tagged "connect" (obj [("video", Bool video)])
        HangUp        -> "hang_up") ]

-- | Replies, whose order says nothing, as the lists in them whose order
-- says nothing: all but events, which come in the order asked for.
repliesJson :: [SessionMessage] -> Value
repliesJson = sorted . map reply
  where
    sortedIds = sorted . map idJson
    reply = \case
      Summary graphs -> tagged "summary" (sorted [Array (Vector.fromList [graphJson g, hex d]) | (g, HeadsDigest d) <- graphs])
      Heads g hs -> tagged "heads" (obj [("graph", graphJson g), ("heads", sortedIds hs)])
      Want wanted -> tagged "want" (sortedIds wanted)
      History g before limit -> tagged "history" (obj [("graph", graphJson g), ("before", sortedIds before), ("limit", Aeson.toJSON limit)])
      m -> messageJson m

factsJson :: Facts -> Value
factsJson = sorted . map factJson . allFacts

stateJson :: AccountState -> Value
stateJson s = obj
  [ ("facts", factsJson (accountFacts s))
  , ("friends", sorted [ obj [("pk", pkJson k), ("wanted", Bool (friendWanted f)), ("request", maybe Null String (friendRequest f))]
                       | (k, f) <- Map.toList (accountFriends s), friendWanted f || isJust (friendRequest f) ])
  , ("merges", sorted [ obj [("chat", chatJson c), ("bot", pkJson (mergeBot m)), ("into", chatJson (mergeInto m)), ("into_bot", pkJson (mergeIntoBot m))]
                      | (c, m) <- Map.toList (accountMerges s) ])
  , ("relay_bots", sorted [obj [("chat", chatJson c), ("bot", pkJson b)] | (c, b) <- Map.toList (accountRelayBots s)])
  , ("read", sorted [obj [("graph", graphJson g), ("rank", Aeson.toJSON r), ("event", idJson i)] | (g, (r, i)) <- Map.toList (accountRead s)])
  , ("settings", sorted [ obj [ ("scope", scopeJson sc), ("kind", Aeson.toJSON k), ("setting", settingJson (keptValue v))
                              , ("bytes", maybe Null hex (keptBytes v)) ]
                        | (sc, kinds) <- Map.toList (accountSettings s), (k, v) <- Map.toList kinds ])
  , ("invites", sorted [ obj [ ("invite", idJson i), ("from", pkJson (inviteFrom v)), ("kind", chatKindJson (inviteKind v))
                             , ("name", String (inviteName v)), ("status", statusName (inviteStatus v))
                             , ("chat", maybe Null chatJson (inviteChat v))
                             , ("needs_password", Bool (inviteStatus v == Failed True)) ]
                       | (i, v) <- Map.toList (accountInvites s) ])
  , ("devices", sorted [obj [("pk", pkJson k), ("name", String n)] | (k, n) <- Map.toList (accountDevices s)])
  , ("chats_joined", sorted [obj [("intent", idJson i), ("chat", maybe Null chatJson c)] | (i, c) <- Map.toList (accountChats s)])
  ]
  where
    statusName = \case
      Pending  -> "pending"
      Joining  -> "joining"
      JoinedChat _ -> "joined"
      Failed _ -> "failed"
      Declined -> "declined"

countsJson :: EventStore -> Value
countsJson s = sorted [obj [("graph", graphJson g), ("count", Aeson.toJSON (countIn g s))] | g <- storeGraphs s]

-- Labels

newtype Labels = Labels (Map.Map Text Value)

labelsOf :: Value -> Labels
labelsOf file = Labels (foldl addGraph withChats (KeyMap.toList (table "graphs")))
  where
    table k = case file ! k of
      Object o -> o
      _        -> KeyMap.empty
    add sigil m (k, v) = Map.insert (sigil <> Key.toText k) (resolve (Labels m) v) m
    withKeys = foldl (add "%") Map.empty (KeyMap.toList (table "keys"))
    withChats = foldl (add "&") withKeys (KeyMap.toList (table "chats"))
    addGraph m (k, v) = Map.insert ("$" <> Key.toText k) (graphJson g) m
      where
        g = case variant (resolve (Labels m) v) of
          ("account", a) -> accountGraph (pk a)
          ("friend", ks) | [a, b] <- items ks -> friendGraph (pk a) (pk b)
          ("group", i) -> chatGraph (GroupChat (bytes i))
          ("conference", i) -> chatGraph (ConferenceChat (bytes i))
          other -> error ("Not a graph: " <> show other)

addEvent :: Text -> BS.ByteString -> Labels -> Labels
addEvent label bs (Labels m) =
  Labels (Map.insert ("@" <> label) (idJson (eventIdOf bs)) (Map.insert ("#" <> label) (hex bs) m))

resolve :: Labels -> Value -> Value
resolve labels@(Labels m) = \case
  String s
    | Just v <- Map.lookup s m -> v
    | Just (c, _) <- Text.uncons s, c `elem` ("%&$@#" :: String) -> error ("No label " <> Text.unpack s)
  Array a -> Array (fmap (resolve labels) a)
  Object o -> Object (fmap (resolve labels) o)
  v -> v

-- | Events of a case by label, and chains of them by name.
data Pool = Pool
  { poolEvents :: [(Text, BS.ByteString)]
  , poolChains :: Map.Map Text [Text]
  }

poolBytes :: Pool -> Text -> [BS.ByteString]
poolBytes p label = case Map.lookup label (poolChains p) of
  Just members -> concatMap (poolBytes p) members
  Nothing -> maybe (error ("No event " <> Text.unpack label)) pure (lookup label (poolEvents p))

poolStore :: Pool -> Value -> EventStore
poolStore p labels = foldl insert' emptyStore (concatMap (poolBytes p . text) (items labels))

insert' :: EventStore -> BS.ByteString -> EventStore
insert' s bs = either (const s) snd (insert bs s)

-- | The case's events, checking the bytes given for each, and the labels
-- with theirs.
pool :: Check -> Labels -> Value -> Value -> IO (Pool, Labels)
pool failures labels0 defaults testCase = go (items (testCase ! "events")) (Pool [] Map.empty) labels0 Map.empty
  where
    go [] p labels _ = pure (p { poolEvents = reverse (poolEvents p) }, labels)
    go (entry : rest) p labels ranks
      | Null /= entry ! "raw" = do
          let label = text (entry ! "label")
              bs = bytes (entry ! "raw")
          go rest p { poolEvents = (label, bs) : poolEvents p } (addEvent label bs labels) ranks
      | String name <- entry ! "chain" = do
          let resolved = resolve labels merged
              after = case entry ! "after" of
                String a | first : _ <- poolBytes p { poolEvents = reverse (poolEvents p) } a -> Just (eventIdOf first)
                _ -> Nothing
              chain (parent, acc, ls, rs) i =
                let e = Event (graph (resolved ! "graph")) (pk (resolved ! "author")) (maybe [] pure parent)
                              (maybe 0 (\x -> rs Map.! x + 1) parent) (fromIntegral i) (Notice (Text.pack (show i)))
                    bs = encodeEvent e
                    label = name <> Text.pack (show i)
                in (Just (eventIdOf bs), acc ++ [(label, bs)], addEvent label bs ls, Map.insert (eventIdOf bs) (eventRank e) rs)
              (_, members, labels', ranks') = foldl chain (after, [], labels, ranks) [0 .. (int (entry ! "count") :: Int) - 1]
          go rest p { poolEvents = reverse members ++ poolEvents p, poolChains = Map.insert name (map fst members) (poolChains p) }
             labels' ranks'
      | otherwise = do
          let resolved = resolve labels merged
              parents = ids (resolved ! "parents")
              rank = case resolved ! "rank" of
                Null -> maybe 0 (+ 1) (maximumMay (mapMaybe (`Map.lookup` ranks) parents))
                r    -> int r
              e = Event (graph (resolved ! "graph")) (pk (resolved ! "author")) parents rank
                        (int (resolved ! "time")) (contentOf (resolved ! "content"))
              bs = encodeEvent e
              label = text (entry ! "label")
          check failures (Text.unpack label <> ": bytes") (entry ! "bytes") (hex bs)
          go rest p { poolEvents = (label, bs) : poolEvents p } (addEvent label bs labels) (Map.insert (eventIdOf bs) rank ranks)
      where
        merged = case (defaults, entry) of
          (Object d, Object e) -> Object (KeyMap.union e d)
          _                    -> entry
    maximumMay [] = Nothing
    maximumMay xs = Just (maximum xs)

list' :: Value -> Text -> [Value]
list' file k = items (file ! k)

named' :: Value -> Text
named' c = text (c ! "name")

-- The files

codec :: Value -> [(Text, Check -> IO ())]
codec file = concat
  [ [ (named' c, \f -> check f "id" (c ! "id") (hex (idOf (variant (c ! "of"))))) | c <- list' file "ids" ]
  , [ (named' c, \f -> do
        let encoded = encodeEvent (eventOf (c ! "event"))
        check f "bytes" (c ! "bytes") (hex encoded)
        check f "id" (c ! "id") (idJson (eventIdOf encoded))
        check f "decoded" (c ! "event") (decoded (eventJson <$> decodeEvent (bytes (c ! "bytes")))))
    | c <- list' file "events" ]
  , [ (named' c, \f -> check f "event" (c ! "event") (decoded (eventJson <$> decodeEvent (bytes (c ! "bytes")))))
    | c <- list' file "event_decodings" ]
  , [ (named' c, \f -> do
        check f "bytes" (c ! "bytes") (hex (encodeMessage (messageOf (c ! "message"))))
        check f "decoded" (c ! "message") (decodedMessage (c ! "bytes")))
    | c <- list' file "messages" ]
  , [ (named' c, \f -> check f "message" (c ! "message") (decodedMessage (c ! "bytes"))) | c <- list' file "message_decodings" ]
  , [ (named' c, \f -> check f "packets" (c ! "packets") (Array (Vector.fromList (map hex (frames (bytes (c ! "message")))))))
    | c <- list' file "packets" ]
  , [ (named' c, \f -> check f "results" (c ! "results") (Array (Vector.fromList (reassembled (map bytes (items (c ! "packets")))))))
    | c <- list' file "reassembly" ]
  , [ (named' c, \f -> do
        let old = either error id (decodeKept (bytes (c ! "setting")))
        check f "bytes" (c ! "bytes") (maybe Null hex (keptBytes (withSetting (settingOf (c ! "value")) old))))
    | c <- list' file "setting_edits" ]
  , [ (named' c, \f -> do
        let link = parseLink (text (c ! "link"))
        check f "address" (c ! "address") (maybe Null (String . Text.toLower . linkAddress) link)
        check f "code" (c ! "code") (maybe Null (String . linkCode) link)
        check f "request" (c ! "request") (maybe Null (String . requestMessage) link))
    | c <- list' file "pairing" ]
  ]
  where
    idOf = \case
      ("account", k) -> let GraphId g = accountGraph (pk k) in g
      ("friend", ks) | [a, b] <- items ks -> let GraphId g = friendGraph (pk a) (pk b) in g
      ("group", i) -> let GraphId g = chatGraph (GroupChat (bytes i)) in g
      ("conference", i) -> let GraphId g = chatGraph (ConferenceChat (bytes i)) in g
      ("blob", d) -> let BlobId b = blobIdOf (bytes d) in b
      ("heads", hs) -> let HeadsDigest d = headsDigest (ids hs) in d
      other -> error ("Not an ID: " <> show other)
    decodedMessage v = decoded (maybe Null messageJson <$> decodeMessage (bytes v))
    reassembled = go reassembler
      where
        go _ [] = []
        go r (p : ps) = case push p r of
          Left (_, r')  -> "error" : go r' ps
          Right (m, r') -> maybe Null hex m : go r' ps
    -- A setting as an event reads it.
    decodeKept bs = case decodeEvent (encodeEvent (Event (GraphId (BS.replicate 32 0)) (PublicKey (BS.replicate 32 0)) [] 0 0
                                                     (SettingContent AccountScope (Kept (UnknownSetting 0) (Just bs))))) of
      Right (Event _ _ _ _ _ (SettingContent _ s)) -> Right s
      Right _                                      -> Left "Not a setting"
      Left e                                       -> Left e

state :: Value -> [(Text, Check -> IO ())]
state file =
  [ (named' c, \f -> do
      (p, labels) <- pool f base defaults c
      let fold' events = stateJson (accountOf account (foldl insert' emptyStore events))
          now = fold' (map snd (poolEvents p))
      check f "state, from the events reversed" now (fold' (reverse (map snd (poolEvents p))))
      check f "state" (resolve labels (c ! "state")) now)
  | c <- list' file "account_states" ]
  ++
  [ (named' c, \f -> do
      let before = foldl (\fs (s, v) -> setFact s v fs) noFacts (map factOf (items (resolve base (c ! "before"))))
          after = updateFacts (map factOf (items (resolve base (c ! "update")))) before
      check f "after" (resolve base (c ! "after")) (factsJson after))
  | c <- list' file "live_facts" ]
  where
    base = labelsOf file
    defaults = file ! "defaults"
    account = graph (resolve base (defaults ! "graph"))

store' :: Value -> [(Text, Check -> IO ())]
store' file =
  [ (named' c, \f -> do
      (p, labels) <- pool f base defaults c
      let inserted = concatMap (poolBytes p . text) (items (c ! "inserts"))
          step (held, done) bs = case insert bs held of
            Right (_, held') -> (held', done ++ [Bool True])
            Left _           -> (held, done ++ [Bool False])
          (s, results) = foldl step (emptyStore, []) inserted
      check f "results" (c ! "results") (Array (Vector.fromList results))
      check f "graphs" (resolve labels (c ! "graphs")) $ sorted
        [ obj [ ("graph", graphJson g), ("heads", sorted (map idJson (headsOf g s)))
              , ("frontier", sorted (map (idJson . fst) (frontier g s))), ("count", Aeson.toJSON (countIn g s)) ]
        | g <- storeGraphs s ]
      case c ! "display" of
        Array gs -> check f "display_order" (resolve labels (c ! "display_order")) $ Array $ Vector.fromList
          [idsJson (map fst (displayOrder (graph g) s)) | g <- items (resolve labels (Array gs))]
        _ -> pure ())
  | c <- list' file "stores" ]
  where
    base = labelsOf file
    defaults = file ! "defaults"

-- | Runs a session between a home's store and a replica until neither has
-- anything left to say, or Nothing if it doesn't end.
converse :: EventStore -> Replica -> [SessionMessage] -> Maybe (EventStore, Replica)
converse = go maxRounds
  where
    go 0 _ _ _ = Nothing
    go _ home replica [] = Just (home, replica)
    go n home replica toReplica =
      let step (h, r, next) message =
            let (replies, r') = received message r
                (h', more) = foldl reply (h, []) replies
            in (h', r', next ++ more)
          reply (h, more) = \case
            Events es -> (foldl insert' h es, more)
            m         -> (h, more ++ answer h m)
          (home', replica', next') = foldl step (home, replica, []) toReplica
      in go (n - 1) home' replica' next'

session :: Value -> [(Text, Check -> IO ())]
session file =
  [ (named' c, \f -> do
      (p, labels) <- pool f base defaults c
      let s = poolStore p (c ! "store")
          message = if c ! "message" == "summary" then summary s else messageOf (resolve labels (c ! "message"))
      check f "replies" (resolve labels (c ! "replies")) (repliesJson (answer s message)))
  | c <- list' file "answers", c ! "role" /= "home" ]
  ++
  [ (named' c, \f -> do
      (p, labels0) <- pool f base defaults c
      let start = (poolStore p (c ! "home_store"), newReplica (pk (resolve labels0 (c ! "device"))) (pk (resolve labels0 (c ! "home"))) emptyStore, labels0)
      let steps = zip [1 :: Int ..] (items (c ! "steps"))
      _ <- foldlM' (runStep f p) start steps
      pure ())
  | c <- list' file "replicas" ]
  where
    base = labelsOf file
    defaults = file ! "defaults"
    foldlM' g z = foldl (\acc x -> acc >>= \a -> g a x) (pure z)
    results = ["replies", "event", "push", "replica", "home"] :: [Text]
    runStep f p (home, replica, labels) (i, step) = do
      let (action, spec') = case [(Key.toText k, v) | Object o <- [step], (k, v) <- KeyMap.toList o, Key.toText k `notElem` results] of
            [one] -> one
            other -> error ("Not a step: " <> show other)
          what = "step " <> show i <> ", " <> Text.unpack action
      case action of
        "receive" -> do
          let (replies, replica') = received (messageOf (resolve labels spec')) replica
          check f (what <> ": replies") (resolve labels (step ! "replies")) (repliesJson replies)
          pure (home, replica', labels)
        "offline" -> pure (home, offline replica, labels)
        "home_add" -> pure (foldl insert' home (concatMap (poolBytes p . text) (items spec')), replica, labels)
        "author" -> do
          let a = resolve labels spec'
              (_, bs, pushed, replica') = author (graph (a ! "graph")) (int (a ! "time") :: Int64) (contentOf (a ! "content")) replica
              labels' = addEvent (text (a ! "label")) bs labels
              home' = foldl insert' home (concat [es | Events es <- pushed])
          check f (what <> ": event") (resolve labels' (step ! "event")) (either (const "error") eventJson (decodeEvent bs))
          check f (what <> ": push") (resolve labels' (step ! "push")) (repliesJson pushed)
          pure (home', replica', labels')
        "converse" -> do
          let start' = if spec' == "session" then [hello, summary home] else map messageOf (items (resolve labels spec'))
          case converse home replica start' of
            Nothing -> check f (what <> ": the session never ends") Null (Bool True) >> pure (home, replica, labels)
            Just (home', replica') -> do
              check f (what <> ": replica") (resolve labels (step ! "replica")) (countsJson (replicaStore replica'))
              check f (what <> ": home") (resolve labels (step ! "home")) (countsJson home')
              pure (home', replica', labels)
        _ -> error ("Not a step: " <> Text.unpack action)

