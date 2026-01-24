{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | What the app shows, how events change it, and what it asks of Tox.
-- Pure, so the whole UI logic is testable off the device.
module Tox.Android.Model
  ( ContactId (..)
  , MessageId
  , Contact (..)
  , contactLabel
  , contactOrder
  , GroupId (..)
  , Group (..)
  , GroupPhase (..)
  , GroupKind (..)
  , newGroup
  , MemberAction (..)
  , memberActions
  , Founding (..)
  , foundingError
  , chatIdError
  , Member (..)
  , GroupEntry (..)
  , Happening (..)
  , Invite (..)
  , groupMuted
  , mentions
  , memberLabel
  , uttered
  , relayCounts
  , relayedThrough
  , groupLabel
  , inviteLabel
  , capitalised
  , kindName
  , ChatRow (..)
  , chatOrder
  , groupNick
  , Mute (..)
  , muted
  , Message (..)
  , Attachment (..)
  , Staged (..)
  , CallRecord (..)
  , FileState (..)
  , FileRef (..)
  , autoAccepted
  , AccountSettings (..)
  , DeviceSettings (..)
  , preview
  , quoted
  , forwardable
  , copiedText
  , callText
  , duration
  , interrupted
  , linkError
  , Delivery (..)
  , Profile (..)
  , ProfileId (..)
  , Account (..)
  , Connection (..)
  , Call (..)
  , CallPhase (..)
  , Glare (..)
  , Route (..)
  , ringTimeout
  , Screen (..)
  , Model (..)
  , Event (..)
  , Command (..)
  , toxIdLength
  , toxId
  , unhex
  , addressError
  , requestError
  , profileError
  , requestMessage
  , initial
  , update
  , back
  , notifications
  , groupNotifications
  , missedCalls
  , callHistory
  ) where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.Bits              (xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Char              (isAlphaNum, isHexDigit)
import           Data.Either            (fromRight)
import           Data.Int               (Int64)
import           Data.List              (sortOn)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isJust, isNothing,
                                         listToMaybe, mapMaybe)
import           Data.Ord               (Down (..))
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Time              (NominalDiffTime, UTCTime, diffUTCTime)
import           Data.Word              (Word64, Word8)
import           Text.Read              (readMaybe)
import qualified Tox.Application.Tox    as Tox

import           Tox.Android.Formats    (Format (..), Reading (..), formats,
                                         readWith)
import qualified Tox.Android.Linked     as Linked
import           Tox.Android.Linked     (Linked)
import           Tox.Android.Locations  (location, locationText)
import           Tox.Android.Messages   (parts)
import           Tox.Android.Relays     (relayed, relayedNames)
import qualified Tox.Sync.Account       as Sync
import qualified Tox.Sync.Event         as Sync
import qualified Tox.Sync.Fact          as Sync
import qualified Tox.Sync.Id            as Sync
import qualified Tox.Sync.Pairing       as Pairing
import qualified Tox.Sync.Replica       as Replica
import qualified Tox.Sync.Setting       as Sync
import qualified Tox.Sync.Store         as Sync

-- | A friend's public key, in upper case hex.
newtype ContactId = ContactId Text
  deriving (Eq, Ord, Show)

-- | A message's place in its chat.
type MessageId = Int

-- | How far one of our messages got.
data Delivery = Pending | Sent | Delivered
  deriving (Eq, Show)

data Message = Message
  { messageMine      :: Bool
  , messageText      :: Text
    -- ^ A file's name, for one with a file.
  , messageTime      :: UTCTime
  , messageDelivery  :: Delivery
    -- ^ Of ours. Theirs are all delivered.
  , messageFile      :: Maybe Attachment
  , messageCall      :: Maybe CallRecord
    -- ^ A call, with the time it began to ring and who made it.
  , messageParts     :: Int
    -- ^ Of ours, how many of its 'parts' the friend confirmed.
  , messageDelivered :: Maybe UTCTime
    -- ^ Of ours, when the friend had all of it, if known.
  , messageKind      :: Tox.MessageType
  , messageRaw       :: Maybe BS.ByteString
    -- ^ Of theirs, the bytes as Tox carried them, which the text was read
    -- from. None from before hTox kept them.
  } deriving (Eq, Show)

-- | Something picked for a chat, which goes with its draft.
data Staged
  = StagedFile FilePath Text Word64 BS.ByteString
    -- ^ Where hTox keeps its copy, its name and size, and a new ID for it.
  | StagedPlace Double Double
  deriving (Eq, Show)

-- | A call that ended, in its chat.
data CallRecord = CallRecord
  { recordVideo    :: Bool
  , recordDuration :: Maybe NominalDiffTime
    -- ^ Of one that was answered.
  , recordMissed   :: Bool
    -- ^ Theirs, which rang here until it stopped.
  } deriving (Eq, Show)

-- | A file sent or received in a chat.
data Attachment = Attachment
  { attachmentSize  :: Word64
  , attachmentPath  :: FilePath
    -- ^ Where it is kept here: ours from the start, theirs as it comes.
  , attachmentDone  :: Word64
    -- ^ How much of it went or came.
  , attachmentState :: FileState
  , attachmentId    :: Maybe BS.ByteString
    -- ^ What Tox calls the file, by which an offer of it again goes on
    -- where it was cut off. None for files from before hTox kept it.
  } deriving (Eq, Show)

data FileState
  = Waiting
    -- ^ For the friend to take ours, or for the user to take theirs.
  | Moving
  | Complete
  | Interrupted
    -- ^ Theirs, cut off by going offline or a restart, to go on when the
    -- friend offers it again.
  | Cancelled
    -- ^ By either side, or cut off with no way to go on.
  deriving (Eq, Show)

-- | A transfer Tox has going with a friend, by its file number there, which
-- lasts as long as the friend stays online.
data FileRef = Sending Word8 | Receiving Word8
  deriving (Eq, Ord, Show)

-- | What a message says in a list or a notification, an action with the
-- name of who did it.
preview :: Text -> Message -> Text
preview name m = case (messageCall m, messageFile m) of
  (Just record, _) -> callText (messageMine m) record
  (_, Just _)      -> "\x1F4CE " <> messageText m
  _ | messageKind m == Tox.MessageAction -> "* " <> name <> " " <> messageText m
    | Just _ <- location (messageText m) -> "\x1F4CD Location"
    | otherwise -> messageText m

-- | Whether a message can be sent on: calls can't, and files only ours or
-- whole ones.
forwardable :: Message -> Bool
forwardable m = isNothing (messageCall m)
  && maybe True (\a -> messageMine m || attachmentState a == Complete) (messageFile m)

-- | A message as a reply quotes it: each line after "> ".
quoted :: Message -> Text
quoted = Text.intercalate "\n" . map ("> " <>) . Text.lines . messageText

-- | What messages of a chat copy as: one its text, several each with whose
-- it was, in the chat's order. Calls are left out.
copiedText :: Model -> ContactId -> [MessageId] -> Text
copiedText model cid is = case [m | (i, m) <- zip [0 ..] messages, i `elem` is, isNothing (messageCall m)] of
  [m] -> messageText m
  ms  -> Text.intercalate "\n" [whose m <> ": " <> messageText m | m <- ms]
  where
    contact = Map.lookup cid (modelContacts model)
    messages = maybe [] contactMessages contact
    whose m
      | messageMine m = profileNickname (modelProfile model)
      | otherwise = maybe "" (contactLabel cid) contact

-- | What a call says in its chat: whose it was, and how long it went.
callText :: Bool -> CallRecord -> Text
callText mine record =
  maybe kind (\talked -> kind <> " \x00B7 " <> duration (round talked)) (recordDuration record)
  where
    kind = whose <> (if recordVideo record then " video call" else " voice call")
    whose
      | recordMissed record = "Missed"
      | mine = "Outgoing"
      | otherwise = "Incoming"

-- | Minutes and seconds.
duration :: Int -> Text
duration seconds = Text.pack (show (seconds `div` 60)) <> ":" <> Text.justifyRight 2 '0' (Text.pack (show (seconds `mod` 60)))

-- | Theirs up to this size come in without asking, unless the user chose
-- otherwise.
autoAccepted :: Word64
autoAccepted = 4 * 1024 * 1024

-- | What the user chose for the account, the same on each of its devices.
data AccountSettings = AccountSettings
  { settingAutoAccept :: Word64
    -- ^ Theirs up to this size come in without asking, none if 0.
  , settingFormatsOff :: Set Text
    -- ^ The keys of the formats not to read, whose messages then show as
    -- Tox carried them.
  } deriving (Eq, Show)

-- | The formats the user lets hTox read.
readable :: AccountSettings -> [Format]
readable account = [f | f <- formats, formatKey f `Set.notMember` settingFormatsOff account]

-- | What the user chose for this device: how Tox reaches the network.
-- Changes apply when Tox starts again.
data DeviceSettings = DeviceSettings
  { settingUdp   :: Bool
  , settingIpv6  :: Bool
  , settingLan   :: Bool
    -- ^ Find friends on the local network.
  , settingStay  :: Bool
    -- ^ Stay online while hTox is closed, and from when the phone starts.
  , settingProxy :: Maybe Tox.Proxy
    -- ^ What TCP relays are reached through, with UDP and local discovery
    -- off.
  } deriving (Eq, Show)

data Contact = Contact
  { contactName          :: Maybe Text
  , contactOnline        :: Bool
  , contactMessages      :: [Message]
    -- ^ Oldest first.
  , contactUnread        :: Int
  , contactRequest       :: Maybe (Text, Text)
    -- ^ Our friend request, its Tox ID and message, until the friend is
    -- first online: Tox forgets it at a restart.
  , contactStatusMessage :: Text
  , contactStatus        :: Tox.UserStatus
    -- ^ As the friend says while online.
  , contactMute          :: Maybe Mute
  , contactAvatar        :: Maybe BS.ByteString
    -- ^ The hash of the picture the friend shows, of those in 'modelAvatars'.
  , contactPictures      :: Maybe Text
    -- ^ The key of the way pictures go to the friend, if not as they are.
  } deriving (Eq, Show)

-- | Notifications of a chat off, for a while or for good.
data Mute = MutedUntil UTCTime | MutedAlways
  deriving (Eq, Show)

-- | Whether a contact's notifications are off at a time.
muted :: UTCTime -> Contact -> Bool
muted now = mutedBy now . contactMute

mutedBy :: UTCTime -> Maybe Mute -> Bool
mutedBy now = \case
  Just (MutedUntil end) -> now < end
  Just MutedAlways      -> True
  Nothing               -> False

-- | Its name, or the start of its key until it has one.
contactLabel :: ContactId -> Contact -> Text
contactLabel (ContactId key) contact = fromMaybe (Text.take 8 key) (contactName contact)

-- | The contacts, the latest chat first, then those with no messages by
-- name.
contactOrder :: Model -> [(ContactId, Contact)]
contactOrder = sortOn latest . Map.toList . modelContacts
  where
    latest (cid, c) =
      (Down (messageTime <$> listToMaybe (reverse (contactMessages c))), Text.toCaseFold (contactLabel cid c))

-- | A group's chat ID, in upper case hex, like 'ContactId': Tox numbers
-- groups anew each time it starts.
newtype GroupId = GroupId Text
  deriving (Eq, Ord, Show)

-- | Someone in a group as Tox says: their key there, in hex, and their
-- name in it.
data Member = Member
  { memberKey  :: Text
  , memberName :: Text
  } deriving (Eq, Show)

data GroupPhase
  = Joining
    -- ^ Until Tox has the group's state from its peers, at each start too.
  | InGroup
  | Refused Tox.JoinFail
  | Kicked
  | Gone
    -- ^ Left by the user, with its chat kept.
  deriving (Eq, Show)

-- | Tox's two kinds of group chat: NGC groups, and the older conferences,
-- whose audio ones hTox joins for their text.
data GroupKind = NgcGroup | TextConference | AudioConference
  deriving (Eq, Show)

-- | A group chat, its ID the group's chat ID or the conference's.
data Group = Group
  { groupName     :: Text
  , groupKind     :: GroupKind
  , groupPhase    :: GroupPhase
  , groupEntries  :: [GroupEntry]
    -- ^ Oldest first.
  , groupUnread   :: Int
  , groupMembers  :: Map Tox.PeerId Member
    -- ^ Without us, by Tox's number for each while they stay, as Tox says
    -- once it is in the group. Not kept.
  , groupSaved    :: Maybe BS.ByteString
    -- ^ What Tox needs to rejoin it, as it last gave it.
  , groupMute     :: Maybe Mute
  , groupMentions :: Bool
    -- ^ Whether messages naming us notify while it is muted, as by default.
  , groupTopic    :: Text
  , groupMayTopic :: Bool
    -- ^ Whether Tox would take a topic from us, as it last said. Not kept.
  , groupSince    :: Maybe UTCTime
    -- ^ When Tox was last in it: it tells of those there before as of
    -- peers joining while it meets them. Not kept.
  , groupRoles    :: Map Tox.PeerId Tox.Role
    -- ^ Of members, as Tox last said. Not kept.
  , groupSelfRole :: Maybe Tox.Role
    -- ^ Ours. Not kept.
  , groupSettings :: Maybe Tox.GroupSettings
    -- ^ The founder's, as Tox last said. Not kept.
  , groupRelayBot :: Maybe Text
    -- ^ The key of the member whose messages show under the names it relays,
    -- if the user chose one.
  } deriving (Eq, Show)

-- | A group as it starts, with nothing in it yet.
newGroup :: Text -> GroupKind -> GroupPhase -> Group
newGroup name kind phase = Group
  { groupName = name, groupKind = kind, groupPhase = phase, groupEntries = [], groupUnread = 0
  , groupMembers = Map.empty, groupSaved = Nothing, groupMute = Nothing, groupMentions = True, groupTopic = ""
  , groupMayTopic = False, groupSince = Nothing, groupRoles = Map.empty, groupSelfRole = Nothing
  , groupSettings = Nothing, groupRelayBot = Nothing }

-- | What we may do to a member of a role, with ours, as toxcore allows:
-- the founder sets any other role and kicks anyone, moderators only users
-- and observers.
data MemberAction = MakeModerator | MakeUser | MakeObserver | Kick
  deriving (Eq, Show)

memberActions :: Maybe Tox.Role -> Tox.Role -> [MemberAction]
memberActions ours theirs = case ours of
  Just Tox.Founder | theirs /= Tox.Founder ->
    [MakeModerator | theirs /= Tox.Moderator] ++ [MakeUser | theirs /= Tox.User]
      ++ [MakeObserver | theirs /= Tox.Observer] ++ [Kick]
  Just Tox.Moderator | theirs `elem` [Tox.User, Tox.Observer] ->
    [MakeUser | theirs /= Tox.User] ++ [MakeObserver | theirs /= Tox.Observer] ++ [Kick]
  _ -> []

-- | What a group's chat shows, at a time.
data GroupEntry = GroupEntry
  { entryTime :: UTCTime
  , entryWhat :: Happening
  } deriving (Eq, Show)

data Happening
  = Said (Maybe Member) Tox.MessageType Text Delivery
    -- ^ By someone as named then, or by us. Ours go 'Pending' to 'Sent',
    -- as groups confirm nothing.
  | PrivateFrom Member Tox.MessageType Text
  | PrivateTo Member Tox.MessageType Text Delivery
    -- ^ Ours, to them alone.
  | Moderated (Maybe Member) (Maybe Member) Tox.Moderation
    -- ^ Who did it to whom: us, if none.
  | Arrived Member
  | Departed Member
  | TopicSet (Maybe Member) Text
    -- ^ By someone, or by us.
  | Noted Text
    -- ^ What a linked account's home device tells of the chat.
  deriving (Eq, Show)

-- | A friend's invite to a group.
data Invite = Invite
  { inviteKind     :: GroupKind
  , inviteName     :: Text
    -- ^ None for a conference.
  , inviteData     :: BS.ByteString
    -- ^ As Tox gave it, to answer with.
  , inviteAnswered :: Maybe Text
    -- ^ Taken, with a group's password, until the friend is online to hear
    -- it.
  } deriving (Eq, Show)

-- | Their name, or the start of their key while they have none.
memberLabel :: Member -> Text
memberLabel m
  | not (Text.null (memberName m)) = memberName m
  | not (Text.null (memberKey m)) = Text.take 8 (memberKey m)
  | otherwise = "Someone"

-- | Who a member's message shows as by, and what it says: the relay bot's
-- under the name it relays, with its own, else as it came.
uttered :: Profile -> Group -> Member -> Text -> (Text, Text)
uttered profile g = utter
  where
    -- Once for all of a group's messages.
    known = speakers profile g
    utter who text = case relayed known text of
      Just (name, said) | Just (memberKey who) == groupRelayBot g -> (name <> " via " <> memberLabel who, said)
      _ -> (memberLabel who, text)

-- | The names of those who speak in a group for themselves, us too, which
-- tell where a relayed name ends.
speakers :: Profile -> Group -> [Text]
speakers profile g = Set.toList $ Set.fromList $ groupNick profile
  : [memberLabel m | GroupEntry _ (Said (Just m) _ _ _) <- groupEntries g, Just (memberKey m) /= groupRelayBot g]

-- | Those who said anything in a group, each as last named, with how many of
-- their messages read as relayed and how many there are, the likeliest
-- bot first.
relayCounts :: Group -> [(Member, Int, Int)]
relayCounts g = sortOn (\(m, r, n) -> (Down r, Down n, memberLabel m)) $ Map.elems $ Map.fromListWith add
  [ (memberKey m, (m, fromEnum (isJust (relayed [] text)), 1)) | GroupEntry _ (Said (Just m) _ text _) <- groupEntries g ]
  where add (m, r, n) (_, r', n') = (m, r + r', n + n')

-- | The names the relay bot relayed, each with when it last did, latest
-- first.
relayedThrough :: Profile -> Group -> [(Text, UTCTime)]
relayedThrough profile g = relayedNames (speakers profile g)
  [ (time, text) | GroupEntry time (Said (Just m) _ text _) <- groupEntries g, Just (memberKey m) == groupRelayBot g ]

-- | What the user can found: a group joined by invite, or by its chat ID
-- too, or a conference, which older clients also have.
data Founding = PrivateGroup | PublicGroup | NewConference
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Why a name can't be one's, if it can't.
foundingError :: Founding -> Text -> Maybe Text
foundingError founding name
  | Text.null (Text.strip name) = Just "The name can't be empty"
  | BS.length (Text.encodeUtf8 (Text.strip name)) > limit = Just "The name is too long"
  | otherwise = Nothing
  where
    limit = if founding == NewConference then Tox.maxNameLength else Tox.maxGroupNameSize

-- | Why a text isn't a group's chat ID, if it isn't. Nothing while it's
-- empty.
chatIdError :: Text -> Maybe Text
chatIdError chatId
  | Text.null chatId = Nothing
  | Text.length chatId /= 64 || not (Text.all isHexDigit chatId) = Just "Must be 64 hexadecimal characters"
  | otherwise = Nothing

-- | What the user calls a kind of group chat.
kindName :: GroupKind -> Text
kindName = \case
  NgcGroup        -> "group"
  TextConference  -> "conference"
  AudioConference -> "audio conference"

-- | A group's name, or its kind until it has one: conferences come with
-- none.
groupLabel :: Group -> Text
groupLabel g = nameOr (groupName g) (groupKind g)

-- | Likewise of an invite.
inviteLabel :: Invite -> Text
inviteLabel invite = nameOr (inviteName invite) (inviteKind invite)

nameOr :: Text -> GroupKind -> Text
nameOr name kind
  | Text.null name = capitalised (kindName kind)
  | otherwise = name

capitalised :: Text -> Text
capitalised word = Text.toUpper (Text.take 1 word) <> Text.drop 1 word

-- | Whether a group's notifications are off at a time.
groupMuted :: UTCTime -> Group -> Bool
groupMuted now = mutedBy now . groupMute

-- | Whether a text names someone: their name as a whole word, in any case,
-- words being letters, digits and underscores.
mentions :: Text -> Text -> Bool
mentions name text
  | Text.null needle = False
  | otherwise = any whole (Text.breakOnAll needle haystack)
  where
    needle = Text.toCaseFold (Text.strip name)
    haystack = Text.toCaseFold text
    whole (before, rest) = not (wordy (Text.takeEnd 1 before))
      && not (wordy (Text.take 1 (Text.drop (Text.length needle) rest)))
    wordy = Text.any (\c -> isAlphaNum c || c == '_')

-- | A chat in the list.
data ChatRow = FriendRow ContactId Contact | GroupRow GroupId Group
  deriving (Eq, Show)

-- | Friends' chats and groups, the latest first, then those with no
-- messages by name.
chatOrder :: Model -> [ChatRow]
chatOrder model = sortOn latest $
  [FriendRow cid c | (cid, c) <- Map.toList (modelContacts model)]
  ++ [GroupRow gid g | (gid, g) <- Map.toList (modelGroups model)]
  where
    latest = \case
      FriendRow cid c ->
        (Down (messageTime <$> listToMaybe (reverse (contactMessages c))), Text.toCaseFold (contactLabel cid c))
      GroupRow _ g ->
        (Down (entryTime <$> listToMaybe (reverse (groupEntries g))), Text.toCaseFold (groupName g))

-- | Our name in groups: Tox takes no empty one.
groupNick :: Profile -> Text
groupNick p = if Text.null (Text.strip (profileNickname p)) then "Tox User" else profileNickname p

data Profile = Profile
  { profileNickname :: Text
  , profileStatus   :: Text
  , profileToxId    :: Text
  } deriving (Eq, Show)

-- | An account on the device, as the database numbers it.
newtype ProfileId = ProfileId Int64
  deriving (Eq, Ord, Show)

-- | An account as the list of them shows it.
data Account = Account
  { accountId   :: ProfileId
  , accountName :: Text
  , accountKey  :: Text
    -- ^ Its public key in hex, empty until Tox first made its keys.
  } deriving (Eq, Show)

-- | Worst first.
data Connection = Offline | ViaTcp | ViaUdp
  deriving (Eq, Ord, Show)

-- | A call with a friend. There is one at most.
data Call = Call
  { callContact    :: ContactId
  , callPhase      :: CallPhase
  , callStarted    :: UTCTime
    -- ^ When it began to ring.
  , callMine       :: Bool
    -- ^ Whether we made it.
  , callVideo      :: Bool
    -- ^ Whether it began as a video call.
  , callCamera     :: Bool
    -- ^ Whether we send them video.
  , callPeerVideo  :: Bool
    -- ^ Whether they send us video.
  , callMuted      :: Bool
    -- ^ Our microphone, whose frames don't go out while muted.
  , callRoute      :: Route
    -- ^ Where its audio goes.
  , callRoutes     :: [Route]
    -- ^ Where it could go.
  , callBackCamera :: Bool
    -- ^ Whether the camera that films is the one on the back.
  , callHidden     :: Bool
    -- ^ Whether the user went back to the chats, with a banner to return.
  , callGlare      :: Glare
  , callEntry      :: Maybe MessageId
    -- ^ Its entry in the chat, there from the start, so a call cut short
    -- by hTox stopping is still there.
  } deriving (Eq, Show)

data CallPhase
  = Calling
  | Ringing
    -- ^ They call us.
  | Talking UTCTime
    -- ^ Since then.
  deriving (Eq, Show)

-- | Tox fails a call both ends place at once, at both ends. The end with
-- the higher key places it again and the other answers that, as other
-- clients do, so both settle on one call.
data Glare
  = Clear
  | Glared UTCTime
    -- ^ Since then.
  | Settling
    -- ^ Placed again or answered, so another failure ends it.
  deriving (Eq, Show)

-- | How long the end placing the call again waits for what is left of the
-- first, and how long the other waits for it.
glareDelay, glareWait :: NominalDiffTime
glareDelay = 2
glareWait = 8

-- | Where a call's audio goes.
data Route = Earpiece | Speaker | Wired | Bluetooth
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How long Tox takes to meet those already in a group it got into.
meeting :: NominalDiffTime
meeting = 15

-- | How long a call rings before it is given up.
ringTimeout :: NominalDiffTime
ringTimeout = 60

data Screen
  = Contacts
  | Chat ContactId
  | Details ContactId
  | Forward ContactId [MessageId]
    -- ^ Choosing whom to send messages of a chat.
  | MessageInfo ContactId MessageId
  | CallsScreen
  | ProfileScreen
  | SettingsScreen
  | ProfilesScreen
  | ProxyScreen
  | AddContact
  | GroupChat GroupId
  | GroupDetails GroupId
  | InviteScreen ContactId GroupId
  | TopicScreen GroupId
  | NewGroupScreen
  | JoinGroupScreen
    -- ^ A public group, by its chat ID.
  | InviteFriendsScreen GroupId
  | GroupSettingsScreen GroupId
  | LinkScreen
    -- ^ Linking to an account another device runs, by its pairing link.
    -- ^ Joining a friend's invite, with its password if it has one.
  deriving (Eq, Show)

data Model = Model
  { modelScreen        :: Screen
  , modelShown         :: Bool
    -- ^ Whether the screen is in front of the user.
  , modelDrawer        :: Bool
    -- ^ Over the contacts.
  , modelProfile       :: Profile
  , modelConnection    :: Connection
  , modelContacts      :: Map ContactId Contact
  , modelRequests      :: Map ContactId Text
    -- ^ Friend requests, with what they say.
  , modelFormer        :: Map ContactId Contact
    -- ^ Contacts deleted with their chats kept, which come back when added
    -- again.
  , modelDeclined      :: Map ContactId Text
    -- ^ Friend requests rejected, which stay away while what they say
    -- stays the same: Tox sends them again and again.
  , modelDrafts        :: Map ContactId Text
  , modelGroups        :: Map GroupId Group
  , modelInvites       :: Map (ContactId, GroupId) Invite
  , modelGroupDrafts   :: Map GroupId Text
  , modelPassword      :: Text
  , modelTopic         :: Text
  , modelFounding      :: Founding
  , modelGroupName     :: Text
    -- ^ Of the group to found, until it is.
  , modelChatId        :: Text
  , modelPrivateTo     :: Maybe Tox.PeerId
    -- ^ Whom the open group chat's draft goes to alone.
  , modelPeerLimit     :: Text
    -- ^ The settings of the group open, as edited until applied.
  , modelGroupPassword :: Text
    -- ^ Of the public group to join, with 'modelPassword'.
    -- ^ As edited on its screen until applied.
    -- ^ Of the invite open, until it is answered.
  , modelNickname      :: Text
    -- ^ Edited on the profile screen until applied, like the status.
  , modelStatus        :: Text
  , modelAddress       :: Text
  , modelRequest       :: Text
  , modelAddError      :: Maybe Text
    -- ^ Why Tox refused the contact last added.
  , modelProxyKind     :: Maybe Tox.ProxyType
    -- ^ The proxy as edited on its screen until applied, like the host
    -- and port.
  , modelProxyHost     :: Text
  , modelProxyPort     :: Text
  , modelTransfers     :: Map (ContactId, FileRef) MessageId
    -- ^ The messages of the transfers Tox has going.
  , modelCall          :: Maybe Call
  , modelFullScreen    :: Bool
    -- ^ Whether calls may ring over the lock screen.
  , modelProfileId     :: ProfileId
    -- ^ The account online, of 'modelProfiles'.
  , modelProfiles      :: [Account]
    -- ^ Every account on the device.
  , modelAway          :: Bool
    -- ^ Whether the open chat is scrolled away from its newest messages.
  , modelUnseen        :: Int
    -- ^ Messages that came in the open chat while it was scrolled away.
  , modelJump          :: Int
    -- ^ Changes to scroll the open chat to its newest messages.
  , modelSelected      :: Set MessageId
    -- ^ Of the open chat.
  , modelStaged        :: Map ContactId [Staged]
    -- ^ What goes with each chat's draft, in the order picked.
  , modelReplies       :: Map ContactId MessageId
    -- ^ What the draft of each chat answers.
  , modelAvatar        :: Maybe BS.ByteString
    -- ^ The hash of the picture we show friends.
  , modelAvatars       :: Map BS.ByteString BS.ByteString
    -- ^ The pictures we and friends show, as PNGs by their SHA-256.
  , modelAccount       :: AccountSettings
  , modelDevice        :: DeviceSettings
  , modelNotice        :: Maybe Text
    -- ^ What the user should know, until they go elsewhere.
  , modelLinked        :: Maybe Linked
    -- ^ The account another device runs on Tox, if this profile is linked
    -- to one: what the chats show is then derived from it.
  , modelLink          :: Text
    -- ^ The pairing link of an account to link to, as typed.
  } deriving (Eq, Show)

data Event
  = Open Screen
  | Back
  | OpenDrawer
  | DraftChanged Text
  | Send UTCTime
  | NicknameChanged Text
  | StatusChanged Text
  | ApplyProfile
  | AddressChanged Text
  | RequestChanged Text
  | AddFriend
  | Accept ContactId
  | Reject ContactId
  | Started Text
    -- ^ Tox runs, with this Tox ID.
  | Connected Connection
  | FriendOnline ContactId Bool
  | FriendNamed ContactId Text
  | FriendStatusMessage ContactId Text
  | FriendStatus ContactId Tox.UserStatus
  | DeleteContact ContactId Bool
    -- ^ Keeping its chat or not.
  | MuteChat ContactId (Maybe UTCTime)
    -- ^ Until a time, or for good.
  | UnmuteChat ContactId
  | Received ContactId UTCTime Tox.MessageType BS.ByteString
    -- ^ A message of the friend as Tox carried it.
  | Delivering ContactId MessageId Delivery
  | Confirmed ContactId MessageId Int UTCTime
    -- ^ How many parts of our message the friend has, as of a time.
  | ScrolledAway Bool
    -- ^ The open chat left its newest messages, or came back to them.
  | JumpToEnd
  | Select MessageId
    -- ^ Or unselect, a message of the open chat.
  | Unselect
  | Reply MessageId
    -- ^ Answer a message of the open chat with the draft.
  | CancelReply
  | ForwardTo ContactId UTCTime
  | Requested ContactId Text
  | AddFailed Text Text
    -- ^ Tox refused to add a Tox ID, for this reason.
  | Shown Bool
    -- ^ The app came in front of the user, or went away.
  | FullScreenChecked Bool
  | ProfilesChanged [Account]
  | ProxyKindChosen (Maybe Tox.ProxyType)
  | ProxyHostChanged Text
  | ProxyPortChanged Text
  | ApplyProxy
  | SendPictures ContactId (Maybe Text)
    -- ^ How pictures go to the friend from now on, by the key of a way.
  | Attach FilePath Text Word64 BS.ByteString
    -- ^ The user picked a file for the open chat: where it is kept, its
    -- name and size, and a new ID for it.
  | Unstage ContactId Int
    -- ^ Take something picked away again, by its place among those.
  | FileStarted ContactId MessageId FileRef
    -- ^ Tox offers the friend our file.
  | FileOffered ContactId FileRef UTCTime Text Word64 FilePath BS.ByteString
    -- ^ The friend offers a file: its name and size, where it would go, and
    -- its ID.
  | Download ContactId MessageId
  | Cancel ContactId MessageId
  | FileMoved ContactId FileRef Word64
    -- ^ How much of the file went or came so far.
  | FileEnded ContactId FileRef Bool
    -- ^ The transfer is over: whether all of the file went or came.
  | StartCall UTCTime Bool
    -- ^ Call the friend of the open chat, with the camera on or off.
  | AnswerCall UTCTime Bool
  | HangUp UTCTime
  | CallElsewhere UTCTime
    -- ^ Another profile has a call, and the device takes one at a time.
  | ToggleMute
  | ToggleCamera
  | HideCall
  | ShowCall
  | RoutesChanged Route [Route]
    -- ^ Where the call's audio goes now, and where it could.
  | FlipCamera
  | CallInvited ContactId UTCTime Bool
    -- ^ The friend calls us, sending video or not.
  | CallPeer ContactId UTCTime Bool
    -- ^ The friend answered our call, or now sends video or not.
  | CallEnded ContactId UTCTime
  | CallGlared ContactId UTCTime
    -- ^ Tox failed the call, as it does one both ends placed at once.
  | Tick UTCTime
    -- ^ Time passes during a call.
  | Notify Text
  | AvatarPicked BS.ByteString
    -- ^ Ours to show, as a PNG.
  | RemoveAvatar
  | FriendAvatar ContactId (Maybe BS.ByteString)
    -- ^ What the friend shows, as a PNG, or that it shows none.
  | SetAutoAccept Word64
  | ToggleFormat Text
    -- ^ Read a format, by its key, or not.
  | SetDevice DeviceSettings
  | ShareLocation Double Double
    -- ^ Where the user is, for the open chat.
  | Invited ContactId GroupId GroupKind Text BS.ByteString
    -- ^ The friend invites us to a group: its name and the invite.
  | MembersListed GroupId UTCTime [(Tox.PeerId, Member)]
    -- ^ Who is in a conference now, without us.
  | PasswordChanged Text
  | AnswerInvite ContactId GroupId Bool
    -- ^ Join, with the password typed, or decline.
  | GroupConnected GroupId UTCTime
    -- ^ Tox has the group's state from its peers.
  | GroupRefused GroupId Tox.JoinFail
  | MemberJoined GroupId UTCTime Tox.PeerId Member
  | MemberLeft GroupId UTCTime Tox.PeerId Tox.ExitType
  | MemberNamed GroupId Tox.PeerId Text
  | GroupReceived GroupId UTCTime Tox.PeerId Tox.MessageType Text
  | GroupSent GroupId MessageId
  | GroupSaved GroupId BS.ByteString
  | GroupNamed GroupId Text
  | LeaveGroup GroupId
  | DeleteGroup GroupId
  | MuteGroup GroupId (Maybe UTCTime)
  | UnmuteGroup GroupId
  | ToggleMentions GroupId
  | TopicChanged GroupId UTCTime (Maybe Tox.PeerId) Text
    -- ^ By a member, if Tox knows which.
  | TopicSetByUs GroupId UTCTime Text
  | TopicAllowed GroupId Bool
  | TopicEdited Text
  | ApplyTopic
  | FoundingChosen Founding
  | GroupNameEdited Text
  | Found
  | GroupCreated GroupId GroupKind Text UTCTime
    -- ^ Tox founded it, with us in it.
  | ChatIdEdited Text
  | JoinPublic
  | InviteFriend GroupId ContactId
  | PrivateReceived GroupId UTCTime Tox.PeerId Tox.MessageType Text
  | PrivatelyTo (Maybe Tox.PeerId)
    -- ^ Write to a member of the open group alone, or to all again.
  | RolesKnown GroupId (Maybe Tox.Role) [(Tox.PeerId, Tox.Role)]
    -- ^ Ours and the members'.
  | GroupModerated GroupId UTCTime (Maybe Tox.PeerId) (Maybe Tox.PeerId) Tox.Moderation
    -- ^ Who did it to whom: us, if none.
  | SettingsKnown GroupId Tox.GroupSettings
  | Moderate GroupId Tox.PeerId MemberAction
  | ChangeSettings GroupId Tox.GroupSettings
  | PeerLimitEdited Text
  | GroupPasswordEdited Text
  | ApplyGroupSettings
  | ChooseRelayBot GroupId (Maybe Text)
  | SyncReceived ContactId BS.ByteString
    -- ^ A sync packet from a friend, which only a linked account's home
    -- sends.
  | LinkEdited Text
    -- ^ The key of the member to read as a relay bot, or none.
  deriving (Eq, Show)

-- | What Tox is to do.
data Command
  = SetProfile Text Text
    -- ^ Our nickname and status message.
  | Befriend ContactId
    -- ^ Without a request: to accept theirs, or to know a friend again.
  | Unfriend ContactId
  | SendAvatar ContactId (Maybe BS.ByteString)
    -- ^ Our picture as a PNG, or that we show none.
  | RequestFriend Text Text
    -- ^ A Tox ID, and what the request says.
  | SendMessage ContactId MessageId Text Int
    -- ^ With how many of its parts the friend confirmed.
  | SendFile ContactId MessageId FilePath Text Word64 (Maybe BS.ByteString)
  | AcceptFile ContactId FileRef FilePath
    -- ^ Into where it is kept, going on after what is there.
  | CancelFile ContactId FileRef
  | ForgetFile FilePath
    -- ^ Our copy of a file picked and taken away again.
  | CallFriend ContactId Bool
    -- ^ With the camera on or off, like the rest of the calls.
  | AnswerFriend ContactId Bool
  | HangUpCall ContactId
  | CameraCall ContactId Bool
  | JoinGroup ContactId GroupId BS.ByteString Text Text
    -- ^ Through the friend's invite, with our name there and a password.
  | JoinConference ContactId GroupId BS.ByteString
  | RestoreGroup GroupKind GroupId BS.ByteString
  | SendGroupMessage GroupId MessageId Text
  | QuitGroup GroupId
  | SetGroupTopic GroupId Text
  | FoundGroup Founding Text Text
    -- ^ With its name and ours in it.
  | JoinPublicGroup GroupId Text Text
    -- ^ With our name in it and its password.
  | InviteToGroup GroupId ContactId
  | SendPrivate GroupId MessageId Tox.PeerId Text
  | ModerateMember GroupId Tox.PeerId MemberAction
  | SetGroupSettings GroupId Tox.GroupSettings
  | SendSync ContactId [BS.ByteString]
    -- ^ Sync packets for the home device of a linked account.
  deriving (Eq, Show)

toxIdLength :: Int
toxIdLength = 76

-- | The Tox ID of a public key and nospam, both in hex, with its checksum.
toxId :: Text -> Text -> Text
toxId key nospam = Text.toUpper (address <> hex (checksum (unhex address)))
  where address = key <> nospam

-- | The two bytes that the bytes before them XOR to, in pairs.
checksum :: BS.ByteString -> BS.ByteString
checksum bytes = BS.pack [foldr xor 0 evens, foldr xor 0 odds]
  where
    indexed = zip [0 :: Int ..] (BS.unpack bytes)
    evens = [b | (i, b) <- indexed, even i]
    odds = [b | (i, b) <- indexed, odd i]

hex :: BS.ByteString -> Text
hex = Text.decodeUtf8 . Base16.encode

-- | Hex as bytes, or none if it isn't hex.
unhex :: Text -> BS.ByteString
unhex = fromRight BS.empty . Base16.decode . Text.encodeUtf8

-- | The proxy as edited, or why it can't be.
editedProxy :: Model -> Either Text (Maybe Tox.Proxy)
editedProxy model = case modelProxyKind model of
  Nothing -> Right Nothing
  Just kind
    | Text.null (modelProxyHost model) -> Left "Which host is the proxy on?"
    | Just port <- readMaybe (Text.unpack (modelProxyPort model)) :: Maybe Integer
    , port > 0 && port < 65536 ->
        Right (Just (Tox.Proxy kind (Text.unpack (modelProxyHost model)) (fromIntegral port)))
    | otherwise -> Left "The port is a number from 1 to 65535"

-- | Why the address isn't a Tox ID of someone else than us, if it isn't.
-- Nothing while it's empty, as that is no mistake yet.
addressError :: Text -> Text -> Maybe Text
addressError ours address
  | Text.null address = Nothing
  | Text.length address /= toxIdLength || not (Text.all isHexDigit address) =
      Just $ "Must be " <> Text.pack (show toxIdLength) <> " hexadecimal characters"
  | toxId (Text.take 64 address) (Text.take 8 (Text.drop 64 address))
      /= Text.toUpper address =
      Just "Not a valid Tox ID, please check it for typos"
  | Text.toUpper (Text.take 64 address) == Text.toUpper (Text.take 64 ours) = Just "This is your own Tox ID"
  | otherwise = Nothing

-- | Why Tox wouldn't send a friend request saying this, if it wouldn't.
requestError :: Text -> Maybe Text
requestError request
  | Text.null (Text.strip request) = Just "The message can't be empty"
  | BS.length (Text.encodeUtf8 (Text.strip request)) > Tox.maxFriendRequestLength = Just "The message is too long"
  | otherwise = Nothing

-- | Why the edited nickname and status message can't be applied, if they
-- can't: Tox takes only so many bytes of each.
profileError :: Text -> Text -> Maybe Text
profileError nickname status
  | bytes nickname > Tox.maxNameLength = Just "The nickname is too long"
  | bytes status > Tox.maxStatusMessageLength = Just "The status message is too long"
  | otherwise = Nothing
  where bytes = BS.length . Text.encodeUtf8 . Text.strip

-- | What a friend request says unless the user writes something else.
requestMessage :: Profile -> Text
requestMessage p =
  "Hey, I'm " <> profileNickname p <> ", come chat with me on Tox!"

-- | No friends yet, before Tox has started.
initial :: Model
initial = Model
  { modelScreen = Contacts
  , modelShown = False
  , modelDrawer = False
  , modelProfile = profile
  , modelConnection = Offline
  , modelContacts = Map.empty
  , modelRequests = Map.empty
  , modelFormer = Map.empty
  , modelDeclined = Map.empty
  , modelDrafts = Map.empty
  , modelGroups = Map.empty
  , modelInvites = Map.empty
  , modelGroupDrafts = Map.empty
  , modelPassword = ""
  , modelTopic = ""
  , modelFounding = PrivateGroup
  , modelGroupName = ""
  , modelChatId = ""
  , modelPrivateTo = Nothing
  , modelPeerLimit = ""
  , modelGroupPassword = ""
  , modelNickname = profileNickname profile
  , modelStatus = profileStatus profile
  , modelAddress = ""
  , modelRequest = requestMessage profile
  , modelAddError = Nothing
  , modelProxyKind = Nothing
  , modelProxyHost = ""
  , modelProxyPort = ""
  , modelTransfers = Map.empty
  , modelCall = Nothing
  , modelFullScreen = True
  , modelProfileId = ProfileId 0
  , modelProfiles = []
  , modelAway = False
  , modelUnseen = 0
  , modelJump = 0
  , modelSelected = Set.empty
  , modelStaged = Map.empty
  , modelReplies = Map.empty
  , modelAvatar = Nothing
  , modelAvatars = Map.empty
  , modelAccount = AccountSettings autoAccepted Set.empty
  , modelDevice = DeviceSettings True True True True Nothing
  , modelNotice = Nothing
  , modelLinked = Nothing
  , modelLink = ""
  }
  where
    profile = Profile
      { profileNickname = "Tox User"
      , profileStatus = "Toxing on Haskell"
      , profileToxId = ""
      }

update :: Event -> Model -> (Model, [Command])
update event model = case modelLinked model of
  Just l  -> linkedUpdate l event model
  Nothing -> ownUpdate event model

-- | A profile's update where this device runs its account on Tox.
ownUpdate :: Event -> Model -> (Model, [Command])
ownUpdate event model = case event of
  Open screen -> only $ opened screen model
    { modelScreen = screen, modelDrawer = False, modelNotice = Nothing, modelSelected = Set.empty
    , modelAway = False, modelUnseen = 0
    -- Whom a group's draft goes to holds between its chat and its page.
    , modelPrivateTo = case (modelScreen model, screen) of
        (GroupDetails a, GroupChat b) | a == b -> modelPrivateTo model
        (GroupChat a, GroupDetails b) | a == b -> modelPrivateTo model
        _                                      -> Nothing }
  -- Only a linked account's home sends them.
  SyncReceived _ _ -> only model
  LinkEdited link -> only model { modelLink = link, modelNotice = Nothing }
  Back -> only $ fromMaybe model (back model { modelNotice = Nothing })
  Notify notice -> only model { modelNotice = Just notice }
  AvatarPicked png ->
    let hash = SHA256.hash png
    in ( pruned model { modelAvatar = Just hash, modelAvatars = Map.insert hash png (modelAvatars model) }
       , [SendAvatar cid (Just png) | (cid, c) <- Map.toList (modelContacts model), contactOnline c] )
  RemoveAvatar ->
    ( pruned model { modelAvatar = Nothing }
    , [SendAvatar cid Nothing | (cid, c) <- Map.toList (modelContacts model), contactOnline c] )
  FriendAvatar cid png -> only $ pruned $
    (adjusted cid (\c -> c { contactAvatar = SHA256.hash <$> png }))
      { modelAvatars = maybe id (\p -> Map.insert (SHA256.hash p) p) png (modelAvatars model) }
  SetAutoAccept size -> only model { modelAccount = (modelAccount model) { settingAutoAccept = size } }
  -- Theirs read again, as Tox carried them if the format is now off.
  ToggleFormat key ->
    let account = modelAccount model
        off = settingFormatsOff account
        account' = account { settingFormatsOff = (if Set.member key off then Set.delete key else Set.insert key) off }
        reread m = case messageRaw m of
          Just raw | not (messageMine m) -> m { messageText = readText (readWith (readable account') raw) }
          _ -> m
    in only model
         { modelAccount = account'
         , modelContacts = (\c -> c { contactMessages = map reread (contactMessages c) }) <$> modelContacts model
         , modelFormer = (\c -> c { contactMessages = map reread (contactMessages c) }) <$> modelFormer model }
  SetDevice device -> only model { modelDevice = device }
  ShareLocation latitude longitude -> only $ (staging (StagedPlace latitude longitude)) { modelNotice = Nothing }
  OpenDrawer -> only model { modelDrawer = True }
  DraftChanged draft -> only $ case modelScreen model of
    Chat cid      -> model { modelDrafts = Map.insert cid draft (modelDrafts model) }
    GroupChat gid -> model { modelGroupDrafts = Map.insert gid draft (modelGroupDrafts model) }
    _             -> model
  Send time -> case modelScreen model of
    GroupChat gid
      | Just g <- Map.lookup gid (modelGroups model)
      , groupPhase g `elem` [Joining, InGroup]
      , Just draft <- Map.lookup gid (modelGroupDrafts model)
      , not (Text.null (Text.strip draft)) ->
          let text = Text.strip draft
              i = length (groupEntries g)
              -- To a member alone, while they are there.
              privately = [ (n, m) | Just n <- [modelPrivateTo model], Just m <- [Map.lookup n (groupMembers g)] ]
              (written, commands') = case privately of
                (n, m) : _ -> (PrivateTo m Tox.MessageNormal text Pending, [SendPrivate gid i n text])
                [] -> (Said Nothing Tox.MessageNormal text Pending, [SendGroupMessage gid i text | groupPhase g == InGroup])
          in ( (withGroup gid g { groupEntries = groupEntries g ++ [GroupEntry time written] })
                 { modelGroupDrafts = Map.delete gid (modelGroupDrafts model)
                 , modelJump = modelJump model + 1
                 , modelUnseen = 0 }
             , commands' )
    -- What was picked first, then the text.
    Chat cid
      | Just contact <- Map.lookup cid (modelContacts model)
      , let staged = Map.findWithDefault [] cid (modelStaged model)
            draft = maybe "" Text.strip (Map.lookup cid (modelDrafts model))
      , not (null staged) || not (Text.null draft) ->
          let replied = Map.lookup cid (modelReplies model) >>= \i -> listToMaybe (drop i (contactMessages contact))
              message = maybe id (\m t -> quoted m <> "\n" <> t) replied draft
              picked = \case
                StagedFile path name size fileId -> ours name time (Just (Attachment size path 0 Waiting (Just fileId)))
                StagedPlace latitude longitude   -> ours (locationText latitude longitude) time Nothing
              sent = map picked staged ++ [ours message time Nothing | not (Text.null draft)]
          in
          ( (withContact cid contact { contactMessages = contactMessages contact ++ sent })
              { modelDrafts = Map.delete cid (modelDrafts model)
              , modelStaged = Map.delete cid (modelStaged model)
              , modelReplies = if Text.null draft then modelReplies model else Map.delete cid (modelReplies model)
              -- Which the user then sees, wherever the chat was.
              , modelJump = modelJump model + 1
              , modelUnseen = 0 }
          , [ case messageFile m of
                Just a  -> SendFile cid i (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
                Nothing -> SendMessage cid i (messageText m) 0
            | contactOnline contact, (i, m) <- zip [length (contactMessages contact) ..] sent ]
          )
    _ -> only model
  NicknameChanged nickname -> only model { modelNickname = nickname }
  StatusChanged status -> only model { modelStatus = status }
  ApplyProfile
    | Nothing <- profileError (modelNickname model) (modelStatus model) ->
        let applied = (modelProfile model)
              { profileNickname = Text.strip (modelNickname model)
              , profileStatus = Text.strip (modelStatus model)
              }
        in (model { modelProfile = applied }, [setProfile applied])
    | otherwise -> only model
  AddressChanged address -> only model { modelAddress = withoutScheme (Text.strip address), modelAddError = Nothing }
  RequestChanged request -> only model { modelRequest = request }
  AddFriend
    | not (Text.null address)
    , Nothing <- addressError (profileToxId (modelProfile model)) address
    , Nothing <- requestError (modelRequest model) ->
        let cid = ContactId (Text.toUpper (Text.take 64 address))
            request = (Text.toUpper address, Text.strip (modelRequest model))
            requesting c = c { contactRequest = Just request }
        in ( model
               { modelScreen = Chat cid
               , modelContacts = Map.alter (Just . requesting . fromMaybe (known cid)) cid (modelContacts model)
               , modelFormer = Map.delete cid (modelFormer model)
               , modelRequests = Map.delete cid (modelRequests model)
               , modelDeclined = Map.delete cid (modelDeclined model)
               , modelAddress = ""
               }
           , [uncurry RequestFriend request]
           )
    | otherwise -> only model
    where address = modelAddress model
  AddFailed address reason ->
    let cid = ContactId (Text.toUpper (Text.take 64 address))
        forget c
          | null (contactMessages c) = Nothing
          | otherwise = Just c { contactRequest = Nothing }
    in only model
         { modelScreen = AddContact
         , modelContacts = Map.update forget cid (modelContacts model)
         , modelAddress = address
         , modelAddError = Just reason
         }
  Accept cid
    | Map.member cid (modelRequests model) ->
        ( (withContact cid (known cid))
            { modelRequests = Map.delete cid (modelRequests model)
            , modelFormer = Map.delete cid (modelFormer model)
            , modelDeclined = Map.delete cid (modelDeclined model) }
        , [Befriend cid]
        )
    | otherwise -> only model
  Reject cid -> only model
    { modelRequests = Map.delete cid (modelRequests model)
    , modelDeclined = maybe id (Map.insert cid) (Map.lookup cid (modelRequests model)) (modelDeclined model)
    }
  Started address ->
    ( model { modelProfile = (modelProfile model) { profileToxId = address }
            -- Tox has them again only once it hears from their peers.
            , modelGroups = (\g -> g { groupPhase = if groupPhase g == InGroup then Joining else groupPhase g
                                     , groupMembers = Map.empty, groupSince = Nothing, groupRoles = Map.empty
                                     , groupSelfRole = Nothing, groupSettings = Nothing }) <$> modelGroups model }
    , setProfile (modelProfile model)
        : [ maybe (Befriend cid) (uncurry RequestFriend) (contactRequest c)
          | (cid, c) <- Map.toList (modelContacts model) ]
        -- After the friends, whom they may find their peers through.
        ++ [ RestoreGroup (groupKind g) gid saved
           | (gid, g) <- Map.toList (modelGroups model), groupPhase g `elem` [Joining, InGroup]
           , Just saved <- [groupSaved g] ]
    )
  Connected connection -> only model { modelConnection = connection }
  FriendOnline cid online -> case Map.lookup cid (modelContacts model) of
    Just contact ->
      -- Tox ends a friend's transfers when it goes offline: ours it hadn't
      -- taken yet are offered again when it is back.
      let (ended, going) = Map.partitionWithKey (\(c, _) _ -> c == cid && not online) (modelTransfers model)
          cut = foldr (\i -> adjustMessage cid i interrupted) model { modelTransfers = going } (Map.elems ended)
          -- Tox also reports a friend online when it moves between TCP and UDP.
          returned = online && not (contactOnline contact)
          met c = c { contactOnline = online, contactRequest = if online then Nothing else contactRequest c
                    , contactStatus = if online then contactStatus c else Tox.Online }
          -- Answered once the friend hears it.
          spent (c, _) invite = c /= cid || not returned || isNothing (inviteAnswered invite)
      in ( cut { modelContacts = Map.adjust met cid (modelContacts cut)
               , modelInvites = Map.filterWithKey spent (modelInvites cut) }
         , [ SendMessage cid i (messageText m) (messageParts m)
           | returned
           , (i, m) <- zip [0 ..] (contactMessages contact)
           , messageMine m, messageDelivery m /= Delivered, Nothing <- [messageFile m] ]
           ++
           [ SendAvatar cid (Just png) | returned, Just png <- [(`Map.lookup` modelAvatars model) =<< modelAvatar model] ]
           ++
           [ SendFile cid i (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
           | returned
           , (i, m) <- zip [0 ..] (contactMessages contact)
           , messageMine m, Just a <- [messageFile m], attachmentState a == Waiting ]
           -- Invites taken while the friend was away, which Tox can
           -- answer only to them.
           ++
           [ joinCommand cid gid invite password
           | returned
           , ((c, gid), invite) <- Map.toList (modelInvites model), c == cid
           , Just password <- [inviteAnswered invite] ]
         )
    Nothing -> only model
  FriendNamed cid name -> only $ adjusted cid (\c -> c { contactName = Just name })
  FriendStatusMessage cid status -> only $ adjusted cid (\c -> c { contactStatusMessage = status })
  FriendStatus cid status -> only $ adjusted cid (\c -> c { contactStatus = status })
  DeleteContact cid keep -> case Map.lookup cid (modelContacts model) of
    Just contact ->
      let calling = [c | Just c <- [modelCall model], callContact c == cid]
      in ( model
             { modelScreen = Contacts
             , modelContacts = Map.delete cid (modelContacts model)
             , modelFormer = if keep
                 then Map.insert cid contact { contactOnline = False, contactUnread = 0, contactRequest = Nothing }
                        (modelFormer model)
                 else modelFormer model
             , modelDrafts = Map.delete cid (modelDrafts model)
             , modelStaged = Map.delete cid (modelStaged model)
             , modelInvites = Map.filterWithKey (\(c, _) _ -> c /= cid) (modelInvites model)
             , modelTransfers = Map.filterWithKey (\(c, _) _ -> c /= cid) (modelTransfers model)
             , modelCall = if null calling then modelCall model else Nothing
             }
         , [HangUpCall cid | not (null calling)] ++ [Unfriend cid]
           ++ [ForgetFile path | StagedFile path _ _ _ <- Map.findWithDefault [] cid (modelStaged model)] )
    Nothing -> only model
  MuteChat cid until' -> only $ adjusted cid (\c -> c { contactMute = Just (maybe MutedAlways MutedUntil until') })
  UnmuteChat cid -> only $ adjusted cid (\c -> c { contactMute = Nothing })
  Received cid time kind bytes
    -- Some clients send a message again: one with the key of an earlier one
    -- is that one.
    | Just key <- readRepeat got
    , Just c <- Map.lookup cid (modelContacts model)
    , key `elem` [ k | m <- contactMessages c, not (messageMine m), Just raw <- [messageRaw m]
                     , Just k <- [readRepeat (readWith (readable (modelAccount model)) raw)] ] -> only model
    | otherwise -> only $ (adjusted cid $ \c -> c
        { contactMessages = contactMessages c ++ [Message False (readText got) time Delivered Nothing Nothing 0 Nothing kind (Just bytes)]
        , contactUnread = contactUnread c + (if reading cid then 0 else 1)
        }) { modelUnseen = modelUnseen model + (if modelScreen model == Chat cid && modelAway model then 1 else 0) }
    where got = readWith (readable (modelAccount model)) bytes
  Delivering cid i delivery -> only $ adjusted cid $ \c -> c
    { contactMessages =
        [ if j == i && messageDelivery m /= Delivered then m { messageDelivery = delivery } else m
        | (j, m) <- zip [0 ..] (contactMessages c) ]
    }
  Confirmed cid i n time -> only $ adjustMessage cid i (\m ->
    let confirmed = max n (messageParts m)
        whole = confirmed >= length (parts (messageText m))
    in m { messageParts = confirmed
         , messageDelivery = if whole then Delivered else messageDelivery m
         , messageDelivered = if whole && isNothing (messageDelivered m) then Just time else messageDelivered m
         }) model
  Select i -> only model
    { modelSelected = (if Set.member i (modelSelected model) then Set.delete else Set.insert) i (modelSelected model) }
  Unselect -> only model { modelSelected = Set.empty }
  ScrolledAway away -> only model { modelAway = away, modelUnseen = if away then modelUnseen model else 0 }
  JumpToEnd -> only model { modelJump = modelJump model + 1, modelUnseen = 0 }
  Reply i -> only $ case modelScreen model of
    Chat cid | Just m <- messageOf cid i, isNothing (messageCall m) ->
      model { modelReplies = Map.insert cid i (modelReplies model), modelSelected = Set.empty }
    _ -> model
  CancelReply -> only $ case modelScreen model of
    Chat cid -> model { modelReplies = Map.delete cid (modelReplies model) }
    _        -> model
  ForwardTo target time -> case (modelScreen model, Map.lookup target (modelContacts model)) of
    (Forward source is, Just contact) ->
      let start = length (contactMessages contact)
          sent = [ m | i <- is, Just m <- [messageOf source i], forwardable m ]
          copies = zipWith (\j m -> (j, copy m)) [start ..] sent
          copy m = ours (messageText m) time (forwardedFile <$> messageFile m)
          -- The same file, offered anew.
          forwardedFile a = a { attachmentState = Waiting, attachmentDone = 0, attachmentId = Nothing }
      in ( (withContact target contact { contactMessages = contactMessages contact ++ map snd copies })
             { modelScreen = Chat target, modelSelected = Set.empty }
         , [ case messageFile m of
               Nothing -> SendMessage target j (messageText m) 0
               Just a  -> SendFile target j (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
           | contactOnline contact, (j, m) <- copies ] )
    _ -> only model
  Requested cid message
    | Map.member cid (modelContacts model) -> (model, [Befriend cid])
    | Map.lookup cid (modelDeclined model) == Just message -> only model
    | otherwise -> only model { modelRequests = Map.insert cid message (modelRequests model) }
  Attach path name size fileId -> case modelScreen model of
    Chat _ -> only (staging (StagedFile path name size fileId))
    -- Picked as the user went elsewhere: the copy is of no use.
    _      -> (model, [ForgetFile path])
  Unstage cid k ->
    let staged = Map.findWithDefault [] cid (modelStaged model)
    in ( model { modelStaged = Map.filter (not . null) (Map.insert cid (take k staged ++ drop (k + 1) staged) (modelStaged model)) }
       , [ForgetFile path | StagedFile path _ _ _ <- take 1 (drop k staged)] )
  FileStarted cid i ref -> only model { modelTransfers = Map.insert (cid, ref) i (modelTransfers model) }
  FileOffered cid ref time name size path fileId -> case Map.lookup cid (modelContacts model) of
    -- The rest of a file of theirs that was cut off, going on in its message.
    Just contact
      | (i, a) : _ <- [ (i, a) | (i, m) <- zip [0 ..] (contactMessages contact), not (messageMine m)
                               , Just a <- [messageFile m], attachmentId a == Just fileId
                               , attachmentSize a == size, attachmentState a `elem` [Waiting, Interrupted] ] ->
          let taken = attachmentState a == Interrupted || size <= settingAutoAccept (modelAccount model)
          in ( (adjustFile cid i (\b -> b { attachmentState = if taken then Moving else Waiting }) model)
                 { modelTransfers = Map.insert (cid, ref) i (modelTransfers model) }
             , [AcceptFile cid ref (attachmentPath a) | taken] )
    Just contact ->
      let i = length (contactMessages contact)
          taken = size <= settingAutoAccept (modelAccount model)
          file = Attachment size path 0 (if taken then Moving else Waiting) (Just fileId)
      in ( (withContact cid contact
             { contactMessages = contactMessages contact ++ [Message False name time Delivered (Just file) Nothing 0 Nothing Tox.MessageNormal Nothing]
             , contactUnread = contactUnread contact + (if reading cid then 0 else 1)
             }) { modelTransfers = Map.insert (cid, ref) i (modelTransfers model) }
         , [AcceptFile cid ref path | taken]
         )
    Nothing -> (model, [CancelFile cid ref])
  Download cid i
    | Just ref <- liveRef cid i ->
        ( adjustFile cid i (\a -> a { attachmentState = Moving }) model
        , [AcceptFile cid ref (attachmentPath a) | Just a <- [fileOf cid i]] )
    | otherwise -> only model
  Cancel cid i ->
    ( adjustFile cid i (\a -> a { attachmentState = Cancelled })
        model { modelTransfers = maybe id (\ref -> Map.delete (cid, ref)) (liveRef cid i) (modelTransfers model) }
    , [CancelFile cid ref | Just ref <- [liveRef cid i]] )
  FileMoved cid ref done -> only $ case Map.lookup (cid, ref) (modelTransfers model) of
    Just i -> adjustFile cid i (\a -> a { attachmentDone = done, attachmentState = Moving }) model
    Nothing -> model
  FileEnded cid ref whole -> only $ case Map.lookup (cid, ref) (modelTransfers model) of
    Just i -> adjustFile cid i
      (\a -> if whole then a { attachmentDone = attachmentSize a, attachmentState = Complete }
             else a { attachmentState = Cancelled })
      model { modelTransfers = Map.delete (cid, ref) (modelTransfers model) }
    Nothing -> model
  StartCall time camera -> case modelScreen model of
    Chat cid
      | Nothing <- modelCall model
      , Just contact <- Map.lookup cid (modelContacts model)
      , contactOnline contact ->
          (begun (newCall cid Calling time camera) { callCamera = camera }, [CallFriend cid camera])
    _ -> only model
  AnswerCall time camera -> case modelCall model of
    Just c | callPhase c == Ringing ->
      ( model { modelCall = Just c { callPhase = Talking time, callCamera = camera } }
      , [AnswerFriend (callContact c) camera] )
    _ -> only model
  HangUp time -> case modelCall model of
    Just c  -> ((recorded time False c) { modelCall = Nothing }, [HangUpCall (callContact c)])
    Nothing -> only model
  -- Theirs missed, as while we have a call here, and ours not placed.
  CallElsewhere time -> case modelCall model of
    Just c | callPhase c == Ringing -> ((recorded time True c) { modelCall = Nothing }, [HangUpCall (callContact c)])
    Just c -> ( (recorded time False c) { modelCall = Nothing, modelNotice = Just "Another profile has a call" }
              , [HangUpCall (callContact c)] )
    Nothing -> only model
  ToggleMute -> only model { modelCall = (\c -> c { callMuted = not (callMuted c) }) <$> modelCall model }
  -- Tox changes what we send only once the call is going.
  ToggleCamera -> case modelCall model of
    Just c | Talking _ <- callPhase c ->
      let camera = not (callCamera c)
      in (model { modelCall = Just c { callCamera = camera } }, [CameraCall (callContact c) camera])
    _ -> only model
  HideCall -> only $ fromMaybe model (back model)
  ShowCall -> only model { modelCall = (\c -> c { callHidden = False }) <$> modelCall model }
  RoutesChanged route routes -> only model { modelCall = (\c -> c { callRoute = route, callRoutes = routes }) <$> modelCall model }
  FlipCamera -> only model { modelCall = (\c -> c { callBackCamera = not (callBackCamera c) }) <$> modelCall model }
  -- Tox had their call before ours went out, or settles a glare: it is
  -- the one call both want.
  CallInvited cid time video -> case modelCall model of
    Just c | callContact c == cid, callPhase c == Calling, callGlare c /= Settling ->
      ( model { modelCall = Just c { callPhase = Talking time, callPeerVideo = video, callGlare = Settling } }
      , [AnswerFriend cid (callCamera c)] )
    Nothing | Map.member cid (modelContacts model) ->
      only (begun (newCall cid Ringing time video) { callPeerVideo = video })
    _ -> (recorded time True (newCall cid Ringing time video), [HangUpCall cid])
  CallPeer cid time video -> only model
    { modelCall = case modelCall model of
        Just c | callContact c == cid -> Just c
          { callPeerVideo = video
          , callPhase = if callPhase c == Calling then Talking time else callPhase c
          }
        other -> other
    }
  CallEnded cid time -> only $ case modelCall model of
    Just c | callContact c == cid -> (recorded time (callPhase c == Ringing) c) { modelCall = Nothing }
    _ -> model
  CallGlared cid time -> case modelCall model of
    Just c | callContact c == cid, callPhase c == Calling, callGlare c == Clear ->
      only model { modelCall = Just c { callGlare = Glared time } }
    _ -> update (CallEnded cid time) model
  Tick time -> case modelCall model of
    Just c
      | Glared since <- callGlare c
      , let again = Text.toUpper (Text.take 64 (profileToxId (modelProfile model))) > Text.toUpper key
            ContactId key = callContact c
      , diffUTCTime time since >= (if again then glareDelay else glareWait) ->
          if again
            then (model { modelCall = Just c { callGlare = Settling } }, [CallFriend (callContact c) (callCamera c)])
            else only (recorded time False c) { modelCall = Nothing }
    Just c
      | callPhase c `elem` [Calling, Ringing]
      , diffUTCTime time (callStarted c) >= ringTimeout ->
          ((recorded time (callPhase c == Ringing) c) { modelCall = Nothing }, [HangUpCall (callContact c)])
    _ -> only model
  FullScreenChecked allowed -> only model { modelFullScreen = allowed }
  ProfilesChanged accounts -> only model { modelProfiles = accounts }
  ProxyKindChosen kind -> only model { modelProxyKind = kind, modelNotice = Nothing }
  ProxyHostChanged host -> only model { modelProxyHost = Text.strip host, modelNotice = Nothing }
  ProxyPortChanged port -> only model { modelProxyPort = Text.strip port, modelNotice = Nothing }
  ApplyProxy -> only $ case editedProxy model of
    Left why -> model { modelNotice = Just why }
    Right proxy -> model { modelDevice = (modelDevice model) { settingProxy = proxy }, modelScreen = SettingsScreen }
  SendPictures cid way -> only (adjusted cid (\c -> c { contactPictures = way }))
  Shown shown -> only $ case modelScreen model of
    Chat cid | shown -> (adjusted cid (\c -> c { contactUnread = 0 })) { modelShown = True }
    GroupChat gid | shown -> (adjustedGroup gid (\g -> g { groupUnread = 0 })) { modelShown = True }
    _ -> model { modelShown = shown }
  Invited cid gid kind name bytes
    | Map.member cid (modelContacts model)
    , maybe True ((`notElem` [Joining, InGroup]) . groupPhase) (Map.lookup gid (modelGroups model)) ->
        only model { modelInvites = Map.insert (cid, gid) (Invite kind name bytes Nothing) (modelInvites model) }
    | otherwise -> only model
  PasswordChanged password -> only model { modelPassword = password }
  AnswerInvite cid gid join -> case Map.lookup (cid, gid) (modelInvites model) of
    Nothing -> only model
    Just invite
      | not join -> only model { modelInvites = Map.delete (cid, gid) (modelInvites model), modelScreen = Contacts }
      | otherwise ->
          let password = modelPassword model
              online = maybe False contactOnline (Map.lookup cid (modelContacts model))
              -- A group left before keeps its chat.
              before = Map.lookup gid (modelGroups model)
              joining = (fromMaybe (newGroup (inviteName invite) (inviteKind invite) Joining) before)
                { groupPhase = Joining, groupMembers = Map.empty, groupSaved = Nothing }
          in ( model
                 { modelInvites = if online then Map.delete (cid, gid) (modelInvites model)
                                  else Map.insert (cid, gid) invite { inviteAnswered = Just password } (modelInvites model)
                 , modelGroups = Map.insert gid joining (modelGroups model)
                 , modelScreen = GroupChat gid
                 , modelPassword = "" }
             , [joinCommand cid gid invite password | online] )
  GroupConnected gid time -> case Map.lookup gid (modelGroups model) of
    Just g | groupPhase g == Joining ->
      ( withGroup gid g { groupPhase = InGroup, groupSince = Just time }
        -- Those written while Tox wasn't in it.
      , [ SendGroupMessage gid i text
        | (i, GroupEntry _ (Said Nothing _ text Pending)) <- zip [0 ..] (groupEntries g) ] )
    _ -> only model
  GroupRefused gid why -> case Map.lookup gid (modelGroups model) of
    Just g | groupPhase g `elem` [Joining, InGroup] ->
      -- Tox keeps what it had of the group, to no use.
      (withGroup gid g { groupPhase = Refused why, groupMembers = Map.empty, groupSaved = Nothing }, [QuitGroup gid])
    _ -> only model
  MemberJoined gid time n member -> only $ adjustedGroup gid $ \g -> g
    { groupMembers = Map.insert n member (groupMembers g)
    , groupEntries = groupEntries g ++ [GroupEntry time (Arrived member) | arrived g time] }
  MemberLeft gid time n how -> only $ adjustedGroup gid $ \g -> g
    { groupMembers = Map.delete n (groupMembers g)
    , groupEntries = groupEntries g
        ++ [ GroupEntry time (Departed member)
           | groupPhase g == InGroup, how `notElem` [Tox.ExitSelfDisconnected, Tox.ExitKicked]
           , Just member <- [Map.lookup n (groupMembers g)] ] }
  MembersListed gid time listed -> case Map.lookup gid (modelGroups model) of
    Just g ->
      let keys = map memberKey . Map.elems
          now = Map.fromList listed
          came = [m | m <- Map.elems now, memberKey m `notElem` keys (groupMembers g)]
          went = [m | m <- Map.elems (groupMembers g), memberKey m `notElem` keys now]
      in ( withGroup gid g
             { groupMembers = now
             , groupEntries = groupEntries g
                 -- The first list after getting in is who was there.
                 ++ [ GroupEntry time e
                    | arrived g time, not (Map.null (groupMembers g)), e <- map Arrived came ++ map Departed went ] }
         -- Those that found no one to go to.
         , [ SendGroupMessage gid i text
           | groupPhase g == InGroup, not (Map.null now)
           , (i, GroupEntry _ (Said Nothing _ text Pending)) <- zip [0 ..] (groupEntries g) ] )
    Nothing -> only model
  MemberNamed gid n name -> only $ adjustedGroup gid $ \g -> g
    { groupMembers = Map.adjust (\m -> m { memberName = name }) n (groupMembers g) }
  GroupReceived gid time n kind text -> only $ (adjustedGroup gid $ \g -> g
    { groupEntries = groupEntries g ++ [GroupEntry time (Said (Just (memberOf g n)) kind text Delivered)]
    , groupUnread = groupUnread g + (if modelShown model && modelScreen model == GroupChat gid then 0 else 1) })
    { modelUnseen = modelUnseen model + (if modelScreen model == GroupChat gid && modelAway model then 1 else 0) }
  GroupSent gid i -> only $ adjustedGroup gid $ \g -> g
    { groupEntries = [ case e of
                         GroupEntry t (Said Nothing kind text Pending) | j == i -> GroupEntry t (Said Nothing kind text Sent)
                         GroupEntry t (PrivateTo who kind text Pending) | j == i -> GroupEntry t (PrivateTo who kind text Sent)
                         _ -> e
                     | (j, e) <- zip [0 ..] (groupEntries g) ] }
  PrivateReceived gid time n kind text -> only $ (adjustedGroup gid $ \g -> g
    { groupEntries = groupEntries g ++ [GroupEntry time (PrivateFrom (memberOf g n) kind text)]
    , groupUnread = groupUnread g + (if modelShown model && modelScreen model == GroupChat gid then 0 else 1) })
    { modelUnseen = modelUnseen model + (if modelScreen model == GroupChat gid && modelAway model then 1 else 0) }
  PrivatelyTo to -> only model { modelPrivateTo = to }
  RolesKnown gid self theirs -> only $ adjustedGroup gid $ \g -> g
    { groupSelfRole = self, groupRoles = Map.fromList theirs }
  GroupModerated gid time setter target moderation -> case Map.lookup gid (modelGroups model) of
    Just g ->
      let notice = GroupEntry time (Moderated (memberOf g <$> setter) (memberOf g <$> target) moderation)
          kicked = isNothing target && moderation == Tox.ModKick
      in if kicked
           -- What Tox kept of it is of no more use.
           then ( withGroup gid g { groupPhase = Kicked, groupEntries = groupEntries g ++ [notice]
                                  , groupMembers = Map.empty, groupSaved = Nothing }
                , [QuitGroup gid] )
           else only (withGroup gid g { groupEntries = groupEntries g ++ [notice] })
    Nothing -> only model
  SettingsKnown gid settings -> only $ adjustedGroup gid (\g -> g { groupSettings = Just settings })
  Moderate gid n permission -> (model, [ModerateMember gid n permission])
  ChangeSettings gid settings -> (model, [SetGroupSettings gid settings])
  PeerLimitEdited limit -> only model { modelPeerLimit = limit }
  GroupPasswordEdited password -> only model { modelGroupPassword = password }
  ApplyGroupSettings -> case modelScreen model of
    GroupSettingsScreen gid
      | Just g <- Map.lookup gid (modelGroups model), Just settings <- groupSettings g ->
          case readMaybe (Text.unpack (Text.strip (modelPeerLimit model))) :: Maybe Integer of
            Just limit | limit > 0 && limit < 65536
                       , BS.length (Text.encodeUtf8 (modelGroupPassword model)) <= Tox.maxPasswordSize ->
              ( model { modelScreen = GroupDetails gid }
              , [SetGroupSettings gid settings { Tox.groupPeerLimit = fromIntegral limit
                                               , Tox.groupPassword = Text.encodeUtf8 (modelGroupPassword model) }] )
            _ -> only model { modelNotice = Just "The limit is a number from 1 to 65535, the password at most 32 bytes" }
    _ -> only model
  ChooseRelayBot gid bot -> only $ adjustedGroup gid (\g -> g { groupRelayBot = bot })
  ToggleMentions gid -> only $ adjustedGroup gid (\g -> g { groupMentions = not (groupMentions g) })
  TopicChanged gid time setter topic -> only $ adjustedGroup gid $ \g -> g
    { groupTopic = topic
    -- Not the topic Tox learns as it gets into the group.
    , groupEntries = groupEntries g
        ++ [ GroupEntry time (TopicSet (Just (maybe (Member "" "") (memberOf g) setter)) topic)
           | arrived g time, topic /= groupTopic g ] }
  TopicSetByUs gid time topic -> only $ adjustedGroup gid $ \g -> g
    { groupTopic = topic, groupEntries = groupEntries g ++ [GroupEntry time (TopicSet Nothing topic)] }
  TopicAllowed gid allowed -> only $ adjustedGroup gid (\g -> g { groupMayTopic = allowed })
  TopicEdited topic -> only model { modelTopic = topic }
  FoundingChosen founding -> only model { modelFounding = founding }
  GroupNameEdited name -> only model { modelGroupName = name }
  Found
    | Nothing <- foundingError (modelFounding model) (modelGroupName model) ->
        ( model { modelScreen = Contacts }
        , [FoundGroup (modelFounding model) (Text.strip (modelGroupName model)) (groupNick (modelProfile model))] )
    | otherwise -> only model { modelNotice = foundingError (modelFounding model) (modelGroupName model) }
  GroupCreated gid kind name time ->
    only (withGroup gid (newGroup name kind InGroup) { groupSince = Just time, groupSelfRole = Just Tox.Founder })
      { modelScreen = GroupChat gid }
  ChatIdEdited chatId -> only model { modelChatId = Text.toUpper (Text.strip chatId) }
  JoinPublic
    | not (Text.null chatId), Nothing <- chatIdError chatId ->
        let gid = GroupId chatId
            joining = maybe True ((`notElem` [Joining, InGroup]) . groupPhase) (Map.lookup gid (modelGroups model))
            fresh = newGroup "" NgcGroup Joining
        in ( model { modelGroups = if joining
                       then Map.insert gid (maybe fresh (\g -> g { groupPhase = Joining, groupSaved = Nothing })
                                              (Map.lookup gid (modelGroups model))) (modelGroups model)
                       else modelGroups model
                   , modelScreen = GroupChat gid, modelPassword = "" }
           , [JoinPublicGroup gid (groupNick (modelProfile model)) (modelPassword model) | joining] )
    | otherwise -> only model
    where chatId = modelChatId model
  InviteFriend gid cid -> (model, [InviteToGroup gid cid])
  ApplyTopic -> case modelScreen model of
    TopicScreen gid
      | Just g <- Map.lookup gid (modelGroups model), groupPhase g == InGroup, groupMayTopic g ->
          (model { modelScreen = GroupDetails gid }, [SetGroupTopic gid (Text.strip (modelTopic model))])
    _ -> only model
  GroupSaved gid saved -> only $ adjustedGroup gid $ \g ->
    if groupPhase g `elem` [Joining, InGroup] then g { groupSaved = Just saved } else g
  GroupNamed gid name -> only $ adjustedGroup gid $ \g -> g { groupName = name }
  LeaveGroup gid -> case Map.lookup gid (modelGroups model) of
    Just g | groupPhase g `elem` [Joining, InGroup] ->
      (withGroup gid g { groupPhase = Gone, groupMembers = Map.empty, groupSaved = Nothing }, [QuitGroup gid])
    _ -> only model
  DeleteGroup gid -> case Map.lookup gid (modelGroups model) of
    Just g ->
      ( model { modelGroups = Map.delete gid (modelGroups model)
              , modelGroupDrafts = Map.delete gid (modelGroupDrafts model)
              , modelScreen = Contacts }
      , [QuitGroup gid | groupPhase g `elem` [Joining, InGroup]] )
    Nothing -> only model
  MuteGroup gid until' -> only $ adjustedGroup gid (\g -> g { groupMute = Just (maybe MutedAlways MutedUntil until') })
  UnmuteGroup gid -> only $ adjustedGroup gid (\g -> g { groupMute = Nothing })
  where
    only m = (m, [])
    reading cid = modelShown model && modelScreen model == Chat cid
    newContact = Contact Nothing False [] 0 Nothing "" Tox.Online Nothing Nothing Nothing
    -- Without the pictures nobody shows any more.
    pruned m = m
      { modelAvatars = Map.restrictKeys (modelAvatars m) $ Set.fromList $
          maybe id (:) (modelAvatar m)
            [ h | c <- Map.elems (modelContacts m) ++ Map.elems (modelFormer m), Just h <- [contactAvatar c] ] }
    ours text time file = Message True text time Pending file Nothing 0 Nothing Tox.MessageNormal Nothing
    messageOf cid i = listToMaybe . drop i . contactMessages =<< Map.lookup cid (modelContacts model)
    -- One deleted with its chat kept comes back with it.
    known cid = fromMaybe newContact (Map.lookup cid (modelFormer model))
    -- As QR codes and links have it.
    withoutScheme a
      | Text.toLower (Text.take 4 a) == "tox:" = Text.drop 4 a
      | otherwise = a
    -- A video call is held away from the ear, so its audio starts on the
    -- loudspeaker.
    newCall cid phase time video =
      Call cid phase time (phase == Calling) video False False False
        (if video then Speaker else Earpiece) [Earpiece, Speaker] False False Clear Nothing
    entry c record = Message (callMine c) "" (callStarted c) Delivered Nothing (Just record) 0 Nothing Tox.MessageNormal Nothing
    -- The call going on, with its entry in the chat.
    begun c = (adjusted (callContact c) $ \contact -> contact
        { contactMessages = contactMessages contact ++ [entry c (CallRecord (callVideo c) Nothing False)] })
      { modelCall = Just c { callEntry = length . contactMessages <$> Map.lookup (callContact c) (modelContacts model) } }
    -- The call's entry in its chat as it ends then, which is unread if missed.
    recorded time missed c = adjusted (callContact c) $ \contact -> contact
      { contactMessages = case callEntry c of
          Just i  -> [ if j == i then entry c record else m | (j, m) <- zip [0 ..] (contactMessages contact) ]
          Nothing -> contactMessages contact ++ [entry c record]
      , contactUnread = contactUnread contact + (if missed && not (reading (callContact c)) then 1 else 0)
      }
      where
        record = CallRecord (callVideo c) talked missed
        talked = case callPhase c of
          Talking since -> Just (diffUTCTime time since)
          _             -> Nothing
    withContact cid contact = model { modelContacts = Map.insert cid contact (modelContacts model) }
    -- For the open chat's draft.
    staging item = case modelScreen model of
      Chat cid -> model { modelStaged = Map.insertWith (flip (++)) cid [item] (modelStaged model) }
      _        -> model
    withGroup gid g = model { modelGroups = Map.insert gid g (modelGroups model) }
    adjustedGroup gid f = model { modelGroups = Map.adjust f gid (modelGroups model) }
    -- How Tox takes an invite.
    joinCommand cid gid invite password = case inviteKind invite of
      NgcGroup -> JoinGroup cid gid (inviteData invite) (groupNick (modelProfile model)) password
      _        -> JoinConference cid gid (inviteData invite)
    -- Who comes after Tox met those there before.
    arrived g time = groupPhase g == InGroup && maybe False ((>= meeting) . diffUTCTime time) (groupSince g)
    -- Who Tox says sent a message, or no one known if it doesn't.
    memberOf g n = Map.findWithDefault (Member "" "") n (groupMembers g)
    adjusted cid f = model { modelContacts = Map.adjust f cid (modelContacts model) }
    fileOf cid i = messageFile =<< listToMaybe (drop i (maybe [] contactMessages (Map.lookup cid (modelContacts model))))
    liveRef cid i = case [ref | ((c, ref), j) <- Map.toList (modelTransfers model), c == cid, j == i] of
      ref : _ -> Just ref
      []      -> Nothing
    setProfile p = SetProfile (profileNickname p) (profileStatus p)

-- Linked accounts

-- | Why a pairing link can't link an account, if it can't.
linkError :: Text -> Maybe Text
linkError link
  | Text.null (Text.strip link) = Just "Paste the link the other device shows"
  | isNothing (Pairing.parseLink link) = Just "That isn't a pairing link"
  | otherwise = Nothing

-- | A linked profile's update: what the user does becomes events for the
-- home device, and what the chats show is derived from the account's
-- events. The device's own Tox has only the home as a friend.
linkedUpdate :: Linked -> Event -> Model -> (Model, [Command])
linkedUpdate l event model = case event of
  Started _ ->
    ( linkedView l model
    , [maybe (Befriend home) (RequestFriend (linkedHomeAddress l)) (Linked.linkedPairing l)] )
  FriendOnline cid online
    | cid == home && online -> (model, [SendSync home (Linked.homeOnline l)])
    | cid == home -> withLinked (Linked.homeOffline l) model []
  SyncReceived cid packet
    | cid == home -> let (replies, l') = Linked.receivePacket packet l in withLinked l' model replies
  Send time -> case modelScreen model of
    Chat cid
      | not (null (Map.findWithDefault [] cid (modelStaged model))) -> notYet "Files"
      | Just draft <- Text.strip <$> Map.lookup cid (modelDrafts model), not (Text.null draft) ->
          let replied = Map.lookup cid (modelReplies model) >>= \i ->
                listToMaybe . drop i . contactMessages =<< Map.lookup cid (modelContacts model)
              text = maybe id (\m t -> quoted m <> "\n" <> t) replied draft
              g = friendGraphOf cid
              (l', packets) = authored g time (Sync.Send (senderIn g) (Text.encodeUtf8 text) Sync.Normal) l
          in withLinked l' model
               { modelDrafts = Map.delete cid (modelDrafts model)
               , modelReplies = Map.delete cid (modelReplies model)
               , modelJump = modelJump model + 1, modelUnseen = 0 } packets
    GroupChat gid
      | isJust (modelPrivateTo model) -> notYet "Private messages"
      | Just c <- chatOf gid
      , Just draft <- Text.strip <$> Map.lookup gid (modelGroupDrafts model), not (Text.null draft) ->
          let g = Sync.chatGraph c
              (l', packets) = authored g time (Sync.Send (senderIn g) (Text.encodeUtf8 draft) Sync.Normal) l
          in withLinked l' model
               { modelGroupDrafts = Map.delete gid (modelGroupDrafts model)
               , modelJump = modelJump model + 1, modelUnseen = 0 } packets
    _ -> only model
  AddFriend
    | Nothing <- addressError (profileToxId (modelProfile model)) address
    , Nothing <- requestError (modelRequest model)
    , Just pk <- keyOf (Text.take 64 address) ->
        let nospam = nospamOf (Text.take 8 (Text.drop 64 address))
            (l', packets) = authored account now
              (Sync.WantFriend pk (Just (Text.strip (modelRequest model))) nospam) l
        in withLinked l' model { modelScreen = Contacts, modelAddress = "" } packets
    | otherwise -> ownUpdate event model
    where address = modelAddress model
  Accept cid -> friendWish cid True
  Reject cid -> friendWish cid False
  DeleteContact cid _ -> let (m, cs) = friendWish cid False in (m { modelScreen = Contacts }, cs)
  MuteChat cid until' -> setting (friendGraphOf cid) (Just (muteOf until' True))
  UnmuteChat cid -> setting (friendGraphOf cid) Nothing
  MuteGroup gid until' | Just c <- chatOf gid ->
    setting (Sync.chatGraph c) (Just (muteOf until' (maybe True groupMentions (Map.lookup gid (modelGroups model)))))
  UnmuteGroup gid | Just c <- chatOf gid -> setting (Sync.chatGraph c) Nothing
  ToggleMentions gid
    | Just c <- chatOf gid, Just g <- Map.lookup gid (modelGroups model), Just m <- groupMute g ->
        setting (Sync.chatGraph c) (Just (muteOf (case m of MutedUntil t -> Just t; MutedAlways -> Nothing)
                                                 (not (groupMentions g))))
  AnswerInvite _ (GroupId invite) join
    | Just i <- Sync.EventId <$> unhex' invite ->
        let password = if Text.null (modelPassword model) then Nothing else Just (modelPassword model)
            (l', packets) = authored account now (Sync.AnswerInvite i join password) l
        in withLinked l' model { modelScreen = Contacts, modelPassword = "" } packets
  LeaveGroup gid | Just c <- chatOf gid ->
    let (l', packets) = authored account now (Sync.Wish (Sync.ChatSubject c) (Sync.Joined False)) l
    in withLinked l' model { modelScreen = Contacts } packets
  ApplyProfile
    | Nothing <- profileError (modelNickname model) (modelStatus model) ->
        let (l1, p1) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Name (Text.strip (modelNickname model)))) l
            (l2, p2) = authored account now (Sync.Wish Sync.AccountSubject (Sync.StatusMessage (Text.strip (modelStatus model)))) l1
        in withLinked l2 model (p1 ++ p2)
  ApplyTopic | TopicScreen gid <- modelScreen model, Just c <- chatOf gid ->
    let (l', packets) = authored account now (Sync.Wish (Sync.ChatSubject c) (Sync.Topic (Text.strip (modelTopic model)))) l
    in withLinked l' model { modelScreen = GroupDetails gid } packets
  ChooseRelayBot gid bot | Just c <- chatOf gid ->
    case (bot, Map.lookup c (Sync.accountRelayBots (Linked.accountState l))) of
      (Just key, _) | Just pk <- keyOf key ->
        let (l', packets) = authored account now (Sync.RelayBot c pk True) l in withLinked l' model packets
      (Nothing, Just pk) ->
        let (l', packets) = authored account now (Sync.RelayBot c pk False) l in withLinked l' model packets
      _ -> only model
  -- From the device's own Tox, whose only friend is the home.
  FriendNamed {} -> only model
  FriendStatusMessage {} -> only model
  FriendStatus {} -> only model
  FriendAvatar {} -> only model
  Received {} -> only model
  Requested {} -> only model
  AddFailed {} -> only model
  Invited {} -> only model
  StartCall {} -> notYet "Calls"
  Attach {} -> notYet "Files"
  SendPictures {} -> notYet "Pictures"
  AvatarPicked {} -> notYet "Avatars"
  Found
    | Nothing <- foundingError (modelFounding model) (modelGroupName model) ->
        let (kind, privacy) = case modelFounding model of
              PrivateGroup  -> (Sync.GroupKind, Sync.Private)
              PublicGroup   -> (Sync.GroupKind, Sync.Public)
              NewConference -> (Sync.ConferenceKind, Sync.Public)
            (l', packets) = authored account now (Sync.NewChat kind (Text.strip (modelGroupName model)) privacy) l
        in withLinked l' model { modelScreen = Contacts } packets
  JoinPublic
    | Nothing <- chatIdError (modelChatId model), Just chatId <- unhex' (modelChatId model) ->
        let password = if Text.null (modelPassword model) then Nothing else Just (modelPassword model)
            (l', packets) = authored account now (Sync.JoinChat chatId password) l
        in withLinked l' model { modelScreen = Contacts, modelChatId = "", modelPassword = "" } packets
  InviteFriend gid (ContactId key) | Just c <- chatOf gid, Just friend <- keyOf key ->
    let (l', packets) = authored account now (Sync.InviteToChat c friend) l
    in withLinked l' model { modelNotice = Just "Invite sent" } packets
  Moderate {} -> notYet "Moderation"
  ApplyGroupSettings -> notYet "Group settings"
  _ -> let (m, _) = ownUpdate event model in readToEnd l m []
  where
    only m = (m, [])
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    account = Sync.accountGraph (Linked.accountKey l)
    -- What Tox does with no screen doesn't tell the time, and events only
    -- show theirs.
    now = linkedClock l
    friendGraphOf (ContactId key) = Sync.friendGraph (Linked.accountKey l) (fromMaybe (Sync.PublicKey BS.empty) (keyOf key))
    chatOf gid = listToMaybe [c | c <- Sync.joinedChats (Linked.accountState l), chatGroupId c == gid]
    senderIn = linkedSender l
    notYet what = only model { modelNotice = Just (what <> " aren't there for linked accounts yet") }
    friendWish (ContactId key) wanted = case keyOf key of
      Just pk ->
        let content = if wanted then Sync.WantFriend pk Nothing Nothing else Sync.UnwantFriend pk
            (l', packets) = authored account now content l
        in withLinked l' model packets
      Nothing -> only model
    setting g s' =
      let content = maybe (Sync.ClearSetting (Sync.ConversationScope g) 0) (Sync.SettingContent (Sync.ConversationScope g) . Sync.kept) s'
          (l', packets) = authored account now content l
      in withLinked l' model packets
    muteOf until' = Sync.Mute (fromIntegral . Linked.toMillis <$> until')
    nospamOf t = case unhex' t of
      Just bytes | BS.length bytes == 4 -> Just (BS.foldl' (\n b -> n * 256 + fromIntegral b) 0 bytes)
      _ -> Nothing

-- | The model with a linked account changed, and its packets for the home.
withLinked :: Linked -> Model -> [BS.ByteString] -> (Model, [Command])
withLinked l model = readToEnd l (linkedView l model { modelLinked = Just l })

-- | Marks the chat in front of the user read to its end, as an event the
-- other devices take.
readToEnd :: Linked -> Model -> [BS.ByteString] -> (Model, [Command])
readToEnd l model packets = case unreadEnd of
  Just (g, i, rank) ->
    let (l', more) = authored (Sync.accountGraph (Linked.accountKey l)) (linkedClock l) (Sync.ReadUpTo g i rank) l
    in (linkedView l' model { modelLinked = Just l' }, send (packets ++ more))
  Nothing -> (model, send packets)
  where
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    send ps = [SendSync home ps | not (null ps)]
    store = Replica.replicaStore (Linked.linkedReplica l)
    shown g = modelShown model && not (modelAway model) && case modelScreen model of
      Chat (ContactId key) -> Just g == (Sync.friendGraph (Linked.accountKey l) <$> keyOf key)
      GroupChat gid -> g `elem` [Sync.chatGraph c | c <- Sync.joinedChats (Linked.accountState l), chatGroupId c == gid]
      _ -> False
    unreadEnd = listToMaybe
      [ (g, i, Sync.eventRank e)
      | g <- Sync.storeGraphs store, shown g
      , (i, e) <- take 1 (reverse (Sync.byRank g store))
      , Just (Sync.eventRank e, i) > Map.lookup g (Sync.accountRead (Linked.accountState l)) ]

-- | An event by this device now, with its packets.
authored :: Sync.GraphId -> UTCTime -> Sync.Content -> Linked -> (Linked, [BS.ByteString])
authored g time content l = (l', packets)
  where (_, packets, l') = Linked.author g time content l

linkedHomeAddress :: Linked -> Text
linkedHomeAddress = Linked.linkedHome

-- | The latest time the account's events tell, for events whose time
-- nothing shows.
linkedClock :: Linked -> UTCTime
linkedClock l = maximum (Linked.linkedSince l : [Linked.eventTime e | g <- Sync.storeGraphs store, (_, e) <- take 1 (reverse (Sync.byRank g store))])
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | Our name in a conversation: our own one in a chat that has it, else the
-- account's.
linkedSender :: Linked -> Sync.GraphId -> Text
linkedSender l g = fromMaybe ourName (listToMaybe
  [n | c <- Sync.joinedChats state, Sync.chatGraph c == g, Sync.SelfName n <- Sync.factsOf (Sync.ChatSubject c) facts])
  where
    state = Linked.accountState l
    facts = Sync.accountFacts state
    ourName = fromMaybe "" (listToMaybe [n | Sync.Name n <- Sync.factsOf Sync.AccountSubject facts])

keyOf :: Text -> Maybe Sync.PublicKey
keyOf key = case unhex' key of
  Just bytes | BS.length bytes == 32 -> Just (Sync.PublicKey bytes)
  _                                  -> Nothing

unhex' :: Text -> Maybe BS.ByteString
unhex' = either (const Nothing) Just . Base16.decode . Text.encodeUtf8

hexOf :: BS.ByteString -> Text
hexOf = Text.toUpper . Text.decodeUtf8 . Base16.encode

chatGroupId :: Sync.ChatId -> GroupId
chatGroupId (Sync.GroupChat i)      = GroupId (hexOf i)
chatGroupId (Sync.ConferenceChat i) = GroupId (hexOf i)

-- | What a linked account's chats, friends, invites and profile are, from
-- its events and what the home says now.
linkedView :: Linked -> Model -> Model
linkedView l model = model
  { modelProfile = (modelProfile model)
      { profileNickname = fromMaybe "" (listToMaybe [n | Sync.Name n <- kept Sync.AccountSubject])
      , profileStatus = fromMaybe "" (listToMaybe [t | Sync.StatusMessage t <- kept Sync.AccountSubject])
      , profileToxId = fromMaybe (Text.toUpper (Linked.linkedHome l))
          (listToMaybe [hexOf a | Sync.Address a <- kept Sync.AccountSubject]) }
  , modelContacts = Map.fromList
      -- The account's devices are the home's friends too, this one even
      -- before the home names it.
      [ (contactIdOf pk, contact pk) | (pk, f) <- Map.toList (Sync.accountFriends state), Sync.friendWanted f
      , pk /= Replica.replicaDevice (Linked.linkedReplica l), Map.notMember pk (Sync.accountDevices state) ]
  , modelRequests = Map.fromList
      [ (contactIdOf pk, asked) | (pk, Sync.Friend False (Just asked)) <- Map.toList (Sync.accountFriends state) ]
  , modelGroups = Map.fromList [(chatGroupId c, group c) | c <- Sync.joinedChats state]
  , modelInvites = Map.fromList
      [ ((contactIdOf (Sync.inviteFrom v), GroupId (hexOf i)), invite v)
      | (Sync.EventId i, v) <- Map.toList (Sync.accountInvites state)
      , Sync.inviteStatus v `elem` [Sync.Pending, Sync.Joining] ]
  }
  where
    state = Linked.accountState l
    facts = Sync.accountFacts state
    live = Linked.liveFacts l
    store = Replica.replicaStore (Linked.linkedReplica l)
    account = Linked.accountKey l
    kept s = Sync.factsOf s facts
    contactIdOf (Sync.PublicKey pk) = ContactId (hexOf pk)
    memberKey' (Sync.PublicKey pk) = hexOf pk
    toxKind Sync.Action = Tox.MessageAction
    toxKind Sync.Normal = Tox.MessageNormal
    readable' = readable (modelAccount model)
    -- What a conversation holds, and what of ours went and arrived.
    timeline g = Sync.displayOrder g store
    went g = Set.fromList [i | (_, e) <- timeline g, Sync.Sent i <- [Sync.eventContent e]]
    arrived g = Map.fromList [(i, Linked.eventTime e) | (_, e) <- timeline g, Sync.Delivered i <- [Sync.eventContent e]]
    ended g = Map.fromList [(i, (d, declined)) | (_, e) <- timeline g, Sync.CallEnded i d declined <- [Sync.eventContent e]]
    delivery g i
      | Map.member i (arrived g) = Delivered
      | Set.member i (went g) = Sent
      | otherwise = Pending
    -- Theirs after the last read, since the link: earlier ones were for the
    -- other devices to tell of.
    unread g = length
      [ () | (i, e) <- timeline g
      , Just (Sync.eventRank e, i) > Map.lookup g (Sync.accountRead state)
      , Linked.eventTime e >= Linked.linkedSince l
      , Sync.Received {} <- [Sync.eventContent e] ]
    muteIn g = case Map.lookup 0 =<< Map.lookup (Sync.ConversationScope g) (Sync.accountSettings state) of
      Just (Sync.Kept (Sync.Mute until' mentioned) _) ->
        Just (maybe MutedAlways (MutedUntil . Linked.fromMillis . fromIntegral) until', mentioned)
      _ -> Nothing
    contact pk = Contact
      { contactName = listToMaybe [n | Sync.Name n <- kept subject]
      , contactOnline = or [c /= Sync.NoConnection | Sync.ConnectionValue c <- Sync.factsOf subject live]
      , contactMessages = mapMaybe (message g) (timeline g)
      , contactUnread = unread g
      , contactRequest = Nothing
      , contactStatusMessage = fromMaybe "" (listToMaybe [t | Sync.StatusMessage t <- kept subject])
      , contactStatus = fromMaybe Tox.Online (listToMaybe [toEnum (fromEnum st) | Sync.Status st <- Sync.factsOf subject live])
      , contactMute = fst <$> muteIn g
      , contactAvatar = Nothing
      , contactPictures = Nothing
      }
      where
        subject = Sync.FriendSubject pk
        g = Sync.friendGraph account pk
    message g (i, e) = case Sync.eventContent e of
      Sync.Received _ _ bytes kind ->
        Just (Message False (readText (readWith readable' bytes)) t Delivered Nothing Nothing 0 Nothing (toxKind kind) (Just bytes))
      Sync.Send _ bytes kind ->
        Just (Message True (Text.decodeUtf8Lenient bytes) t (delivery g i) Nothing Nothing 0 (Map.lookup i (arrived g)) (toxKind kind) Nothing)
      Sync.CallStarted _ incoming video ->
        let (talked, declined) = case Map.lookup i (ended g) of
              Just (d, no) -> (fromIntegral <$> d, no)
              Nothing      -> (Nothing, False)
            missed = incoming && Map.member i (ended g) && isNothing talked && not declined
        in Just (Message (not incoming) "" t Delivered Nothing
                   (Just (CallRecord video ((/ 1000) <$> talked) missed)) 0 Nothing Tox.MessageNormal Nothing)
      _ -> Nothing
      where t = Linked.eventTime e
    group c = (newGroup name (if isGroup then NgcGroup else TextConference) InGroup)
      { groupEntries = mapMaybe (entry g) (timeline g)
      , groupUnread = unread g
      , groupMembers = Map.fromList [(n, Member (memberKey' pk) (peerName pk)) | (n, pk) <- peers]
      , groupMute = fst <$> muteIn g
      , groupMentions = maybe True snd (muteIn g)
      , groupTopic = fromMaybe "" (listToMaybe [t | Sync.Topic t <- kept subject])
      , groupMayTopic = not locked || selfRole `elem` [Just Tox.Founder, Just Tox.Moderator]
      , groupRoles = Map.fromList [(n, role) | (n, pk) <- peers, Sync.RoleValue r <- peerFacts pk, let role = toxRole r]
      , groupSelfRole = selfRole
      , groupSettings = Tox.GroupSettings
          <$> listToMaybe [p == Sync.Private | Sync.PrivacyValue p <- kept subject]
          <*> Just (Text.encodeUtf8 (fromMaybe "" (listToMaybe [p | Sync.Password p <- kept subject])))
          <*> listToMaybe [fromIntegral n | Sync.PeerLimit n <- kept subject]
          <*> Just locked
          <*> listToMaybe [toxVoice v | Sync.VoiceStateValue v <- kept subject]
      , groupRelayBot = memberKey' <$> Map.lookup c (Sync.accountRelayBots state)
      }
      where
        g = Sync.chatGraph c
        subject = Sync.ChatSubject c
        isGroup = case c of
          Sync.GroupChat _ -> True
          _                -> False
        name = fromMaybe "" (listToMaybe [n | Sync.Name n <- kept subject])
        locked = or [b | Sync.TopicLock b <- kept subject]
        selfRole = listToMaybe [toxRole r | Sync.RoleValue r <- kept subject]
        peerFacts pk = Sync.factsOf (Sync.PeerSubject c pk) live
        peerName pk = fromMaybe "" (listToMaybe [n | Sync.Name n <- peerFacts pk])
        -- Numbered here, as Tox's numbers stay with the home.
        peers = zip [0 ..] [pk | (Sync.PeerSubject c' pk, Sync.Present True) <- Sync.allFacts live, c' == c]
    entry g (i, e) = GroupEntry (Linked.eventTime e) <$> case Sync.eventContent e of
      Sync.Received from sender bytes kind ->
        Just (Said (Just (Member (maybe "" memberKey' from) sender)) (toxKind kind) (readText (readWith readable' bytes)) Delivered)
      Sync.Send _ bytes kind -> Just (Said Nothing (toxKind kind) (Text.decodeUtf8Lenient bytes) (delivery g i))
      Sync.Notice t -> Just (Noted t)
      _ -> Nothing
    toxRole = \case
      Sync.Founder   -> Tox.Founder
      Sync.Moderator -> Tox.Moderator
      Sync.User      -> Tox.User
      Sync.Observer  -> Tox.Observer
    toxVoice = \case
      Sync.VoiceEveryone   -> Tox.VoiceAll
      Sync.VoiceModerators -> Tox.VoiceModerators
      Sync.VoiceFounder    -> Tox.VoiceFounder
    invite v = Invite
      { inviteKind = if Sync.inviteKind v == Sync.GroupKind then NgcGroup else TextConference
      , inviteName = Sync.inviteName v
      , inviteData = Sync.inviteData v
      , inviteAnswered = if Sync.inviteStatus v == Sync.Joining then Just (fromMaybe "" (Sync.invitePassword v)) else Nothing
      }

-- | Change a contact's message.
adjustMessage :: ContactId -> MessageId -> (Message -> Message) -> Model -> Model
adjustMessage cid i f model = model { modelContacts = Map.adjust contact cid (modelContacts model) }
  where
    contact c = c { contactMessages = [ if j == i then f m else m | (j, m) <- zip [0 ..] (contactMessages c) ] }

adjustFile :: ContactId -> MessageId -> (Attachment -> Attachment) -> Model -> Model
adjustFile cid i f = adjustMessage cid i (\m -> m { messageFile = f <$> messageFile m })

-- | A message whose transfer Tox ended before it was over. Ours waits to be
-- offered again and theirs to be offered again, and both go on where they
-- were, but only those with an ID can.
interrupted :: Message -> Message
interrupted m = m { messageFile = cut <$> messageFile m }
  where
    cut a
      | attachmentState a `notElem` [Waiting, Moving] = a
      | messageMine m && (attachmentState a == Waiting || resumable) = a { attachmentState = Waiting }
      | resumable && attachmentState a == Moving = a { attachmentState = Interrupted }
      | resumable = a
      | otherwise = a { attachmentState = Cancelled }
      where resumable = isJust (attachmentId a)

-- | What the screen opened starts with.
opened :: Screen -> Model -> Model
opened screen model = case screen of
  Chat cid -> model
    { modelContacts =
        Map.adjust (\c -> c { contactUnread = 0 }) cid (modelContacts model)
    }
  GroupChat gid -> model
    { modelGroups = Map.adjust (\g -> g { groupUnread = 0 }) gid (modelGroups model) }
  InviteScreen _ _ -> model { modelPassword = "" }
  TopicScreen gid -> model { modelTopic = maybe "" groupTopic (Map.lookup gid (modelGroups model)) }
  NewGroupScreen -> model { modelGroupName = "", modelFounding = PrivateGroup }
  GroupSettingsScreen gid -> case groupSettings =<< Map.lookup gid (modelGroups model) of
    Just settings -> model { modelPeerLimit = Text.pack (show (Tox.groupPeerLimit settings))
                           , modelGroupPassword = Text.decodeUtf8Lenient (Tox.groupPassword settings) }
    Nothing -> model
  JoinGroupScreen -> model { modelChatId = "", modelPassword = "" }
  LinkScreen -> model { modelLink = "" }
  ProfileScreen -> model
    { modelNickname = profileNickname (modelProfile model)
    , modelStatus = profileStatus (modelProfile model)
    }
  ProxyScreen ->
    let proxy = settingProxy (modelDevice model)
    in model
      { modelProxyKind = Tox.proxyType <$> proxy
      , modelProxyHost = maybe "" (Text.pack . Tox.proxyHost) proxy
      , modelProxyPort = maybe "" (Text.pack . show . Tox.proxyPort) proxy
      }
  AddContact -> model
    { modelAddress = ""
    , modelRequest = requestMessage (modelProfile model)
    , modelAddError = Nothing
    }
  _ -> model

-- | Back a step, or Nothing on the contacts, where Back leaves the app.
back :: Model -> Maybe Model
back model
  -- A call ringing can't be hidden, one going on goes behind a banner.
  | Just c <- modelCall model, not (callHidden c) =
      if callPhase c == Ringing then Nothing else Just model { modelCall = Just c { callHidden = True } }
  | modelDrawer model = Just model { modelDrawer = False }
  | modelScreen model == Contacts = Nothing
  | not (Set.null (modelSelected model)) = Just model { modelSelected = Set.empty }
  | Details cid <- modelScreen model = Just model { modelScreen = Chat cid }
  | GroupDetails gid <- modelScreen model = Just model { modelScreen = GroupChat gid }
  | TopicScreen gid <- modelScreen model = Just model { modelScreen = GroupDetails gid }
  | InviteFriendsScreen gid <- modelScreen model = Just model { modelScreen = GroupDetails gid }
  | GroupSettingsScreen gid <- modelScreen model = Just model { modelScreen = GroupDetails gid }
  | Forward cid _ <- modelScreen model = Just model { modelScreen = Chat cid }
  | MessageInfo cid _ <- modelScreen model = Just model { modelScreen = Chat cid }
  | modelScreen model == ProxyScreen = Just model { modelScreen = SettingsScreen }
  | modelScreen model == LinkScreen = Just model { modelScreen = ProfilesScreen }
  | otherwise = Just model { modelScreen = Contacts }

-- | The unread messages and files of each contact with any and not muted
-- at a time, oldest first, which the user is notified of.
notifications :: UTCTime -> Model -> Map ContactId [Text]
notifications now model = Map.mapMaybeWithKey lines' (unreadOf now model)
  where
    lines' cid = nonEmpty . map (preview (named cid)) . filter (isNothing . messageCall)
    named cid = maybe "" (contactLabel cid) (Map.lookup cid (modelContacts model))

-- | Every call with a contact, newest first.
callHistory :: Model -> [(ContactId, Message, CallRecord)]
callHistory model = sortOn (\(_, m, _) -> Down (messageTime m))
  [ (cid, m, record)
  | (cid, c) <- Map.toList (modelContacts model), m <- contactMessages c, Just record <- [messageCall m] ]

-- | The unread messages of each group with any and not muted at a time,
-- oldest first, each with who wrote it.
--
-- While muted, those naming us still notify, unless the user chose not.
groupNotifications :: UTCTime -> Model -> Map GroupId [Text]
groupNotifications now model = Map.mapMaybe unread (modelGroups model)
  where
    unread g
      | groupUnread g > 0 && (not (groupMuted now g) || groupMentions g) =
          let said = uttered (modelProfile model) g
              received = \case
                GroupEntry _ (Said (Just who) kind text _) -> [(said who text, False, kind)]
                GroupEntry _ (PrivateFrom who kind text)   -> [((memberLabel who, text), True, kind)]
                _                                          -> []
              theirs = concatMap received (groupEntries g)
              -- Those to us alone are as good as naming us.
              shown ((_, text), private, _) = not (groupMuted now g) || private || mentions (groupNick (modelProfile model)) text
          in nonEmpty [ line m | m <- drop (length theirs - groupUnread g) theirs, shown m ]
      | otherwise = Nothing
    line ((who, text), private, kind)
      | kind == Tox.MessageAction = "* " <> name <> " " <> text
      | otherwise = name <> ": " <> text
      where name = who <> (if private then " (privately)" else "")

-- | How many missed calls of each contact with any are unread, and it isn't
-- muted at a time, which the user is notified of apart from messages.
missedCalls :: UTCTime -> Model -> Map ContactId Int
missedCalls now = Map.mapMaybe (positive . length . filter (isJust . messageCall)) . unreadOf now
  where positive n = if n > 0 then Just n else Nothing

-- | What is unread of each contact, oldest first: theirs, of calls only the
-- missed ones. None of a muted contact.
unreadOf :: UTCTime -> Model -> Map ContactId [Message]
unreadOf now model = Map.mapMaybe unread (modelContacts model)
  where
    unread c
      | contactUnread c > 0 && not (muted now c) =
          let theirs = filter unreadable (contactMessages c)
          in Just (drop (length theirs - contactUnread c) theirs)
      | otherwise = Nothing
    unreadable m = not (messageMine m) && maybe True recordMissed (messageCall m)

nonEmpty :: [a] -> Maybe [a]
nonEmpty xs = if null xs then Nothing else Just xs
