{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.ViewSpec (spec) where

import qualified Data.ByteString     as BS
import           Data.Int            (Int32)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time           (UTCTime (..), fromGregorian, utc)
import           Hatter.Action       (Action (..), OnChange (..))
import           Hatter.Widget
import           Test.Hspec

import qualified Tox.Application.Tox as Tox

import           Tox.Android.Model
import           Tox.Android.Sample
import           Tox.Android.Theme   (theme)
import           Tox.Android.View

env :: Env
env = Env
  { envTheme = theme 1 1080 False
  , envNow = UTCTime (fromGregorian 2026 11 2) 43200
  , envZone = utc
  , envAvatar = const BS.empty
  , envQr = BS.empty
  , envPictureSize = const Nothing
  , envDecoded = const Nothing
  , envClock24 = False
  }

handlers :: Handlers
handlers = Handlers
  { hBack = Action 0
  , hSend = Action 1
  , hOpenDrawer = Action 2
  , hOpenProfile = Action 3
  , hOpenAddContact = Action 4
  , hApplyProfile = Action 5
  , hAddFriend = Action 6
  , hCopyToxId = Action 12
  , hAttach = Action 17
  , hTapMessage = \cid i -> Action (400 + 10 * numbered cid + fromIntegral i)
  , hHoldMessage = \cid i -> Action (900 + 10 * numbered cid + fromIntegral i)
  , hCancelFile = \cid i -> Action (1100 + 10 * numbered cid + fromIntegral i)
  , hUnselect = Action 31
  , hCopySelected = Action 32
  , hForwardSelected = Action 33
  , hCancelReply = Action 34
  , hUnstage = \cid k -> Action (1750 + 10 * numbered cid + fromIntegral k)
  , hOpenSettings = Action 35
  , hOpenProfiles = Action 51
  , hSwitchProfile = \(ProfileId i) -> Action (1400 + fromIntegral i)
  , hDeleteProfile = \(ProfileId i) -> Action (1500 + fromIntegral i)
  , hNewProfile = Action 52
  , hImportProfile = Action 53
  , hPickAvatar = Action 40
  , hRemoveAvatar = Action 41
  , hExport = Action 42
  , hAutoAccept = Action 36
  , hToggleUdp = Action 37
  , hToggleIpv6 = Action 38
  , hToggleLan = Action 39
  , hToggleStay = Action 50
  , hToggleFormat = const (Action 73)
  , hOpenProxy = Action 54
  , hProxyKind = Action . maybe 55 (\x -> if x == Tox.HttpProxy then 56 else 57)
  , hProxyHost = OnChange 58
  , hProxyPort = OnChange 59
  , hApplyProxy = Action 60
  , hForwardTo = Action . (1000 +) . numbered
  , hOpenChat = Action . (100 +) . numbered
  , hAccept = Action . (200 +) . numbered
  , hReject = Action . (300 +) . numbered
  , hDetails = Action . (500 +) . numbered
  , hMuteChat = Action . (600 +) . numbered
  , hDelete = Action . (700 +) . numbered
  , hCopyKey = Action . (800 +) . numbered
  , hPictures = Action . (1300 +) . numbered
  , hDraft = OnChange 7
  , hNickname = OnChange 8
  , hStatus = OnChange 9
  , hAddress = OnChange 10
  , hRequest = OnChange 11
  , hCall = \camera -> Action (if camera then 21 else 20)
  , hAnswer = \camera -> Action (if camera then 23 else 22)
  , hHangUp = Action 24
  , hMute = Action 25
  , hCamera = Action 26
  , hRoute = Action 27
  , hFlipCamera = Action 28
  , hHideCall = Action 44
  , hOpenChats = Action 46
  , hOpenCalls = Action 47
  , hCallBack = Action . (1200 +) . numbered
  , hAllowFullScreen = Action 49
  , hShowCall = Action 45
  , hScrolled = OnChange 29
  , hJump = Action 43
  , hOpenGroup = const (Action 1600)
  , hGroupDetails = const (Action 1601)
  , hMuteGroup = const (Action 1602)
  , hLeaveGroup = const (Action 1603)
  , hDeleteGroup = const (Action 1604)
  , hCopyChatId = const (Action 1605)
  , hOpenInvite = \cid _ -> Action (1700 + numbered cid)
  , hJoin = \cid _ -> Action (1710 + numbered cid)
  , hDecline = \cid _ -> Action (1720 + numbered cid)
  , hPassword = OnChange 61
  , hEditTopic = const (Action 1606)
  , hToggleMentions = const (Action 1607)
  , hTopic = OnChange 62
  , hApplyTopic = Action 63
  , hFounding = const (Action 64)
  , hGroupName = OnChange 65
  , hFound = Action 66
  , hChatId = OnChange 67
  , hJoinPublic = Action 68
  , hInviteFriends = const (Action 1608)
  , hInviteFriend = \_ cid -> Action (1730 + numbered cid)
  , hMember = \_ n -> Action (1740 + fromIntegral n)
  , hEndPrivate = Action 69
  , hGroupSettings = const (Action 1609)
  , hPrivacy = const (Action 1610)
  , hTopicLock = const (Action 1611)
  , hVoice = const (Action 1612)
  , hRelayBot = const (Action 1613)
  , hStopRelay = const (Action 1614)
  , hPeerLimit = OnChange 70
  , hGroupPassword = OnChange 71
  , hApplyGroupSettings = Action 72
  , hOpenLink = Action 73
  , hLinkEdited = OnChange 74
  , hLink = Action 75
  }

numbered :: ContactId -> Int32
numbered cid = fromIntegral (length (takeWhile (/= cid) [alice, bob, carol]))

shown :: Model -> Widget
shown = view env handlers

model' :: [Event] -> Model
model' events = applying events sample

children :: Widget -> [Widget]
children widget = case widget of
  Column s              -> map liWidget (lsWidgets s)
  Row s                 -> map liWidget (lsWidgets s)
  Stack items           -> map liWidget items
  LazyColumn items      -> items
  Styled _ child        -> [child]
  Animated _ child      -> [child]
  Tappable _ child      -> [child]
  LongPressable _ child -> [child]
  Scrolled _ _ child    -> [child]
  _                     -> []

-- | Every label, button and input value, in order.
texts :: Widget -> [Text]
texts widget = case widget of
  Text c      -> [tcLabel c]
  Button c    -> [bcLabel c]
  TextInput c -> [tiValue c]
  _           -> concatMap texts (children widget)

-- | What each tap fires, with the texts it covers.
taps :: Widget -> [([Text], Action)]
taps widget = case widget of
  Tappable action child -> (texts child, action) : taps child
  Button c              -> [([bcLabel c], bcAction c)]
  _                     -> concatMap taps (children widget)

tapOn :: Text -> Widget -> Maybe Action
tapOn caption widget =
  -- The innermost, which the platform gives the tap.
  case reverse [action | (covered, action) <- taps widget, caption `elem` covered] of
    action : _ -> Just action
    []         -> Nothing

spec :: Spec
spec = do
  it "lists invites and groups, and a group's messages under their senders' names" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        joined = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                 , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0)
                 , GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "hi"
                 , GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "again" ]
        listed = shown (model' (joined ++ [Back, Invited bob (GroupId "C0DE") NgcGroup "Other" "other invite"]))
    tapOn "Other" listed `shouldBe` Just (Action 1701)
    tapOn "Team" listed `shouldBe` Just (Action 1600)
    texts listed `shouldContain` ["Bob invited you to a group"]
    let chatTexts = texts (shown (model' joined))
    filter (`elem` ["Dana", "hi", "again", "2 members"]) chatTexts `shouldBe` ["2 members", "Dana", "hi", "again"]

  it "attaches with the chat's one button until there is something to send" $ do
    let actions = map snd . taps . shown
        empty = actions (model' [Open (Chat alice)])
        written = actions (model' [Open (Chat alice), DraftChanged "hi"])
    (Action 17 `elem` empty, Action 1 `elem` empty) `shouldBe` (True, False)
    (Action 17 `elem` written, Action 1 `elem` written) `shouldBe` (False, True)

  it "lists the contacts, each opening its chat" $ do
    texts (shown sample) `shouldContain` ["Alice", "Looks good already.", "9:02 AM", "1"]
    tapOn "Bob" (shown sample) `shouldBe` Just (Action 101)

  it "names a contact by the start of its key until it has a name" $
    tapOn "19A974C2" (shown (model' [Accept carol])) `shouldBe` Just (Action 102)

  it "lists friend requests to accept or reject" $ do
    texts (shown sample) `shouldContain` ["Hey, I'm Carol, come chat with me on Tox!"]
    map snd (taps (shown sample)) `shouldContain` [Action 202, Action 302]

  it "shows a hidden call's chat under a banner that returns to it" $ do
    let page = view env { envNow = at 3 9 2 } handlers
          (model' [Open (Chat alice), StartCall (at 3 9 0) False, CallPeer alice (at 3 9 0) False, HideCall])
    mapM_ (\t -> texts page `shouldContain` [t]) ["Return to the call", "2:00", "Looks good already."]
    tapOn "Return to the call" page `shouldBe` Just (Action 45)

  it "lists calls newest first, calling back those online" $ do
    let made = [ CallInvited bob (at 3 9 0) False, CallEnded bob (at 3 9 1)
                  , Open (Chat alice), StartCall (at 3 10 0) False, CallPeer alice (at 3 10 0) False, HangUp (at 3 10 2) ]
        page = shown (model' (made ++ [Open CallsScreen]))
    [cid | (cid, _, _) <- callHistory (model' made)] `shouldBe` [alice, bob]
    tapOn "Bob" page `shouldBe` Just (Action 101)
    filter ((>= 1200) . actionId) (map snd (taps page)) `shouldBe` [Action 1200]
    texts (shown (model' [Open CallsScreen])) `shouldContain` ["No calls yet"]
    tapOn "Allow" (shown (model' [Open CallsScreen, FullScreenChecked False])) `shouldBe` Just (Action 49)

  it "shows a few emoji alone large" $ do
    let sizes w = case w of
          Text c -> [(tcLabel c, fontSize <$> tcFontConfig c)]
          _      -> concatMap sizes (children w)
        page = shown (model' [Open (Chat alice), Received alice (at 2 9 3) Tox.MessageNormal "\xF0\x9F\x91\x8D"])
    sizes page `shouldContain` [("\x1F44D", Just 48)]

  it "lists the profiles, switching to the others" $ do
    let page = shown (model' [ProfilesChanged [Account (ProfileId 0) "Tox User" "", Account (ProfileId 7) "Work" (Text.replicate 8 "19A974C2")], Open ProfilesScreen])
    texts page `shouldContain` ["Tox User"]
    tapOn "Tox User" page `shouldBe` Nothing
    tapOn "Work" page `shouldBe` Just (Action 1407)
    map snd (taps page) `shouldContain` [Action 1507, Action 52, Action 53]

  it "says there are no chats yet" $
    texts (shown initial) `shouldContain` ["No chats yet"]

  it "opens the drawer over the contacts, with the profile" $ do
    let drawer = shown (model' [OpenDrawer])
    texts drawer `shouldContain` ["Tox User", "Toxing on Haskell"]
    tapOn "Toxing on Haskell" drawer `shouldBe` Just (Action 3)

  it "shows the chat's messages by day, and its draft" $ do
    let model = model' [Open (Chat alice), DraftChanged "typing"]
    mapM_ (\t -> texts (shown model) `shouldContain` [t])
      [ "Yesterday", "Hi! Is this the Haskell client?", "3:14 PM"
      , "Today", "Looks good already.", "typing" ]

  it "shows times on the clock the user wants, and older days with their date" $ do
    let model = model' [Open (Chat alice)]
    texts (view env { envClock24 = True } handlers model) `shouldContain` ["15:14"]
    texts (view env { envNow = at 9 12 0 } handlers model) `shouldContain` ["Sun, Nov 1"]
    texts (view env { envNow = UTCTime (fromGregorian 2027 1 5) 0 } handlers model)
      `shouldContain` ["Nov 1, 2026"]

  it "shows the time of a run's last message only" $ do
    let run = [Open (Chat bob), DraftChanged "one", Send (at 2 12 0), DraftChanged "two", Send (at 2 12 1)]
        times = filter (== "12:00 PM") (texts (shown (model' run)))
    times `shouldBe` []
    texts (shown (model' run)) `shouldContain` ["12:01 PM"]

  it "lists the latest chat first, with a draft where there is one" $ do
    let model = model' [Open (Chat bob), DraftChanged "for Bob", Send (at 2 13 0), Open (Chat alice), DraftChanged "unsent", Back]
        listed = texts (shown model)
    filter (`elem` ["Alice", "Bob"]) listed `shouldBe` ["Bob", "Alice"]
    listed `shouldContain` ["Draft: ", "unsent"]

  it "says Applied once the profile is" $ do
    texts (shown (model' [Open ProfileScreen])) `shouldContain` ["Applied"]
    texts (shown (model' [Open ProfileScreen, NicknameChanged "Toxer"]))
      `shouldContain` ["Apply changes"]

  it "says why an address isn't a Tox ID" $ do
    let model = model' [Open AddContact, AddressChanged "C0FFEE"]
    texts (shown model) `shouldContain` ["C0FFEE", "Must be 76 hexadecimal characters"]

  it "says why Tox wouldn't take a friend request" $ do
    let model = model' [Open AddContact, RequestChanged ""]
    texts (shown model) `shouldContain` ["The message can't be empty"]

  it "says why the app didn't start, with a button to try again" $ do
    let failed = startFailure (envTheme env) (Action 30) "Tox didn't start: PortInUse"
    texts failed `shouldContain` ["Tox didn't start: PortInUse"]
    tapOn "Try again" failed `shouldBe` Just (Action 30)

  it "shows a notice in the chat until the user goes elsewhere" $ do
    let noticed = [Open (Chat alice), Notify "Calls need the microphone"]
    texts (shown (model' noticed)) `shouldContain` ["Calls need the microphone"]
    texts (shown (model' (noticed ++ [Back, Open (Chat alice)]))) `shouldNotContain` ["Calls need the microphone"]

  it "shows a contact's page, opened from its chat" $ do
    tapOn "Alice" (shown (model' [Open (Chat alice)])) `shouldBe` Just (Action 500)
    let page = shown (model' [ FriendStatusMessage alice "Writing Haskell", FriendStatus alice Tox.Away
                             , MuteChat alice (Just (at 2 18 0)), Open (Details alice) ])
        ContactId key = alice
    mapM_ (\t -> texts page `shouldContain` [t])
      ["Alice", "Writing Haskell", "Away", "Unmute notifications", "Muted until 6:00 PM", key, "Delete contact"]
    tapOn "Delete contact" page `shouldBe` Just (Action 700)

  it "selects messages, and answers one" $ do
    let selected = shown (model' [Open (Chat alice), Select 0, Select 2])
        replying = shown (model' [Open (Chat alice), Reply 0])
    texts selected `shouldContain` ["2 selected"]
    map snd (taps selected) `shouldContain` [Action 32, Action 33]
    texts replying `shouldContain` ["> Hi! Is this the Haskell client?"]

  it "tells when a message went and came" $ do
    let info i = texts (shown (model' [Open (MessageInfo alice i)]))
    info 1 `shouldContain` ["Sent", "Nov 1, 2026 3:15 PM", "Delivered", "Nov 1, 2026 3:16 PM"]
    info 0 `shouldContain` ["Received", "Nov 1, 2026 3:14 PM"]

  it "shows the settings, opened from the drawer" $ do
    tapOn "Settings" (shown (model' [OpenDrawer])) `shouldBe` Just (Action 35)
    let page = texts (shown (model' [Open SettingsScreen, SetAutoAccept 0, SetDevice (DeviceSettings True False True True Nothing)]))
    page `shouldContain` ["Accept files without asking", "Never"]
    page `shouldContain` ["Use IPv6", "Off"]

  it "says a contact hasn't answered our request yet" $ do
    let address = toxId "0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD0123ABCD" "DEADBEEF"
    texts (shown (model' [Open AddContact, AddressChanged address, AddFriend, Back]))
      `shouldContain` ["0123ABCD", "Friend request sent"]

  it "shows a friend's file with its size, to take or decline" $ do
    let i = length (maybe [] contactMessages (Map.lookup alice (modelContacts sample)))
        large = FileOffered alice (Receiving 0) (at 3 9 0) "notes.pdf" (autoAccepted + 1) "/received/notes.pdf" "id"
        chat = shown (model' [large, Open (Chat alice)])
    texts chat `shouldContain` ["notes.pdf", "4.0 MB \x00B7 Offered"]
    tapOn "notes.pdf" chat `shouldBe` Just (Action (400 + fromIntegral i))
    tapOn "Accept" chat `shouldBe` Just (Action (400 + fromIntegral i))
    tapOn "Decline" chat `shouldBe` Just (Action (1100 + fromIntegral i))

  it "shows a call over the screen, with how long it has gone" $ do
    let ringing = shown (model' [Open (Chat bob), CallInvited alice (at 3 9 0) False])
    mapM_ (\t -> texts ringing `shouldContain` [t]) ["Alice", "Incoming voice call"]
    map snd (taps ringing) `shouldBe` [Action 24, Action 22, Action 23]
    let talking = shown (model' [CallInvited alice (at 3 9 0) False, AnswerCall (UTCTime (fromGregorian 2026 11 2) 43075) False])
    texts talking `shouldContain` ["2:05"]
    let missed = shown (model' [Open (Chat alice), CallInvited alice (at 3 9 0) False, CallEnded alice (at 3 9 1)])
    mapM_ (\t -> texts missed `shouldContain` [t]) ["Missed voice call", "9:00 AM"]
    map snd (taps talking) `shouldBe` [Action 44, Action 27, Action 26, Action 25, Action 24]
