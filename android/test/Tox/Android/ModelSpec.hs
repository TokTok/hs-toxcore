{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.ModelSpec (spec) where

import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as TextEncoding
import           Data.Time           (addUTCTime)
import           Test.Hspec

import qualified Tox.Application.Tox as Tox

import           Tox.Android.Model
import           Tox.Android.Sample

messagesOf :: ContactId -> Model -> [Message]
messagesOf cid = maybe [] contactMessages . Map.lookup cid . modelContacts

-- | What Tox is asked to do by the last of the events.
commands :: [Event] -> Event -> [Command]
commands events = snd . flip update (applying events sample)

groupOf :: Model -> Maybe Group
groupOf = Map.lookup team . modelGroups

dana :: Member
dana = Member (Text.replicate 8 "DA7A0000") "Dana"

spec :: Spec
spec = do
  it "joins a friend's group invite with the password once the friend is online" $ do
    let answered = [Invited bob team NgcGroup "Team" "invite", Open (InviteScreen bob team), PasswordChanged "pw"]
    commands answered (AnswerInvite bob team True) `shouldBe` []
    commands (answered ++ [AnswerInvite bob team True]) (FriendOnline bob True)
      `shouldContain` [JoinGroup bob team "invite" "Tox User" "pw"]
    let joined = applying (answered ++ [AnswerInvite bob team True, FriendOnline bob True]) sample
    modelInvites joined `shouldBe` Map.empty
    groupPhase <$> groupOf joined `shouldBe` Just Joining

  it "spends an invite a wrong password fails, leaving what Tox has of the group" $ do
    let events = [Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True]
    commands (take 1 events) (AnswerInvite alice team True) `shouldBe` [JoinGroup alice team "invite" "Tox User" ""]
    commands events (GroupRefused team Tox.JoinFailBadPassword) `shouldBe` [QuitGroup team]
    groupPhase <$> groupOf (applying (events ++ [GroupRefused team Tox.JoinFailBadPassword]) sample)
      `shouldBe` Just (Refused Tox.JoinFailBadPassword)

  it "names messages by their sender, and tells who came and went once in the group" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        events =
          [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
          , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0)
          -- Met with Dana, while Tox gets to know who is there.
          , MemberJoined team (at 3 9 0) 3 (Member (Text.replicate 8 "E0E00000") "Eve")
          , MemberJoined team (at 3 9 1) 2 carl, GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "hi"
          , MemberLeft team (at 3 9 3) 2 Tox.ExitQuit, Back ]
        model = applying events sample
    map entryWhat . groupEntries <$> groupOf model
      `shouldBe` Just [Arrived carl, Said (Just dana) Tox.MessageNormal "hi" Delivered, Departed carl]
    Map.lookup team (groupNotifications (at 3 9 4) model) `shouldBe` Just ["Dana: hi"]

  it "finds mentions as whole words in any case" $ do
    map (mentions "Tox User") ["hi tox user!", "@Tox User: look", "Tox Users", "xTox User", "Tox"]
      `shouldBe` [True, True, False, False, False]
    mentions "  " "anything" `shouldBe` False

  it "notifies of a muted group's messages naming us only, unless asked not to" $ do
    let events = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                 , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0), Back
                 , MuteGroup team Nothing
                 , GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "lunch?"
                 , GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "tox user, lunch?" ]
        shown evs = Map.lookup team (groupNotifications (at 3 9 3) (applying evs sample))
    shown events `shouldBe` Just ["Dana: tox user, lunch?"]
    shown (events ++ [ToggleMentions team]) `shouldBe` Nothing

  it "shows a relay bot's messages under the names it relays, once chosen" $ do
    let bridge = Member (Text.replicate 8 "B0B00000") "Bridge"
        events = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                 , MemberJoined team (at 3 9 0) 1 dana, MemberJoined team (at 3 9 0) 2 bridge
                 , GroupConnected team (at 3 9 0), Back, MuteGroup team Nothing
                 , GroupReceived team (at 3 9 1) 2 Tox.MessageNormal "<Bob> hi"
                 , GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "<Dana> is me"
                 , GroupReceived team (at 3 9 3) 2 Tox.MessageNormal "<Carol> tox user, lunch?" ]
        relaying = applying (events ++ [ChooseRelayBot team (Just (memberKey bridge))]) sample
        g = groupOf relaying
    fmap relayCounts g `shouldBe` Just [(bridge, 2, 2), (dana, 1, 1)]
    fmap (relayedThrough (modelProfile relaying)) g `shouldBe` Just [("Carol", at 3 9 3), ("Bob", at 3 9 1)]
    Map.lookup team (groupNotifications (at 3 9 4) relaying) `shouldBe` Just ["Carol via Bridge: tox user, lunch?"]
    Map.lookup team (groupNotifications (at 3 9 4) (applying events sample))
      `shouldBe` Just ["Bridge: <Carol> tox user, lunch?"]

  it "tells of topics set after Tox got into the group, by whom" $ do
    let events = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                 , MemberJoined team (at 3 9 0) 1 dana
                 , TopicChanged team (at 3 9 0) Nothing "first", GroupConnected team (at 3 9 0)
                 , TopicChanged team (at 3 9 1) (Just 1) "second", TopicSetByUs team (at 3 9 2) "third" ]
        g = groupOf (applying events sample)
    groupTopic <$> g `shouldBe` Just "third"
    map entryWhat . groupEntries <$> g `shouldBe` Just [TopicSet (Just dana) "second", TopicSet Nothing "third"]
    let edited = [Open (TopicScreen team), TopicEdited " fourth "]
    commands (events ++ edited) ApplyTopic `shouldBe` []
    commands (events ++ TopicAllowed team True : edited) ApplyTopic `shouldBe` [SetGroupTopic team "fourth"]

  it "joins an audio conference for its text, resends what found no one, and rejoins it" $ do
    let talk = GroupId "01C0FFEE"
        carl = Member (Text.replicate 8 "CA470000") "Carl"
        joined = [ Invited alice talk AudioConference "" "cookie", AnswerInvite alice talk True
                 , GroupConnected talk (at 3 9 0), DraftChanged "early", Send (at 3 9 1) ]
    commands (take 1 joined) (AnswerInvite alice talk True) `shouldBe` [JoinConference alice talk "cookie"]
    commands (init joined) (Send (at 3 9 1)) `shouldBe` [SendGroupMessage talk 0 "early"]
    commands joined (MembersListed talk (at 3 9 2) [(1, dana)]) `shouldBe` [SendGroupMessage talk 0 "early"]
    let later = joined ++ [GroupSent talk 0, MembersListed talk (at 3 9 2) [(1, dana)]
                          , MembersListed talk (at 3 9 3) [(2, carl)], GroupSaved talk "state"]
        g = Map.lookup talk (modelGroups (applying later sample))
    map entryWhat . drop 1 . groupEntries <$> g `shouldBe` Just [Arrived carl, Departed dana]
    commands later (Started "") `shouldContain` [RestoreGroup AudioConference talk "state"]

  it "founds groups, joins public ones by chat ID, and invites friends" $ do
    let named = [Open NewGroupScreen, FoundingChosen PublicGroup, GroupNameEdited " Lab "]
    commands named Found `shouldBe` [FoundGroup PublicGroup "Lab" "Tox User"]
    modelNotice (applying [Open NewGroupScreen, Found] sample) `shouldBe` Just "The name can't be empty"
    let created = applying (named ++ [Found, GroupCreated team NgcGroup "Lab" (at 3 9 0)]) sample
    (modelScreen created, groupPhase <$> groupOf created) `shouldBe` (GroupChat team, Just InGroup)
    let chat = Text.replicate 8 "c0ffee00"
        typed = [Open JoinGroupScreen, ChatIdEdited chat, PasswordChanged "pw"]
    commands typed JoinPublic `shouldBe` [JoinPublicGroup team "Tox User" "pw"]
    groupPhase <$> groupOf (applying (typed ++ [JoinPublic]) sample) `shouldBe` Just Joining
    commands [Open JoinGroupScreen, ChatIdEdited "C0FFEE"] JoinPublic `shouldBe` []
    commands [] (InviteFriend team alice) `shouldBe` [InviteToGroup team alice]

  it "lets the founder do anything to others, moderators only to users and observers" $ do
    memberActions (Just Tox.Founder) Tox.Moderator `shouldBe` [MakeUser, MakeObserver, Kick]
    memberActions (Just Tox.Founder) Tox.Founder `shouldBe` []
    memberActions (Just Tox.Moderator) Tox.User `shouldBe` [MakeObserver, Kick]
    memberActions (Just Tox.Moderator) Tox.Moderator `shouldBe` []
    memberActions (Just Tox.User) Tox.Observer `shouldBe` []

  it "says things to a member alone, and hears and notifies theirs" $ do
    let inGroup = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                  , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0) ]
        -- As the member's menu on the group's page does it.
        written = inGroup ++ [Open (GroupDetails team), PrivatelyTo (Just 1), Open (GroupChat team), DraftChanged "psst"]
    commands written (Send (at 3 9 1)) `shouldBe` [SendPrivate team 0 1 "psst"]
    let both = written ++ [Send (at 3 9 1), GroupSent team 0, Back, MuteGroup team Nothing
                          , PrivateReceived team (at 3 9 2) 1 Tox.MessageNormal "hush"]
        model = applying both sample
    map entryWhat . groupEntries <$> groupOf model
      `shouldBe` Just [PrivateTo dana Tox.MessageNormal "psst" Sent, PrivateFrom dana Tox.MessageNormal "hush"]
    -- Even muted, as it names no one else.
    Map.lookup team (groupNotifications (at 3 9 3) model) `shouldBe` Just ["Dana (privately): hush"]

  it "tells who moderated whom, and leaves a group that removed us" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        inGroup = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                  , MemberJoined team (at 3 9 0) 1 dana, MemberJoined team (at 3 9 0) 2 carl
                  , GroupConnected team (at 3 9 0), GroupSaved team "state" ]
        moderated = inGroup ++ [GroupModerated team (at 3 9 1) (Just 1) (Just 2) Tox.ModModerator]
    map entryWhat . groupEntries <$> groupOf (applying moderated sample)
      `shouldBe` Just [Moderated (Just dana) (Just carl) Tox.ModModerator]
    commands moderated (GroupModerated team (at 3 9 2) (Just 1) Nothing Tox.ModKick) `shouldBe` [QuitGroup team]
    let kicked = applying (moderated ++ [GroupModerated team (at 3 9 2) (Just 1) Nothing Tox.ModKick]) sample
    (groupPhase <$> groupOf kicked, groupSaved =<< groupOf kicked) `shouldBe` (Just Kicked, Nothing)
    commands [] (Moderate team 2 Kick) `shouldBe` [ModerateMember team 2 Kick]

  it "applies the founder's settings when they make sense" $ do
    let settings = Tox.GroupSettings True "" 100 True Tox.VoiceAll
        opened = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                 , GroupConnected team (at 3 9 0), SettingsKnown team settings, Open (GroupSettingsScreen team) ]
    modelPeerLimit (applying opened sample) `shouldBe` "100"
    commands (opened ++ [PeerLimitEdited "50", GroupPasswordEdited "pw"]) ApplyGroupSettings
      `shouldBe` [SetGroupSettings team settings { Tox.groupPeerLimit = 50, Tox.groupPassword = "pw" }]
    commands (opened ++ [PeerLimitEdited "0"]) ApplyGroupSettings `shouldBe` []

  it "shows TRIfA's messages as Tox carried them while the user wants" $ do
    let trailer = BS.pack ([0, 0] ++ replicate 32 7 ++ [0, 0, 0, 1])
        received = [Received alice (at 3 9 0) Tox.MessageNormal ("hi" <> trailer)]
        texts' events = map messageText (drop 3 (messagesOf alice (applying events sample)))
    texts' received `shouldBe` ["hi"]
    texts' (received ++ [ToggleFormat "message_v3"]) `shouldBe` [TextEncoding.decodeUtf8Lenient ("hi" <> trailer)]
    texts' (ToggleFormat "message_v3" : received ++ received) `shouldBe` replicate 2 (TextEncoding.decodeUtf8Lenient ("hi" <> trailer))

  it "sends what was written while joining, and rejoins from what Tox kept at a start" $ do
    let written = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
                  , DraftChanged "early", Send (at 3 9 0) ]
    commands written (Send (at 3 9 0)) `shouldBe` []
    commands written (GroupConnected team (at 3 9 0)) `shouldBe` [SendGroupMessage team 0 "early"]
    let kept = written ++ [GroupConnected team (at 3 9 0), GroupSent team 0, GroupSaved team "state"]
    commands kept (Started "") `shouldContain` [RestoreGroup NgcGroup team "state"]
    groupPhase <$> groupOf (applying (kept ++ [Started ""]) sample) `shouldBe` Just Joining

  it "sends the chat's draft at its time, and clears it" $ do
    let events = [Open (Chat alice), DraftChanged "  hello  "]
        sent = applying (events ++ [Send (at 3 9 0)]) sample
    last (messagesOf alice sent) `shouldBe` Message True "hello" (at 3 9 0) Pending Nothing Nothing 0 Nothing Tox.MessageNormal Nothing
    Map.lookup alice (modelDrafts sent) `shouldBe` Nothing
    commands events (Send (at 3 9 0)) `shouldBe` [SendMessage alice 3 "hello" 0]

  it "sends nothing blank" $ do
    let events = [Open (Chat alice), DraftChanged "   "]
    applying (events ++ [Send (at 3 9 0)]) sample `shouldBe` applying events sample
    commands events (Send (at 3 9 0)) `shouldBe` []

  it "sends what a friend hasn't got once they are online" $ do
    let events = [Open (Chat bob), DraftChanged "first", Send (at 3 9 0)]
    commands events (Send (at 3 9 0)) `shouldBe` []
    commands (events ++ [DraftChanged "second", Send (at 3 9 1), Delivering bob 1 Sent])
      (FriendOnline bob True)
      `shouldBe` [SendMessage bob 0 "first" 0, SendMessage bob 1 "second" 0]

  it "marks a message sent, then delivered, and never back" $ do
    let delivery events = map messageDelivery (messagesOf alice (applying events sample))
    delivery [] `shouldBe` [Delivered, Delivered, Delivered]
    let sent = [Open (Chat alice), DraftChanged "hi", Send (at 3 9 0)]
    last (delivery sent) `shouldBe` Pending
    last (delivery (sent ++ [Delivering alice 3 Sent])) `shouldBe` Sent
    last (delivery (sent ++ [Confirmed alice 3 1 (at 3 9 1), Delivering alice 3 Sent]))
      `shouldBe` Delivered

  it "delivers a long message once the friend has all its parts" $ do
    let long = Text.replicate 3000 "a"
        written = [Open (Chat alice), DraftChanged long]
        sent = written ++ [Send (at 3 9 0)]
        last' events = last (messagesOf alice (applying (sent ++ events) sample))
    commands written (Send (at 3 9 0)) `shouldBe` [SendMessage alice 3 long 0]
    messageDelivery (last' [Confirmed alice 3 1 (at 3 9 1), Confirmed alice 3 2 (at 3 9 1)]) `shouldBe` Pending
    messageDelivery (last' [Confirmed alice 3 2 (at 3 9 1), Confirmed alice 3 3 (at 3 9 1)]) `shouldBe` Delivered
    commands (sent ++ [Confirmed alice 3 1 (at 3 9 1), FriendOnline alice False]) (FriendOnline alice True)
      `shouldBe` [SendMessage alice 3 long 1]

  it "counts what is received while its chat isn't in front of the user" $ do
    let unread events = contactUnread <$> Map.lookup alice (modelContacts (applying events sample))
        more = Received alice (at 3 9 0) Tox.MessageNormal "more"
    unread [] `shouldBe` Just 1
    unread [more] `shouldBe` Just 2
    unread [Open (Chat alice)] `shouldBe` Just 0
    unread [Shown True, Open (Chat alice), more] `shouldBe` Just 0
    unread [Shown True, Open (Chat alice), Shown False, more] `shouldBe` Just 1
    unread [Open (Chat alice), more, Shown True] `shouldBe` Just 0

  it "notifies of each contact's unread messages" $ do
    let notified events = Map.lookup alice (notifications (at 3 9 0) (applying events sample))
    notified [Received alice (at 3 9 0) Tox.MessageNormal "more"] `shouldBe` Just ["Looks good already.", "more"]
    let answered = [ CallInvited alice (at 3 9 0) False, AnswerCall (at 3 9 0) False, HangUp (at 3 9 1)
                   , Received alice (at 3 9 2) Tox.MessageNormal "more" ]
    map messageCall (messagesOf alice (applying answered sample))
      `shouldSatisfy` elem (Just (CallRecord False (Just 60) False))
    notified answered `shouldBe` Just ["Looks good already.", "more"]
    notified [Open (Chat alice)] `shouldBe` Nothing

  it "applies only the profile Tox takes, counting bytes" $ do
    let tooLong = Text.replicate 33 "\x1F600"
        (model, applied) = update ApplyProfile (applying [Open ProfileScreen, NicknameChanged tooLong] sample)
    profileNickname (modelProfile model) `shouldBe` profileNickname (modelProfile sample)
    applied `shouldBe` []
    profileError (Text.replicate 32 "\x1F600") "" `shouldBe` Nothing

  it "takes small files from friends, and asks before large ones" $ do
    let offer size = FileOffered alice (Receiving 0) (at 3 9 0) "a file" size "/received/a file" "id"
        files events = [ attachmentState f | m <- messagesOf alice (applying events sample), Just f <- [messageFile m] ]
        downloadIt = Download alice (length (messagesOf alice (applying [offer (autoAccepted + 1)] sample)) - 1)
    commands [] (offer autoAccepted) `shouldBe` [AcceptFile alice (Receiving 0) "/received/a file"]
    commands [] (offer (autoAccepted + 1)) `shouldBe` []
    files [offer (autoAccepted + 1)] `shouldBe` [Waiting]
    commands [offer (autoAccepted + 1)] downloadIt `shouldBe` [AcceptFile alice (Receiving 0) "/received/a file"]
    files [offer 10, FileMoved alice (Receiving 0) 5, FileEnded alice (Receiving 0) True] `shouldBe` [Complete]

  it "takes files up to the size the user chose, none when never" $ do
    let offer = FileOffered alice (Receiving 0) (at 3 9 0) "a file" (1024 * 1024) "/received/a file" "id"
    commands [SetAutoAccept (1024 * 1024)] offer `shouldBe` [AcceptFile alice (Receiving 0) "/received/a file"]
    commands [SetAutoAccept (1024 * 1024 - 1)] offer `shouldBe` []
    commands [SetAutoAccept 0] offer `shouldBe` []

  it "offers our file again after the friend was offline, and goes on with theirs" $ do
    let ours = [Open (Chat alice), Attach "/sent/a.png" "a.png" 10 "ours", Send (at 3 9 0)]
        i = length (messagesOf alice sample)
        theirs n = FileOffered alice (Receiving n) (at 3 9 1) "b.txt" 20 "/received/b.txt"
        cut = ours ++ [ FileStarted alice i (Sending 0), FileMoved alice (Sending 0) 5
                      , theirs 1 "theirs", FileMoved alice (Receiving 1) 5, FriendOnline alice False ]
        files events = [ attachmentState f | m <- messagesOf alice (applying events sample), Just f <- [messageFile m] ]
    commands (init ours) (Send (at 3 9 0)) `shouldBe` [SendFile alice i "/sent/a.png" "a.png" 10 (Just "ours")]
    files cut `shouldBe` [Waiting, Interrupted]
    commands cut (FriendOnline alice True) `shouldContain` [SendFile alice i "/sent/a.png" "a.png" 10 (Just "ours")]
    commands (cut ++ [FriendOnline alice True]) (FriendOnline alice True) `shouldBe` []
    -- Theirs offered again goes on in its message, a new one has its own.
    let returned = cut ++ [FriendOnline alice True]
    commands returned (theirs 2 "theirs") `shouldBe` [AcceptFile alice (Receiving 2) "/received/b.txt"]
    files (returned ++ [theirs 2 "theirs"]) `shouldBe` [Waiting, Moving]
    files (returned ++ [theirs 2 "other"]) `shouldBe` [Waiting, Interrupted, Moving]

  it "calls an online friend, talks once they answer, and hangs up" $ do
    let calling = [Open (Chat alice), StartCall (at 3 9 0) False]
        talking = calling ++ [CallPeer alice (at 3 9 0) True]
        phase events = callPhase <$> modelCall (applying events sample)
    commands [Open (Chat alice)] (StartCall (at 3 9 0) False) `shouldBe` [CallFriend alice False]
    commands [Open (Chat bob)] (StartCall (at 3 9 0) False) `shouldBe` []
    commands calling ToggleCamera `shouldBe` []
    phase talking `shouldBe` Just (Talking (at 3 9 0))
    callPeerVideo <$> modelCall (applying talking sample) `shouldBe` Just True
    commands talking ToggleCamera `shouldBe` [CameraCall alice True]
    -- Muting keeps our microphone's frames here, Tox has nothing to do.
    commands talking ToggleMute `shouldBe` []
    callMuted <$> modelCall (applying (talking ++ [ToggleMute]) sample) `shouldBe` Just True
    commands talking (HangUp (at 3 9 1)) `shouldBe` [HangUpCall alice]
    phase (talking ++ [HangUp (at 3 9 1)]) `shouldBe` Nothing

  it "answers a call, and turns away another during it" $ do
    let ringing = [CallInvited alice (at 3 9 0) False]
    callPhase <$> modelCall (applying ringing sample) `shouldBe` Just Ringing
    commands ringing (AnswerCall (at 3 9 0) True) `shouldBe` [AnswerFriend alice True]
    commands ringing (CallInvited bob (at 3 9 0) False) `shouldBe` [HangUpCall bob]
    modelCall (applying (ringing ++ [CallEnded bob (at 3 9 0)]) sample) `shouldBe` modelCall (applying ringing sample)
    modelCall (applying (ringing ++ [CallEnded alice (at 3 9 0)]) sample) `shouldBe` Nothing

  it "misses a call while another profile has one, and doesn't place ours" $ do
    let ringing = [CallInvited alice (at 3 9 0) False]
        missed = applying (ringing ++ [CallElsewhere (at 3 9 0)]) sample
    commands ringing (CallElsewhere (at 3 9 0)) `shouldBe` [HangUpCall alice]
    (modelCall missed, missedCalls (at 3 9 0) missed Map.!? alice) `shouldBe` (Nothing, Just 1)
    let placed = applying [Open (Chat alice), StartCall (at 3 9 0) False, CallElsewhere (at 3 9 0)] sample
    (modelCall placed, modelNotice placed) `shouldBe` (Nothing, Just "Another profile has a call")

  it "starts a video call on the loudspeaker and follows the platform's routes" $ do
    let route events = (\c -> (callRoute c, callRoutes c)) <$> modelCall (applying events sample)
    route [Open (Chat alice), StartCall (at 3 9 0) True] `shouldBe` Just (Speaker, [Earpiece, Speaker])
    route [CallInvited alice (at 3 9 0) False] `shouldBe` Just (Earpiece, [Earpiece, Speaker])
    route [CallInvited alice (at 3 9 0) False, RoutesChanged Bluetooth [Earpiece, Speaker, Bluetooth]]
      `shouldBe` Just (Bluetooth, [Earpiece, Speaker, Bluetooth])

  it "hides a call going on behind a banner, but not one ringing" $ do
    let talking = [Open (Chat alice), StartCall (at 3 9 0) False, CallPeer alice (at 3 9 0) False]
        hidden events = callHidden <$> modelCall (applying events sample)
    hidden (talking ++ [Back]) `shouldBe` Just True
    modelScreen (applying (talking ++ [Back]) sample) `shouldBe` Chat alice
    hidden (talking ++ [HideCall, ShowCall]) `shouldBe` Just False
    back (applying [CallInvited alice (at 3 9 0) False] sample) `shouldBe` Nothing

  it "settles a call both ends placed at once" $ do
    -- Our key is above Alice's and below Bob's.
    let placed cid = [FriendOnline bob True, Open (Chat cid), StartCall (at 3 9 0) False, CallGlared cid (at 3 9 0)]
        phase events = callPhase <$> modelCall (applying events sample)
        later n = addUTCTime n (at 3 9 0)
    commands (placed alice) (Tick (at 3 9 0)) `shouldBe` []
    phase (placed alice ++ [Tick (later 2), CallGlared alice (later 3)]) `shouldBe` Nothing
    commands (placed alice) (Tick (later 2)) `shouldBe` [CallFriend alice False]
    commands (placed bob) (Tick (later 2)) `shouldBe` []
    commands (placed bob) (CallInvited bob (at 3 9 0) False) `shouldBe` [AnswerFriend bob False]
    phase (placed bob ++ [CallInvited bob (at 3 9 0) False]) `shouldBe` Just (Talking (at 3 9 0))
    phase (placed bob ++ [Tick (later 8)]) `shouldBe` Nothing
    commands (placed alice) (CallInvited carol (at 3 9 0) False) `shouldBe` [HangUpCall carol]
    -- Their call reached Tox before ours went out.
    commands [Open (Chat alice), StartCall (at 3 9 0) False] (CallInvited alice (at 3 9 0) True)
      `shouldBe` [AnswerFriend alice False]

  it "takes a proxy only with a host and a port" $ do
    let device events = settingProxy (modelDevice (applying (Open SettingsScreen : Open ProxyScreen : events) sample))
        socks = [ProxyKindChosen (Just Tox.Socks5Proxy), ProxyHostChanged " 127.0.0.1 "]
    device (socks ++ [ProxyPortChanged "9050", ApplyProxy]) `shouldBe` Just (Tox.Proxy Tox.Socks5Proxy "127.0.0.1" 9050)
    device (socks ++ [ProxyPortChanged "70000", ApplyProxy]) `shouldBe` Nothing
    modelNotice (applying (Open ProxyScreen : socks ++ [ApplyProxy]) sample) `shouldBe` Just "The port is a number from 1 to 65535"

  it "gives up a call nobody answers" $ do
    let calling = [Open (Chat alice), StartCall (at 3 9 0) False]
    commands calling (Tick (at 3 9 0)) `shouldBe` []
    commands calling (Tick (at 3 9 1)) `shouldBe` [HangUpCall alice]
    modelCall (applying (calling ++ [Tick (at 3 9 1)]) sample) `shouldBe` Nothing
    commands (calling ++ [CallPeer alice (at 3 9 0) False]) (Tick (at 3 10 0)) `shouldBe` []

  it "keeps each call in its chat, with missed ones unread" $ do
    let entries events = [ (messageMine m, messageTime m, c) | m <- messagesOf alice (applying events sample), Just c <- [messageCall m] ]
        unread events = contactUnread <$> Map.lookup alice (modelContacts (applying events sample))
        talked = [Open (Chat alice), StartCall (at 3 9 0) True, CallPeer alice (at 3 9 0) True, HangUp (at 3 9 2)]
        missed = [CallInvited alice (at 3 9 0) False, CallEnded alice (at 3 9 1)]
    entries talked `shouldBe` [(True, at 3 9 0, CallRecord True (Just 120) False)]
    -- There while it goes, so it stays if hTox stops.
    entries (init talked) `shouldBe` [(True, at 3 9 0, CallRecord True Nothing False)]
    entries [CallInvited alice (at 3 9 0) False, HangUp (at 3 9 1)] `shouldBe` [(False, at 3 9 0, CallRecord False Nothing False)]
    entries missed `shouldBe` [(False, at 3 9 0, CallRecord False Nothing True)]
    unread missed `shouldBe` (succ <$> unread [])
    entries [CallInvited bob (at 3 9 0) False, CallInvited alice (at 3 9 1) True]
      `shouldBe` [(False, at 3 9 1, CallRecord True Nothing True)]
    map (preview "Alice") (filter ((/= Nothing) . messageCall) (messagesOf alice (applying (talked ++ missed) sample)))
      `shouldBe` ["Outgoing video call \x00B7 2:00", "Missed voice call"]

  it "keeps each chat's draft" $ do
    let model = applying [Open (Chat alice), DraftChanged "for Alice", Back, Open (Chat bob)] sample
    Map.lookup alice (modelDrafts model) `shouldBe` Just "for Alice"
    Map.lookup bob (modelDrafts model) `shouldBe` Nothing

  it "shows an action with who did it, and a message sent again once" $ do
    let waved = applying [Received alice (at 3 9 0) Tox.MessageAction "waves"] sample
        trifa = "again" <> BS.pack (0 : 0 : replicate 32 7 ++ [0, 0, 0, 1])
        twice = applying (replicate 2 (Received alice (at 3 9 1) Tox.MessageNormal trifa)) sample
    Map.lookup alice (notifications (at 3 9 2) waved) `shouldBe` Just ["Looks good already.", "* Alice waves"]
    drop 3 (map messageText (messagesOf alice twice)) `shouldBe` ["again"]

  it "keeps what is picked with the draft until sent, in order, or taken away" $ do
    let picked = [ Open (Chat alice), Attach "/sent/a.png" "a.png" 10 "a", ShareLocation 1 2
                 , Attach "/sent/b.txt" "b.txt" 20 "b", DraftChanged "look" ]
    commands picked (Unstage alice 0) `shouldBe` [ForgetFile "/sent/a.png"]
    commands (picked ++ [Unstage alice 0]) (Send (at 3 9 0))
      `shouldBe` [ SendMessage alice 3 "geo:1.000000,2.000000" 0
                 , SendFile alice 4 "/sent/b.txt" "b.txt" 20 (Just "b"), SendMessage alice 5 "look" 0 ]
    Map.lookup alice (modelStaged (applying (picked ++ [Send (at 3 9 0)]) sample)) `shouldBe` Nothing
    commands picked (DeleteContact alice False) `shouldContain` [ForgetFile "/sent/a.png", ForgetFile "/sent/b.txt"]

  it "sends where the user is as a place, which the list shows as one" $ do
    let shared = [Open (Chat alice), ShareLocation 52.37 4.9, Send (at 3 9 0)]
    commands (init shared) (Send (at 3 9 0)) `shouldBe` [SendMessage alice 3 "geo:52.370000,4.900000" 0]
    map (preview "Alice") (drop 3 (messagesOf alice (applying shared sample))) `shouldBe` ["\x1F4CD Location"]

  it "shows friends our picture, now and when they come back" $ do
    let picked = [AvatarPicked "png"]
    commands [] (AvatarPicked "png") `shouldBe` [SendAvatar alice (Just "png")]
    commands (picked ++ [FriendOnline alice False]) (FriendOnline alice True) `shouldSatisfy` elem (SendAvatar alice (Just "png"))
    commands picked RemoveAvatar `shouldBe` [SendAvatar alice Nothing]
    Map.size (modelAvatars (applying (picked ++ [RemoveAvatar]) sample)) `shouldBe` 0

  it "keeps the picture a friend shows while it shows it" $ do
    let shown' = applying [FriendAvatar alice (Just "theirs")] sample
    ((`Map.lookup` modelAvatars shown') =<< contactAvatar =<< Map.lookup alice (modelContacts shown'))
      `shouldBe` Just "theirs"
    modelAvatars (applying [FriendAvatar alice (Just "theirs"), FriendAvatar alice Nothing] sample) `shouldBe` Map.empty

  it "orders the contacts by their latest message, the rest by name" $ do
    map fst (contactOrder sample) `shouldBe` [alice, bob]
    map fst (contactOrder (applying [Received bob (at 3 9 0) Tox.MessageNormal "now"] sample)) `shouldBe` [bob, alice]

  it "notifies of missed calls apart from messages" $ do
    let missed = applying [CallInvited alice (at 3 9 0) False, CallEnded alice (at 3 9 1)] sample
    Map.lookup alice (notifications (at 3 9 2) missed) `shouldBe` Just ["Looks good already."]
    Map.lookup alice (missedCalls (at 3 9 2) missed) `shouldBe` Just 1
    Map.lookup alice (missedCalls (at 3 9 2) sample) `shouldBe` Nothing

  it "notifies of no muted chat until its mute ends" $ do
    let mutedUntil end = applying [MuteChat alice end] sample
    Map.member alice (notifications (at 2 12 0) (mutedUntil (Just (at 2 13 0)))) `shouldBe` False
    Map.member alice (notifications (at 2 14 0) (mutedUntil (Just (at 2 13 0)))) `shouldBe` True
    Map.member alice (notifications (at 9 0 0) (mutedUntil Nothing)) `shouldBe` False
    Map.member alice (notifications (at 2 12 0) (applying [UnmuteChat alice] (mutedUntil Nothing))) `shouldBe` True

  it "keeps a friend's status while online only" $ do
    let status events = contactStatus <$> Map.lookup alice (modelContacts (applying events sample))
    status [FriendStatus alice Tox.Busy] `shouldBe` Just Tox.Busy
    status [FriendStatus alice Tox.Busy, FriendOnline alice False] `shouldBe` Just Tox.Online

  it "goes back from a contact's page to its chat" $
    modelScreen <$> back (applying [Open (Details alice)] sample) `shouldBe` Just (Chat alice)

  it "deletes a contact and tells Tox, keeping its chat when asked" $ do
    let ContactId key = bob
        address = toxId key "DEADBEEF"
        kept = applying [Open (Chat bob), DraftChanged "hi", Send (at 3 9 0), DeleteContact bob True] sample
    commands [] (DeleteContact bob False) `shouldBe` [Unfriend bob]
    Map.member bob (modelContacts kept) `shouldBe` False
    Map.member bob (modelFormer (applying [DeleteContact bob False] sample)) `shouldBe` False
    let again = applying [Open AddContact, AddressChanged address, AddFriend] kept
    map messageText (messagesOf bob again) `shouldBe` ["hi"]
    Map.member bob (modelFormer again) `shouldBe` False

  it "ends a call with a contact it deletes" $ do
    let calling = [Open (Chat alice), StartCall (at 3 9 0) False]
    commands calling (DeleteContact alice False) `shouldBe` [HangUpCall alice, Unfriend alice]
    modelCall (applying (calling ++ [DeleteContact alice False]) sample) `shouldBe` Nothing

  it "selects messages until Back, which leaves the chat only then" $ do
    let selected = applying [Open (Chat alice), Select 0, Select 2, Select 0] sample
    modelSelected selected `shouldBe` Set.fromList [2]
    ((\m -> (modelScreen m, modelSelected m)) <$> back selected) `shouldBe` Just (Chat alice, Set.empty)

  it "answers a message with its lines quoted" $ do
    let replied = [Open (Chat bob), DraftChanged "two\nlines", Send (at 3 9 0), Reply 0, DraftChanged "ok"]
    commands (replied ++ [FriendOnline bob True]) (Send (at 3 9 1)) `shouldBe` [SendMessage bob 1 "> two\n> lines\nok" 0]
    map messageText (messagesOf bob (applying (replied ++ [Send (at 3 9 1)]) sample)) `shouldBe` ["two\nlines", "> two\n> lines\nok"]
    Map.lookup bob (modelReplies (applying (replied ++ [Send (at 3 9 1)]) sample)) `shouldBe` Nothing

  it "copies one message as its text, several with whose they were" $ do
    copiedText sample alice [1] `shouldBe` "It is, and it's on the network now."
    copiedText sample alice [0, 1] `shouldBe` "Alice: Hi! Is this the Haskell client?\nTox User: It is, and it's on the network now."

  it "forwards messages in order, sending them once the friend is online" $ do
    let forwarding target = [Open (Forward alice [0, 2]), ForwardTo target (at 3 9 0)]
    commands [Open (Forward alice [0, 2])] (ForwardTo alice (at 3 9 0))
      `shouldBe` [ SendMessage alice 3 "Hi! Is this the Haskell client?" 0
                 , SendMessage alice 4 "Looks good already." 0 ]
    commands [Open (Forward alice [0, 2])] (ForwardTo bob (at 3 9 0)) `shouldBe` []
    map messageText (messagesOf bob (applying (forwarding bob) sample))
      `shouldBe` ["Hi! Is this the Haskell client?", "Looks good already."]
    modelScreen (applying (forwarding bob) sample) `shouldBe` Chat bob

  it "counts what comes while the chat is scrolled away, until it is back" $ do
    let away = [Open (Chat alice), ScrolledAway True, Received alice (at 3 9 0) Tox.MessageNormal "one"]
    modelUnseen (applying away sample) `shouldBe` 1
    modelUnseen (applying (away ++ [ScrolledAway False]) sample) `shouldBe` 0
    modelJump (applying (away ++ [JumpToEnd]) sample) `shouldBe` modelJump sample + 1

  it "accepts or rejects a friend request" $ do
    Map.member carol (modelRequests sample) `shouldBe` True
    let accepted = applying [Accept carol] sample
    Map.member carol (modelContacts accepted) `shouldBe` True
    Map.member carol (modelRequests accepted) `shouldBe` False
    commands [] (Accept carol) `shouldBe` [Befriend carol]
    let rejected = applying [Reject carol] sample
    Map.member carol (modelContacts rejected) `shouldBe` False
    Map.member carol (modelRequests rejected) `shouldBe` False

  it "keeps a rejected request away while it says the same" $ do
    let again message = Map.member carol (modelRequests (applying [Reject carol, Requested carol message] sample))
    again "Hey, I'm Carol, come chat with me on Tox!" `shouldBe` False
    again "It's me again" `shouldBe` True

  it "adds a contact by Tox ID, and opens its chat" $ do
    let key = Text.replicate 8 "0123ABCD"
        address = toxId key "DEADBEEF"
        events = [Open AddContact, AddressChanged ("tox:" <> address)]
        model = applying (events ++ [AddFriend]) sample
    modelScreen model `shouldBe` Chat (ContactId key)
    Map.member (ContactId key) (modelContacts model) `shouldBe` True
    modelAddress model `shouldBe` ""
    commands events AddFriend
      `shouldBe` [RequestFriend address "Hey, I'm Tox User, come chat with me on Tox!"]

  it "sends our request again after a restart, until the friend is online" $ do
    let key = Text.replicate 8 "0123ABCD"
        address = toxId key "DEADBEEF"
        added = [Open AddContact, AddressChanged address, AddFriend]
        request = RequestFriend address "Hey, I'm Tox User, come chat with me on Tox!"
    commands added (Started "") `shouldSatisfy` elem request
    commands (added ++ [FriendOnline (ContactId key) True]) (Started "")
      `shouldSatisfy` elem (Befriend (ContactId key))

  it "takes back a contact Tox refused, saying why" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
        model = applying [Open AddContact, AddressChanged address, AddFriend, AddFailed address "No"] sample
    modelContacts model `shouldBe` modelContacts sample
    (modelScreen model, modelAddress model, modelAddError model) `shouldBe` (AddContact, address, Just "No")

  it "sends no request Tox would refuse" $ do
    let events = [Open AddContact, AddressChanged (toxId (Text.replicate 8 "0123ABCD") "DEADBEEF")]
    commands (events ++ [RequestChanged "   "]) AddFriend `shouldBe` []
    requestError (Text.replicate 921 "a") `shouldBe` Nothing
    requestError (Text.replicate 922 "a") `shouldBe` Just "The message is too long"

  it "adds nothing that isn't a Tox ID" $ do
    let events = [Open AddContact, AddressChanged "C0FFEE"]
    modelContacts (applying (events ++ [AddFriend]) sample) `shouldBe` modelContacts sample
    commands events AddFriend `shouldBe` []

  it "finds a typo by the checksum" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
        typo = Text.take 10 address <> "F" <> Text.drop 11 address
    addressError "" address `shouldBe` Nothing
    addressError "" typo `shouldBe` Just "Not a valid Tox ID, please check it for typos"

  it "adds no contact of ourselves" $ do
    let ours = profileToxId (modelProfile sample)
    addressError ours (Text.toLower ours) `shouldBe` Just "This is your own Tox ID"

  it "keeps profile edits until they are applied" $ do
    let events = [Open ProfileScreen, NicknameChanged "Toxer"]
    profileNickname (modelProfile (applying events sample)) `shouldBe` "Tox User"
    profileNickname (modelProfile (applying (events ++ [ApplyProfile]) sample)) `shouldBe` "Toxer"
    commands events ApplyProfile `shouldBe` [SetProfile "Toxer" "Toxing on Haskell"]

  it "tells a new Tox the profile and friends" $
    snd (update (Started "") sample)
      `shouldBe` [SetProfile "Tox User" "Toxing on Haskell", Befriend alice, Befriend bob]

  it "goes back from the drawer, then the screen, then out" $ do
    modelDrawer <$> back (applying [OpenDrawer] sample) `shouldBe` Just False
    modelScreen <$> back (applying [Open ProfileScreen] sample) `shouldBe` Just Contacts
    back sample `shouldBe` Nothing
