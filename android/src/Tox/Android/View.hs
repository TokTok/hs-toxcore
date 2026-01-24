{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The widgets of each screen.
module Tox.Android.View
  ( Env (..)
  , Handlers (..)
  , view
  , startFailure
  , peerStream
  , selfStream
  ) where

import qualified Data.ByteString       as BS
import           Data.Char             (toLower)
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (isJust, isNothing, listToMaybe)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Time             (TimeZone, UTCTime, defaultTimeLocale,
                                        diffDays, diffUTCTime, formatTime,
                                        localDay, utcToLocalTime)
import           Data.Word             (Word64)
import           Hatter.Action         (Action, OnChange)
import           Hatter.Widget
import           Numeric               (showFFloat)
import           System.FilePath       (takeExtension)
import qualified Tox.Application.Tox   as Tox

import           Tox.Android.Emoji     (emojiOnly)
import qualified Tox.Android.Formats   as Formats
import           Tox.Android.Locations (location)
import           Tox.Android.Model
import           Tox.Android.Pictures  (PictureFormat (..), pictureFormat)
import           Tox.Android.Theme
import           Tox.Android.Version   (version)

-- | What the widgets show besides the model.
data Env = Env
  { envTheme       :: Theme
  , envNow         :: UTCTime
  , envZone        :: TimeZone
  , envAvatar      :: Text -> BS.ByteString
    -- ^ A key's identicon, as a PNG.
  , envQr          :: BS.ByteString
    -- ^ Our Tox ID's QR code, as a PNG.
  , envPictureSize :: FilePath -> Maybe (Int, Int)
    -- ^ A picture file's width and height, where known.
  , envDecoded     :: FilePath -> Maybe BS.ByteString
    -- ^ A picture in a format the platform can't read, as one it can,
    -- once decoded.
  , envClock24     :: Bool
    -- ^ Whether the user wants times on a 24-hour clock.
  }

-- | The handles the widgets fire.
data Handlers = Handlers
  { hBack               :: Action
  , hSend               :: Action
  , hOpenDrawer         :: Action
  , hOpenProfile        :: Action
  , hOpenAddContact     :: Action
  , hOpenChats          :: Action
  , hOpenCalls          :: Action
  , hCallBack           :: ContactId -> Action
  , hAllowFullScreen    :: Action
    -- ^ A voice call to the contact, from the list of calls.
  , hApplyProfile       :: Action
  , hAddFriend          :: Action
  , hCopyToxId          :: Action
  , hAttach             :: Action
  , hTapMessage         :: ContactId -> MessageId -> Action
  , hHoldMessage        :: ContactId -> MessageId -> Action
  , hCancelFile         :: ContactId -> MessageId -> Action
  , hUnselect           :: Action
  , hCopySelected       :: Action
  , hForwardSelected    :: Action
  , hCancelReply        :: Action
  , hUnstage            :: ContactId -> Int -> Action
  , hOpenSettings       :: Action
  , hOpenProfiles       :: Action
  , hSwitchProfile      :: ProfileId -> Action
  , hDeleteProfile      :: ProfileId -> Action
  , hNewProfile         :: Action
  , hImportProfile      :: Action
  , hPickAvatar         :: Action
  , hRemoveAvatar       :: Action
  , hExport             :: Action
  , hAutoAccept         :: Action
  , hToggleUdp          :: Action
  , hToggleIpv6         :: Action
  , hToggleLan          :: Action
  , hToggleStay         :: Action
  , hToggleFormat       :: Text -> Action
    -- ^ By the format's key.
  , hOpenProxy          :: Action
  , hProxyKind          :: Maybe Tox.ProxyType -> Action
  , hProxyHost          :: OnChange
  , hProxyPort          :: OnChange
  , hApplyProxy         :: Action
  , hForwardTo          :: ContactId -> Action
    -- ^ Opens a file that is there, or takes one that waits.
  , hOpenChat           :: ContactId -> Action
  , hAccept             :: ContactId -> Action
  , hReject             :: ContactId -> Action
  , hDetails            :: ContactId -> Action
  , hMuteChat           :: ContactId -> Action
  , hDelete             :: ContactId -> Action
  , hCopyKey            :: ContactId -> Action
  , hPictures           :: ContactId -> Action
    -- ^ Offers the ways pictures can go to the contact.
  , hDraft              :: OnChange
  , hNickname           :: OnChange
  , hStatus             :: OnChange
  , hAddress            :: OnChange
  , hRequest            :: OnChange
  , hCall               :: Bool -> Action
    -- ^ With the camera on or off, like 'hAnswer'.
  , hAnswer             :: Bool -> Action
  , hHangUp             :: Action
  , hMute               :: Action
  , hCamera             :: Action
  , hRoute              :: Action
    -- ^ Switches between earpiece and loudspeaker, or offers the routes
    -- when there are more.
  , hFlipCamera         :: Action
  , hHideCall           :: Action
  , hShowCall           :: Action
  , hScrolled           :: OnChange
  , hJump               :: Action
  , hOpenGroup          :: GroupId -> Action
  , hGroupDetails       :: GroupId -> Action
  , hMuteGroup          :: GroupId -> Action
  , hLeaveGroup         :: GroupId -> Action
  , hDeleteGroup        :: GroupId -> Action
  , hCopyChatId         :: GroupId -> Action
  , hOpenInvite         :: ContactId -> GroupId -> Action
  , hJoin               :: ContactId -> GroupId -> Action
  , hDecline            :: ContactId -> GroupId -> Action
  , hPassword           :: OnChange
  , hEditTopic          :: GroupId -> Action
  , hToggleMentions     :: GroupId -> Action
  , hTopic              :: OnChange
  , hApplyTopic         :: Action
  , hFounding           :: Founding -> Action
  , hGroupName          :: OnChange
  , hFound              :: Action
  , hChatId             :: OnChange
  , hJoinPublic         :: Action
  , hInviteFriends      :: GroupId -> Action
  , hInviteFriend       :: GroupId -> ContactId -> Action
  , hMember             :: GroupId -> Tox.PeerId -> Action
    -- ^ Offers what can be done with a member.
  , hEndPrivate         :: Action
  , hGroupSettings      :: GroupId -> Action
  , hPrivacy            :: GroupId -> Action
  , hTopicLock          :: GroupId -> Action
  , hVoice              :: GroupId -> Action
    -- ^ Offers who may speak.
  , hRelayBot           :: GroupId -> Action
  , hStopRelay          :: GroupId -> Action
  , hPeerLimit          :: OnChange
  , hGroupPassword      :: OnChange
  , hApplyGroupSettings :: Action
  , hOpenLink           :: Action
  , hLinkEdited         :: OnChange
  , hLink               :: Action
  }

view :: Env -> Handlers -> Model -> Widget
view env h model = case modelCall model of
  Just c | Just contact <- Map.lookup (callContact c) (modelContacts model) ->
    if callHidden c
      then column [callBanner env h c, grow (screen env h model)]
      else callScreen env h c contact (modelNotice model)
  _ -> screen env h model

-- | What the user opened.
screen :: Env -> Handlers -> Model -> Widget
screen env h model = case modelScreen model of
      Contacts      -> contacts env h model
      Chat cid      -> maybe (contacts env h model) (chat env h model cid)
                         (Map.lookup cid (modelContacts model))
      Details cid   -> maybe (contacts env h model) (details env h cid)
                         (Map.lookup cid (modelContacts model))
      Forward _ _   -> forward env h model
      MessageInfo cid i
        | Just contact <- Map.lookup cid (modelContacts model)
        , m : _ <- drop i (contactMessages contact) -> messageInfo env h cid contact m
        | otherwise -> contacts env h model
      CallsScreen   -> calls env h model
      ProfileScreen -> profile env h model
      SettingsScreen -> settings env h model
      ProfilesScreen -> profiles env h model
      ProxyScreen   -> proxy env h model
      AddContact    -> addContact env h model
      GroupChat gid -> maybe (contacts env h model) (groupChat env h model gid)
                         (Map.lookup gid (modelGroups model))
      GroupDetails gid -> maybe (contacts env h model) (groupDetails env h model gid)
                         (Map.lookup gid (modelGroups model))
      InviteScreen cid gid
        | Just invite <- Map.lookup (cid, gid) (modelInvites model) -> inviteScreen env h model cid gid invite
        | otherwise -> contacts env h model
      TopicScreen gid -> maybe (contacts env h model) (topicScreen env h model) (Map.lookup gid (modelGroups model))
      NewGroupScreen -> founding env h model
      GroupSettingsScreen gid -> maybe (contacts env h model) (groupSettingsScreen env h model gid) (Map.lookup gid (modelGroups model))
      JoinGroupScreen -> joinGroup env h model
      InviteFriendsScreen gid -> maybe (contacts env h model) (inviteFriends env h model gid) (Map.lookup gid (modelGroups model))
      LinkScreen -> linking env h model

-- | The chats, with invites and friend requests above them.
contacts :: Env -> Handlers -> Model -> Widget
contacts env h model = stack $
  column
      [ appBar t
          [ iconButton t (hOpenDrawer h) "ic_menu"
          , gap t 20
          , icon (if modelConnection model == Offline then "ic_offline_bolt" else "ic_online_prediction")
          , gap t 16
          , grow (label 22 (pText p) ("Tox (" <> profileNickname (modelProfile model) <> ")"))
          ]
      , grow $ stack
          [ if null (chatOrder model) && Map.null (modelRequests model) && Map.null (modelInvites model)
              then stack [empty]
              else scrollColumn $ map inviteRow (Map.toList (modelInvites model))
                ++ map requestRow (Map.toList (modelRequests model))
                ++ map chatRow (chatOrder model)
          , Tappable (hOpenAddContact h) $ styled
              (\s -> s { wsBackgroundColor = Just (pMine p), wsCornerRadius = Just (dp t 16)
                       , wsPadding = Just (dp t 18), wsMargin = Just (dp t 16)
                       , wsAlignSelf = Just SelfEnd })
              (icon "ic_add")
          ]
      , tabs env h Contacts
      ]
  : [ drawer | modelDrawer model ]
  where
    t = envTheme env
    p = thPalette t
    empty = middle $ padded t 16 $ column
      [ middle $ styled (\s -> s { wsWidth = Just (dp t 48), wsHeight = Just (dp t 48) })
          (icon "ic_chat_bubble_outline")
      , gap t 16
      , middle (label 16 (pText p) "No chats yet")
      , gap t 4
      , middle (label 14 (pMuted p) "Add a contact with the + button to start chatting")
      ]
    requestRow (cid@(ContactId key), message) = padded t 14 $ row
      [ middle (avatar env 48 key Nothing)
      , gap t 16
      , grow $ middle $ column
          [ maxLines 2 (label 16 (pText p) message)
          , maxLines 1 (label 14 (pMuted p) key)
          ]
      , middle (iconButton t (hAccept h cid) "ic_check")
      , middle (iconButton t (hReject h cid) "ic_close")
      ]
    inviteRow ((cid, gid@(GroupId chatKey)), invite) = Tappable (hOpenInvite h cid gid) $ padded t 14 $ row
      [ middle (avatar env 48 chatKey Nothing)
      , gap t 16
      , grow $ middle $ column
          [ maxLines 1 (labelIn bold 16 (pText p) (inviteLabel invite))
          , maxLines 2 $ label 14 (pMuted p) $ case inviteAnswered invite of
              Just _  -> "Joins when " <> friendName cid <> " is online"
              Nothing -> friendName cid <> " invited you to " <> article (kindName (inviteKind invite))
          ]
      , middle (iconButton t (hDecline h cid gid) "ic_close")
      ]
    friendName cid = maybe "A friend" (contactLabel cid) (Map.lookup cid (modelContacts model))
    chatRow = \case
      FriendRow cid contact -> contactRow (cid, contact)
      GroupRow gid g -> groupRow gid g
    groupRow gid@(GroupId chatKey) g = Tappable (hOpenGroup h gid) $ padded t 14 $ row
      [ middle (avatar env 48 chatKey Nothing)
      , gap t 16
      , grow $ middle $ column
          [ row [ middle (styled (\s -> s { wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) }) (icon "ic_group"))
                , gap t 6
                , grow (middle (labelIn (if groupUnread g > 0 then bold else id) 16 (pText p) (groupLabel g))) ]
          , case Map.lookup gid (modelGroupDrafts model) of
              Just draft | not (Text.null (Text.strip draft)) -> row
                [label 14 (pError p) "Draft: ", grow (maxLines 1 (label 14 (pMuted p) draft))]
              _ -> maxLines 1 (label 14 (pMuted p) (maybe (phaseText (groupPhase g)) (entryPreview (uttered (modelProfile model) g)) (lastEntry g)))
          ]
      , middle $ column $
          styled (\s -> s { wsAlignSelf = Just SelfEnd })
            (label 12 (pMuted p) (maybe "" (listTime env . entryTime) (lastEntry g)))
          : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
                (icon "ic_notifications_off")
            | groupMuted (envNow env) g ]
          ++ [ styled (\s -> s { wsAlignSelf = Just SelfEnd
                            , wsBackgroundColor = Just (if groupMuted (envNow env) g then pMuted p else pAccent p)
                            , wsCornerRadius = Just (dp t 10), wsPadding = Just (dp t 4)
                            , wsMargin = Just (dp t 2) }) $
              label 12 (pOnAccent p) (Text.pack (show (groupUnread g)))
          | groupUnread g > 0 ]
      ]
    contactRow (cid@(ContactId key), contact) = Tappable (hOpenChat h cid) $ padded t 14 $ row
      [ middle (avatar env 48 key (Just (contactOnline contact)))
      , gap t 16
      , grow $ middle $ column
          -- Bold while it has unread messages.
          [ labelIn (if contactUnread contact > 0 then bold else id) 16 (pText p) (contactLabel cid contact)
          , case Map.lookup cid (modelDrafts model) of
              Just draft | not (Text.null (Text.strip draft)) -> row
                [label 14 (pError p) "Draft: ", grow (maxLines 1 (label 14 (pMuted p) draft))]
              _ -> maxLines 1 (label 14 (pMuted p) (maybe (waiting contact) (preview (contactLabel cid contact)) (lastMessage contact)))
          ]
      , middle $ column $
          styled (\s -> s { wsAlignSelf = Just SelfEnd })
            (label 12 (pMuted p) (maybe "" (listTime env . messageTime) (lastMessage contact)))
          : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
                (icon "ic_notifications_off")
            | muted (envNow env) contact ]
          ++ [ styled (\s -> s { wsAlignSelf = Just SelfEnd
                            , wsBackgroundColor = Just (if muted (envNow env) contact then pMuted p else pAccent p)
                            , wsCornerRadius = Just (dp t 10), wsPadding = Just (dp t 4)
                            , wsMargin = Just (dp t 2) }) $
              label 12 (pOnAccent p) (Text.pack (show (contactUnread contact)))
          | contactUnread contact > 0 ]
      ]
    waiting contact = maybe "" (const "Friend request sent") (contactRequest contact)
    drawer = stack
      [ Tappable (hBack h) $ styled
          (\s -> s { wsBackgroundColor = Just (pScrim p), wsAlignSelf = Just SelfStretch })
          (text "")
      , styled (\s -> s { wsBackgroundColor = Just (pBar p), wsAlignSelf = Just SelfStretch
                        , wsWidth = Just (thScreenWidth t * 0.8) }) $ column
          [ Tappable (hOpenProfile h) $ padded t 24 $ row
              [ grow $ middle $ column
                  [ label 24 (pText p) (profileNickname (modelProfile model))
                  , label 14 (pMuted p) (profileStatus (modelProfile model))
                  ]
              , middle (avatar env 48 (ownKey model) Nothing)
              ]
          , divider t
          , Tappable (hOpenProfiles h) $ padded t 24 $ label 16 (pText p) "Profiles"
          , Tappable (hOpenSettings h) $ padded t 24 $ label 16 (pText p) "Settings"
          ]
      ]

-- | Between the chats and the calls, under both.
tabs :: Env -> Handlers -> Screen -> Widget
tabs env h current = styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ row
  [ tab Contacts (hOpenChats h) "ic_chat_bubble_outline" "Chats"
  , tab CallsScreen (hOpenCalls h) "ic_call" "Calls"
  ]
  where
    t = envTheme env
    p = thPalette t
    tab screen' action name caption = grow $ Tappable action $ padded t 8 $ column
      [ middle $ styled (\s -> s { wsWidth = Just (dp t 24), wsHeight = Just (dp t 24) }) (icon name)
      , middle (label 12 (if screen' == current then pAccent p else pMuted p) caption)
      ]

-- | Every call, newest first, each calling back.
calls :: Env -> Handlers -> Model -> Widget
calls env h model = column $
  [ appBar t [gap t 16, grow (label 22 (pText p) "Calls")] ]
  ++ [ styled (\s -> s { wsBackgroundColor = Just (pTheirs p) }) $ padded t 12 $ row
         [ grow $ middle $ label 14 (pText p) "Calls can't ring over the lock screen"
         , middle (pill t (pMine p) (hAllowFullScreen h) "Allow")
         ]
     | not (modelFullScreen model) ]
  ++
  [ grow $ case [ (cid, c, m, record) | (cid, m, record) <- callHistory model
                                      , Just c <- [Map.lookup cid (modelContacts model)] ] of
      []      -> stack [middle (label 16 (pMuted p) "No calls yet")]
      history -> scrollColumn (map callRow history)
  , tabs env h CallsScreen
  ]
  where
    t = envTheme env
    p = thPalette t
    callRow (cid@(ContactId key), contact, m, record) = Tappable (hOpenChat h cid) $ padded t 14 $ row $
      [ middle (avatar env 48 key (Just (contactOnline contact)))
      , gap t 16
      , grow $ middle $ column
          [ label 16 (if recordMissed record then pError p else pText p) (contactLabel cid contact)
          , row
              [ middle $ styled (\s -> s { wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
                  (icon (callIcon (messageMine m) record))
              , gap t 4
              , label 14 (pMuted p) $ listTime env (messageTime m)
                  <> maybe "" (\talked -> " \x00B7 " <> duration (round talked)) (recordDuration record)
              ]
          ]
      ]
      ++ [ middle (iconButton t (hCallBack h cid) "ic_call") | contactOnline contact ]

-- | Which way a call went, or that it was missed.
callIcon :: Bool -> CallRecord -> Text
callIcon mine record
  | recordMissed record = "ic_call_missed"
  | mine = "ic_call_made"
  | otherwise = "ic_call_received"

chat :: Env -> Handlers -> Model -> ContactId -> Contact -> Widget
chat env h model cid@(ContactId key) contact = column $
  [ if not (Set.null (modelSelected model))
      then styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ appBar t
        [ iconButton t (hUnselect h) "ic_close"
        , gap t 8
        , grow (middle (label 22 (pText p) (Text.pack (show (Set.size (modelSelected model))) <> " selected")))
        , iconButton t (hCopySelected h) "ic_content_copy"
        , iconButton t (hForwardSelected h) "ic_forward"
        ]
      else styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ appBar t $
      [ iconButton t (hBack h) "ic_arrow_back"
      , grow $ Tappable (hDetails h cid) $ row
          [ middle (avatar env 40 key (Just (contactOnline contact)))
          , gap t 16
          , grow (middle (label 22 (pText p) (contactLabel cid contact)))
          ]
      ]
      ++ concat [ [iconButton t (hCall h True) "ic_videocam", iconButton t (hCall h False) "ic_call"]
                | contactOnline contact ]
  , grow $ stack $
      Scrolled (hScrolled h) (modelJump model)
        (styled (\s -> s { wsPadding = Just (dp t 10), wsKeepAtEnd = Just True })
          (LazyColumn (messages Nothing (zip [0 ..] (contactMessages contact)))))
      -- Back to the newest, with how many came meanwhile.
      : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $ column
            [ grow (text "")
            , Tappable (hJump h) $ styled
                (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 24)
                         , wsPadding = Just (dp t 12) }) $
                label 16 (pText p) (if modelUnseen model > 0 then "\x2193 " <> Text.pack (show (modelUnseen model)) else "\x2193")
            ]
        | modelAway model ]
  ]
  ++ [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 8) }) (label 14 (pError p) notice)
     | Just notice <- [modelNotice model] ]
  -- What goes with the draft.
  ++ [ styled (\s -> s { wsPadding = Just (dp t 8) }) $ scrollRow (zipWith tile [0 ..] staged) | not (null staged) ]
  -- What the draft answers.
  ++ [ styled (\s -> s { wsBackgroundColor = Just (pTheirs p), wsPadding = Just (dp t 8)
                       , wsMargin = Just (dp t 8), wsCornerRadius = Just (dp t 12) }) $ row
         [ middle (icon "ic_reply")
         , gap t 8
         , grow $ middle $ maxLines 3 $ label 14 (pMuted p) (quoted m)
         , middle (iconButton t (hCancelReply h) "ic_close")
         ]
     | Just i <- [Map.lookup cid (modelReplies model)], m : _ <- [drop i (contactMessages contact)] ]
  ++
  [ row
      [ grow $ middle $ styled
          (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 28)
                   , wsPadding = Just (dp t 16), wsMargin = Just (dp t 8) })
          (maxLines 3 (input InputMultiline (hDraft h) "Tox message" draft))
      -- Attach until there is something to send.
      , middle $ Tappable (if writing then hSend h else hAttach h) $ styled
          (\s -> s { wsBackgroundColor = Just (pMine p), wsCornerRadius = Just (dp t 26)
                   , wsPadding = Just (dp t 14), wsMargin = Just (dp t 8) })
          (icon (if writing then "ic_send" else "ic_attach_file"))
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    draft = Map.findWithDefault "" cid (modelDrafts model)
    staged = Map.findWithDefault [] cid (modelStaged model)
    writing = not (Text.null (Text.strip draft)) || not (null staged)
    -- Something picked, with an X to take it away again.
    tile k picked = styled (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 12)
                                  , wsMargin = Just (dp t 4), wsPadding = Just (dp t 4) }) $ column
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd }) (iconButton t (hUnstage h cid k) "ic_close")
      , case picked of
          StagedFile path name _ _
            | isPicture path || isJust (envDecoded env path) ->
                styled (\s -> s { wsWidth = Just (dp t 96), wsHeight = Just (dp t 96), wsCornerRadius = Just (dp t 8) }) $
                  Image ImageConfig
                    { icSource = maybe (ImageFile path) ImageData (envDecoded env path), icScaleType = ScaleFill }
            | otherwise -> styled (\s -> s { wsWidth = Just (dp t 96) }) $ column
                [ middle $ styled (\s -> s { wsWidth = Just (dp t 36), wsHeight = Just (dp t 36) }) (icon "ic_insert_drive_file")
                , maxLines 2 (label 12 (pText p) name) ]
          StagedPlace latitude longitude -> styled (\s -> s { wsWidth = Just (dp t 96) }) $ column
            [ label 28 (pText p) "\x1F4CD"
            , label 12 (pMuted p) (Text.pack (showFFloat (Just 3) latitude "" <> ", " <> showFFloat (Just 3) longitude "")) ]
      ]
    -- A date before each day's first message.
    messages _ [] = []
    messages previous ((i, m) : ms) =
      [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 8) }) $
          label 14 (pMuted p) (dayHeading env (messageTime m))
      | Just (day m) /= previous ]
      ++ held i (bubble i m (maybe True (not . together m . snd) (listToMaybe ms))) : messages (Just (day m)) ms
    -- Every message taps and holds, and shows whether it is selected.
    held i b = Tappable (hTapMessage h cid i) $ LongPressable (hHoldMessage h cid i) $
      if Set.member i (modelSelected model)
        then styled (\s -> s { wsBackgroundColor = Just (pDivider p), wsAlignSelf = Just SelfStretch }) (column [b])
        else b
    day = localDay . utcToLocalTime (envZone env) . messageTime
    -- A run: text from the same side, close together, which shows the time
    -- of its last message only.
    together m n = messageMine m == messageMine n && day m == day n
      && diffUTCTime (messageTime n) (messageTime m) < 180
      && all (\x -> isNothing (messageCall x) && isNothing (messageFile x)) [m, n]
    box m = styled
      (\s -> s { wsBackgroundColor = Just (if messageMine m then pMine p else pTheirs p)
               , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12)
               , wsMargin = Just (dp t 4)
               , wsAlignSelf = Just (if messageMine m then SelfEnd else SelfStart) })
    bubble i m lastOfRun = case (messageCall m, messageFile m) of
      (Just record, _) -> styled
        (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 16)
                 , wsPadding = Just (dp t 8), wsMargin = Just (dp t 8), wsAlignSelf = Just SelfCenter }) $ row
        [ middle $ styled (\s -> s { wsWidth = Just (dp t 16), wsHeight = Just (dp t 16) })
            (icon (callIcon (messageMine m) record))
        , gap t 8
        , middle (label 13 (if recordMissed record then pError p else pText p) (preview "" m))
        , gap t 8
        , middle (stamp m)
        ]
      (_, Nothing)
        | messageKind m == Tox.MessageNormal, emojiOnly (messageText m) ->
            styled (\s -> s { wsMargin = Just (dp t 4), wsAlignSelf = Just (if messageMine m then SelfEnd else SelfStart) }) $
              column $ label 48 (pText p) (messageText m)
                : [ styled (\s -> s { wsAlignSelf = Just (if messageMine m then SelfEnd else SelfStart) }) (stamp m) | lastOfRun ]
      (_, Nothing) -> box m $ row $
        [ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.6) }) $ case location (messageText m) of
            -- A place, which a tap opens in the maps app.
            Just (latitude, longitude) -> column
              [ label 16 (pText p) (preview (contactLabel cid contact) m)
              , label 13 (pMuted p) (Text.pack (showFFloat (Just 5) latitude "" <> ", " <> showFFloat (Just 5) longitude ""))
              ]
            Nothing -> label 16 (pText p) (preview (contactLabel cid contact) m)
        ]
        ++ concat [ [gap t 8, stamp m] | lastOfRun ]
        -- Within a run, still that it hasn't gone.
        ++ concat [ [gap t 8, styled (\s -> s { wsAlignSelf = Just SelfEnd }) (deliveryMark Pending)]
                  | not lastOfRun, messageMine m, messageDelivery m == Pending ]
      (_, Just file)
        -- Ours shows from the start, theirs once all of it is here.
        | isPicture (attachmentPath file) || isJust (envDecoded env (attachmentPath file))
        , messageMine m || attachmentState file == Complete ->
            box m $ column $
              [ styled (\s -> s { wsWidth = Just pictureWidth, wsHeight = Just (pictureHeight (attachmentPath file))
                                , wsCornerRadius = Just (dp t 12) }) $
                  Image ImageConfig
                    { icSource = maybe (ImageFile (attachmentPath file)) ImageData (envDecoded env (attachmentPath file))
                    , icScaleType = ScaleFill }
              , gap t 4
              ]
              ++ [ row (middle (label 13 (pMuted p) (fileStatus True file)) : stopping i m file)
                 | attachmentState file /= Complete ]
              ++ [stamp m]
        | otherwise -> box m $ row $
            [ middle $ styled (\s -> s { wsWidth = Just (dp t 36), wsHeight = Just (dp t 36) })
                (icon "ic_insert_drive_file")
            , gap t 8
            , middle $ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.5) }) $ column $
                [ maxLines 2 (label 16 (pText p) (messageText m))
                , label 13 (pMuted p) (fileStatus (messageMine m) file)
                ]
                ++ [ row [pill t (pMine p) (hTapMessage h cid i) "Accept", gap t 8, pill t (pTheirs p) (hCancelFile h cid i) "Decline"]
                   | not (messageMine m), attachmentState file == Waiting ]
            ]
            ++ stopping i m file
            ++ [gap t 8, stamp m]
    -- A cancel for a transfer going on, or one of ours waiting to go.
    stopping i m file =
      [ middle (iconButton t (hCancelFile h cid i) "ic_close")
      | attachmentState file == Moving || (messageMine m && attachmentState file == Waiting) ]
    pictureWidth = thScreenWidth t * 0.6
    -- As tall as the picture is for that width, up to 4:3 upright.
    pictureHeight path = case envPictureSize env path of
      Just (across, down) | across > 0 -> min (pictureWidth * 4 / 3) (pictureWidth * fromIntegral down / fromIntegral across)
      _ -> pictureWidth * 3 / 4
    -- When a message was sent, and how far ours got.
    stamp m = styled (\s -> s { wsAlignSelf = Just SelfEnd }) $ row $
      label 12 (pMuted p) (localTime env (clockFormat env) (messageTime m))
        : concat
            [ [gap t 3, deliveryMark (messageDelivery m)]
            | messageMine m, Nothing <- [messageFile m], Nothing <- [messageCall m] ]
    deliveryMark delivery = styled (\s -> s { wsWidth = Just (dp t 14), wsHeight = Just (dp t 14) })
      (icon (deliveryIcon delivery))
    deliveryIcon delivery = case delivery of
      Pending   -> "ic_schedule"
      Sent      -> "ic_done"
      Delivered -> "ic_done_all"

-- | The streams a call's video is drawn into: the friend's, and ours.
peerStream, selfStream :: Text
peerStream = "peer"
selfStream = "self"

-- | Its last entry, if any.
lastEntry :: Group -> Maybe GroupEntry
lastEntry g = case reverse (groupEntries g) of
  e : _ -> Just e
  []    -> Nothing

-- | What an entry says in a list, with who said it as 'uttered' tells.
entryPreview :: (Member -> Text -> (Text, Text)) -> GroupEntry -> Text
entryPreview utter (GroupEntry _ what) = case what of
  Said who kind body _
    | kind == Tox.MessageAction -> "* " <> by <> " " <> said
    | otherwise -> by <> ": " <> said
    where (by, said) = maybe ("You", body) (`utter` body) who
  Arrived who -> memberLabel who <> " joined"
  Departed who -> memberLabel who <> " left"
  TopicSet who topic -> maybe "You" memberLabel who <> " set the topic: " <> topic
  Noted t -> t
  PrivateFrom who _ said -> memberLabel who <> " (privately): " <> said
  PrivateTo who _ said _ -> "You to " <> memberLabel who <> " (privately): " <> said
  Moderated by whom m ->
    let target = maybe "you" memberLabel whom
    in maybe "You" memberLabel by <> case m of
         Tox.ModKick      -> " removed " <> target
         Tox.ModModerator -> " made " <> target <> " a moderator"
         Tox.ModUser      -> " made " <> target <> " a member who can speak"
         Tox.ModObserver  -> " made " <> target <> " an observer"

-- | Where we are with a group, while not in it.
phaseText :: GroupPhase -> Text
phaseText = \case
  Joining                         -> "Joining\x2026"
  InGroup                         -> ""
  Refused Tox.JoinFailFull        -> "Couldn't join: the group is full"
  Refused Tox.JoinFailBadPassword -> "Couldn't join: the password was wrong. Ask for a new invite."
  Refused Tox.JoinFailUnknown     -> "Couldn't join. Ask for a new invite."
  Gone                            -> "You left this group"
  Kicked                          -> "You were removed from this group"

-- | A role's name, but the common one's.
roleName :: Tox.Role -> Maybe Text
roleName = \case
  Tox.Founder   -> Just "Founder"
  Tox.Moderator -> Just "Moderator"
  Tox.Observer  -> Just "Observer"
  Tox.User      -> Nothing

groupChat :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
groupChat env h model gid@(GroupId chatKey) g = column $
  [ styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ appBar t
      [ iconButton t (hBack h) "ic_arrow_back"
      , grow $ Tappable (hGroupDetails h gid) $ row
          [ middle (avatar env 40 chatKey Nothing)
          , gap t 16
          , grow $ middle $ column
              [ maxLines 1 (label 22 (pText p) (groupLabel g))
              , maxLines 1 (label 13 (pMuted p) subtitle)
              ]
          ]
      ]
  , grow $ stack $
      Scrolled (hScrolled h) (modelJump model)
        (styled (\s -> s { wsPadding = Just (dp t 10), wsKeepAtEnd = Just True })
          (LazyColumn (entries Nothing (groupEntries g))))
      : [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $ column
            [ grow (text "")
            , Tappable (hJump h) $ styled
                (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 24)
                         , wsPadding = Just (dp t 12) }) $
                label 16 (pText p) (if modelUnseen model > 0 then "\x2193 " <> Text.pack (show (modelUnseen model)) else "\x2193")
            ]
        | modelAway model ]
  ]
  ++ [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 12) }) $
         label 14 (if groupPhase g == Joining then pMuted p else pError p) (phaseText (groupPhase g))
     | groupPhase g /= InGroup ]
  -- Whom the draft goes to alone.
  ++ [ styled (\s -> s { wsBackgroundColor = Just (pTheirs p), wsPadding = Just (dp t 8)
                       , wsMargin = Just (dp t 8), wsCornerRadius = Just (dp t 12) }) $ row
         [ middle (icon "ic_reply")
         , gap t 8
         , grow $ middle $ label 14 (pError p) ("Privately to " <> memberLabel m)
         , middle (iconButton t (hEndPrivate h) "ic_close")
         ]
     | groupPhase g == InGroup, Just n <- [modelPrivateTo model], Just m <- [Map.lookup n (groupMembers g)] ]
  ++
  [ row
      [ grow $ middle $ styled
          (\s -> s { wsBackgroundColor = Just (pTheirs p), wsCornerRadius = Just (dp t 28)
                   , wsPadding = Just (dp t 16), wsMargin = Just (dp t 8) })
          (maxLines 3 (input InputMultiline (hDraft h) "Message" (Map.findWithDefault "" gid (modelGroupDrafts model))))
      , middle $ Tappable (hSend h) $ styled
          (\s -> s { wsBackgroundColor = Just (pMine p), wsCornerRadius = Just (dp t 26)
                   , wsPadding = Just (dp t 14), wsMargin = Just (dp t 8) })
          (icon "ic_send")
      ]
  | groupPhase g `elem` [Joining, InGroup] ]
  where
    t = envTheme env
    p = thPalette t
    subtitle = case groupPhase g of
      InGroup -> let n = Map.size (groupMembers g) + 1
                 in memberCount n
      phase -> phaseText phase
    day = localDay . utcToLocalTime (envZone env) . entryTime
    -- A date before each day's first entry.
    entries _ [] = []
    entries before (e : es) =
      [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 8) }) $
          label 14 (pMuted p) (dayHeading env (entryTime e))
      | fmap day before /= Just (day e) ]
      ++ entry (maybe True (\b -> not (together b e)) before) e (maybe True (not . together e) (listToMaybe es))
      : entries (Just e) es
    -- The sender's name above the first of their run.
    entry first this lastOfRun = case entryWhat this of
          Said who kind body delivery ->
            let mine = isNothing who
                (by, said) = maybe ("You", body) (`utter` body) who
            in styled
              (\s -> s { wsBackgroundColor = Just (if mine then pMine p else pTheirs p)
                       , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12)
                       , wsMargin = Just (dp t 4)
                       , wsAlignSelf = Just (if mine then SelfEnd else SelfStart) }) $ column $
              [ labelIn bold 13 (pAccent p) by | first, not mine ]
              ++
              [ row $
                  styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.6) })
                    (label 16 (pText p) (if kind == Tox.MessageAction then "* " <> by <> " " <> said else said))
                  : concat [ [gap t 8, stamp mine delivery (entryTime this)] | lastOfRun ]
              ]
          PrivateFrom who _ said -> privately False ("Privately from " <> memberLabel who) said Delivered
          PrivateTo who _ said delivery -> privately True ("Privately to " <> memberLabel who) said delivery
          _ -> styled (\s -> s { wsAlignSelf = Just SelfCenter, wsPadding = Just (dp t 4) }) $
                 label 13 (pMuted p) (entryPreview utter this)
      where
        -- A message for one, marked as such.
        privately mine heading said delivery = styled
          (\s -> s { wsBackgroundColor = Just (if mine then pMine p else pTheirs p)
                   , wsCornerRadius = Just (dp t 20), wsPadding = Just (dp t 12), wsMargin = Just (dp t 4)
                   , wsAlignSelf = Just (if mine then SelfEnd else SelfStart) }) $ column
          [ labelIn bold 13 (pError p) heading
          , row [ styled (\s -> s { wsMaxWidth = Just (thScreenWidth t * 0.6) }) (label 16 (pText p) said)
                , gap t 8, stamp mine delivery (entryTime this) ]
          ]
    utter = uttered (modelProfile model) g
    -- Text by the same one, close together, which shows the time of its
    -- last message only. A relay bot's are by each name it relays.
    together a b = case (entryWhat a, entryWhat b) of
      (Said x _ tx _, Said y _ ty _) -> fmap memberKey x == fmap memberKey y && fmap (author tx) x == fmap (author ty) y
                                        && day a == day b && diffUTCTime (entryTime b) (entryTime a) < 180
      _ -> False
    author body m = fst (utter m body)
    stamp mine delivery time = styled (\s -> s { wsAlignSelf = Just SelfEnd }) $ row $
      label 12 (pMuted p) (localTime env (clockFormat env) time)
        : concat [ [gap t 3, styled (\s -> s { wsWidth = Just (dp t 14), wsHeight = Just (dp t 14) })
                      (icon (if delivery == Pending then "ic_schedule" else "ic_done"))]
                 | mine ]

-- | A group's page: who is in it, and what can be done with it.
groupDetails :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
groupDetails env h model gid@(GroupId chatKey) g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) (capitalised (kindName (groupKind g))))]
  , grow $ scrollColumn $
      [ gap t 24
      , middle (avatar env 80 chatKey Nothing)
      , gap t 12
      , middle (label 24 (pText p) (groupLabel g))
      ]
      ++ [ middle (padded t 4 (label 14 (pMuted p) (phaseText (groupPhase g)))) | groupPhase g /= InGroup ]
      ++ [ middle (padded t 4 (label 14 (pError p) notice)) | Just notice <- [modelNotice model] ]
      ++
      [ gap t 16
      , Tappable (hMuteGroup h gid) $ padded t 16 $ row
          [ middle (icon (if silent then "ic_notifications_off" else "ic_notifications"))
          , gap t 24
          , grow $ middle $ column $
              label 16 (pText p) (if silent then "Unmute notifications" else "Mute notifications")
                : [label 13 (pMuted p) until' | silent, Just until' <- [mutedUntil env (groupMute g)]]
          ]
      ]
      ++ [ Tappable (hToggleMentions h gid) $ padded t 16 $ row
             [ middle (icon (if groupMentions g then "ic_check_box" else "ic_check_box_outline_blank"))
             , gap t 24
             , grow (middle (label 16 (pText p) "Still notify me of mentions"))
             ]
         | silent ]
      -- Conferences have only a title.
      ++
      [ (if editable then Tappable (hEditTopic h gid) else id) $ padded t 16 $ row $
          grow (middle (column
              [ label 16 (pText p) "Topic"
              , label 13 (pMuted p) (if Text.null (groupTopic g) then "None" else groupTopic g)
              ]))
          : [ middle (icon "ic_edit") | editable ]
      | groupKind g == NgcGroup ]
      ++ [ padded t 16 (label 14 (pMuted p) audioNote) | groupKind g == AudioConference ]
      ++
      [ padded t 16 $ row
          [ grow $ middle $ column
              [ label 16 (pText p) (if groupKind g == NgcGroup then "Chat ID" else "Conference ID")
              , labelIn monospace 13 (pMuted p) chatKey
              ]
          , middle (iconButton t (hCopyChatId h gid) "ic_content_copy")
          ]
      ]
      ++ concat
      [ padded t 16 (label 14 (pAccent p) (memberCount (length members + 1)))
        : person (ownKey model) "You" (groupSelfRole g)
        : [ Tappable (hMember h gid n) (person (memberKey m) (memberLabel m) (Map.lookup n (groupRoles g)))
          | (n, m) <- members ]
      | groupPhase g == InGroup ]
      ++ relaying
      ++
      [ Tappable (hGroupSettings h gid) $ padded t 16 $ row
          [ middle (icon "ic_edit")
          , gap t 24
          , grow (middle (label 16 (pText p) "Group settings"))
          ]
      | groupPhase g == InGroup, groupSelfRole g == Just Tox.Founder, groupKind g == NgcGroup ]
      ++
      [ Tappable (hInviteFriends h gid) $ padded t 16 $ row
          [ middle (icon "ic_add")
          , gap t 24
          , grow (middle (label 16 (pText p) "Invite friends"))
          ]
      | groupPhase g == InGroup ]
      ++
      [ Tappable (hLeaveGroup h gid) $ padded t 16 $ row
          [ middle (icon "ic_exit_to_app")
          , gap t 24
          , grow (middle (label 16 (pError p) ("Leave " <> kindName (groupKind g))))
          ]
      | groupPhase g `elem` [Joining, InGroup] ]
      ++
      [ Tappable (hDeleteGroup h gid) $ padded t 16 $ row
          [ middle (icon "ic_delete")
          , gap t 24
          , grow (middle (label 16 (pError p) "Delete chat"))
          ]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    silent = groupMuted (envNow env) g
    editable = groupPhase g == InGroup && groupMayTopic g
    senders = relayCounts g
    relaying = case groupRelayBot g of
      Just bot ->
        padded t 16 (label 14 (pAccent p) ("Relayed by " <> maybe "the bot" memberLabel (lookup bot [(memberKey m, m) | (m, _, _) <- senders])))
          : [ padded t 16 $ row
                [ grow (middle (label 16 (pText p) name))
                , middle (label 12 (pMuted p) (listTime env time))
                ]
            | (name, time) <- relayedThrough (modelProfile model) g ]
          ++ [ Tappable (hStopRelay h gid) $ padded t 16 $ row
                 [ middle (icon "ic_close")
                 , gap t 24
                 , grow (middle (label 16 (pText p) "Stop showing names from the relay bot"))
                 ] ]
      Nothing ->
        [ Tappable (hRelayBot h gid) $ padded t 16 $ row
            [ middle (icon "ic_group")
            , gap t 24
            , grow $ middle $ column
                [ label 16 (pText p) "Show names from a relay bot"
                , label 13 (pMuted p) "For a bot that carries messages from elsewhere as \"<name> text\""
                ]
            ]
        | any (\(_, relays, _) -> relays > 0) senders ]
    -- By role, then name.
    members = sortOn (\(n, m) -> (Map.findWithDefault Tox.User n (groupRoles g), Text.toCaseFold (memberLabel m)))
      (Map.toList (groupMembers g))
    person key name role = padded t 12 $ row $
      [ middle (avatar env 40 key Nothing)
      , gap t 16
      , grow (middle (label 16 (pText p) name))
      ]
      ++ [ middle (label 13 (pMuted p) r) | Just r <- [roleName =<< role] ]

-- | What the founder sets for everyone in a group.
groupSettingsScreen :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
groupSettingsScreen env h model gid g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Group settings")]
  , grow $ scrollColumn $ case groupSettings g of
      Nothing -> [padded t 16 (label 14 (pMuted p) "Tox doesn't have the group's settings yet")]
      Just current ->
        [ check (hPrivacy h gid) (Tox.groupPrivate current) "Private"
            (if Tox.groupPrivate current then "Friends join when invited" else "Anyone with its chat ID can join")
        , check (hTopicLock h gid) (Tox.groupTopicLock current) "Topic locked" "Only moderators may change the topic"
        , Tappable (hVoice h gid) $ padded t 16 $ column
            [label 16 (pText p) "Who may speak", label 13 (pMuted p) (voiceName (Tox.groupVoice current))]
        , padded t 16 $ column $
            [ label 12 (pAccent p) "Most members"
            , input InputNumber (hPeerLimit h) "" (modelPeerLimit model)
            , label 12 (pAccent p) "Password"
            , input InputPassword (hGroupPassword h) "" (modelGroupPassword model)
            , label 12 (pMuted p) "None if empty"
            ]
            ++ [label 12 (pError p) notice | Just notice <- [modelNotice model]]
        , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
            row [pill t (pMine p) (hApplyGroupSettings h) "Save"]
        ]
  ]
  where
    t = envTheme env
    p = thPalette t
    check action on title about = Tappable action $ padded t 16 $ row
      [ middle (icon (if on then "ic_check_box" else "ic_check_box_outline_blank"))
      , gap t 24
      , grow $ middle $ column [label 16 (pText p) title, label 13 (pMuted p) about]
      ]

-- | Who may speak, as the user calls it.
voiceName :: Tox.Voice -> Text
voiceName = \case
  Tox.VoiceAll        -> "Everyone but observers"
  Tox.VoiceModerators -> "Moderators"
  Tox.VoiceFounder    -> "The founder"

-- | A group or conference to found, which we are then in alone.
founding :: Env -> Handlers -> Model -> Widget
founding env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "New group")]
  , grow $ scrollColumn $
      [ padded t 16 $ column $
          [ label 12 (pAccent p) "Name"
          , input InputText (hGroupName h) "" (modelGroupName model)
          ]
          ++ [ label 12 (pError p) err
             | Just err <- [ if Text.null (modelGroupName model) then modelNotice model
                             else foundingError (modelFounding model) (modelGroupName model) ] ]
      ]
      ++ map choice [minBound .. maxBound]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
          row [pill t (pMine p) (hFound h) "Create"]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    choice kind = Tappable (hFounding h kind) $ padded t 16 $ row
      [ middle (icon (if modelFounding model == kind then "ic_radio_button_checked" else "ic_radio_button_unchecked"))
      , gap t 24
      , grow $ middle $ column [label 16 (pText p) title, label 13 (pMuted p) about]
      ]
      where
        (title, about) = case kind of
          PrivateGroup  -> ("Private group", "Friends join when you invite them")
          PublicGroup   -> ("Public group", "Anyone with its chat ID can join")
          NewConference -> ("Conference", "For friends whose apps have no groups")

-- | A public group to join by its chat ID.
joinGroup :: Env -> Handlers -> Model -> Widget
joinGroup env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Join a public group")]
  , padded t 16 $ column $
      [ label 12 (pAccent p) "Chat ID"
      , input InputText (hChatId h) "" (modelChatId model)
      ]
      ++ [label 12 (pError p) err | Just err <- [chatIdError (modelChatId model)]]
      ++
      [ label 12 (pAccent p) "Password"
      , input InputPassword (hPassword h) "" (modelPassword model)
      , label 12 (pMuted p) "Only if the group has one."
      ]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hJoinPublic h) "Join"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Friends online, whom Tox can invite.
inviteFriends :: Env -> Handlers -> Model -> GroupId -> Group -> Widget
inviteFriends env h model gid g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) ("Invite to " <> groupLabel g))]
  , grow $ scrollColumn $
      [ padded t 16 (label 14 (pText p) notice) | Just notice <- [modelNotice model] ]
      ++ (if null online then [padded t 16 (label 14 (pMuted p) "No friends are online")] else map friend online)
  ]
  where
    t = envTheme env
    p = thPalette t
    online = [ (cid, c) | (cid, c) <- contactOrder model, contactOnline c ]
    friend (cid@(ContactId key), c) = Tappable (hInviteFriend h gid cid) $ padded t 14 $ row
      [ middle (avatar env 48 key (Just True))
      , gap t 16
      , grow (middle (label 16 (pText p) (contactLabel cid c)))
      ]

-- | A group's topic as edited, which Tox may refuse: the founder can lock
-- it to moderators.
topicScreen :: Env -> Handlers -> Model -> Group -> Widget
topicScreen env h model g = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Topic")]
  , padded t 16 $ column
      [ label 12 (pAccent p) (groupName g)
      , input InputMultiline (hTopic h) "" (modelTopic model)
      ]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hApplyTopic h) "Save"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Joining a friend's invite: Tox checks a group's password only after
-- it takes the invite, which works once.
inviteScreen :: Env -> Handlers -> Model -> ContactId -> GroupId -> Invite -> Widget
inviteScreen env h model cid gid@(GroupId chatKey) invite = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Invite")]
  , grow $ scrollColumn $
      [ gap t 24
      , middle (avatar env 80 chatKey Nothing)
      , gap t 12
      , middle (label 24 (pText p) (inviteLabel invite))
      , middle (padded t 4 (label 14 (pMuted p) (friend <> " invited you to " <> article (kindName (inviteKind invite)))))
      ]
      -- Tox checks a group's password once, conferences have none.
      ++ [ padded t 16 $ column
             [ label 12 (pAccent p) "Password"
             , input InputPassword (hPassword h) "" (modelPassword model)
             , label 12 (pMuted p) "Only if the group has one. A wrong one needs a new invite."
             ]
         | inviteKind invite == NgcGroup ]
      ++ [ padded t 16 (label 14 (pMuted p) audioNote) | inviteKind invite == AudioConference ]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $ row
          [ pill t (pTheirs p) (hDecline h cid gid) "Decline"
          , gap t 8
          , pill t (pMine p) (hJoin h cid gid) "Join"
          ]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    friend = maybe "A friend" (contactLabel cid) (Map.lookup cid (modelContacts model))

memberCount :: Int -> Text
memberCount n = Text.pack (show n) <> (if n == 1 then " member" else " members")

-- | What hTox does in an audio conference.
audioNote :: Text
audioNote = "Its audio isn't played: you take part by text."

-- | A noun with "a" or "an".
article :: Text -> Text
article noun = (if Text.take 1 noun `elem` ["a", "e", "i", "o", "u"] then "an " else "a ") <> noun

-- | Until when a mute lasts, with the time too unless it ends today.
mutedUntil :: Env -> Maybe Mute -> Maybe Text
mutedUntil env = \case
  Just (MutedUntil end) -> Just ("Muted until " <> listTime env end <> clock end)
  _                     -> Nothing
  where
    clock end
      | listTime env end == localTime env (clockFormat env) end = ""
      | otherwise = ", " <> localTime env (clockFormat env) end

-- | Over the screens while a call is hidden, which brings it back.
callBanner :: Env -> Handlers -> Call -> Widget
callBanner env h c = Tappable (hShowCall h) $
  styled (\s -> s { wsBackgroundColor = Just (pOnline p), wsPadding = Just (dp t 12) }) $ row
    [ grow (label 16 white "Return to the call")
    , label 16 white $ case callPhase c of
        Talking since -> duration (round (diffUTCTime (envNow env) since))
        _             -> "Calling\x2026"
    ]
  where
    t = envTheme env
    p = thPalette t
    white = Color 0xff 0xff 0xff 0xff

-- | The call, over every screen, with its buttons at the bottom: the
-- friend's video if they send it, else who they are, and ours in a corner.
callScreen :: Env -> Handlers -> Call -> Contact -> Maybe Text -> Widget
callScreen env h c contact notice = styled (\s -> s { wsBackgroundColor = Just (pBar p) }) $ column
  [ grow $ stack $
      ( if peerVideo
          then styled (\s -> s { wsAlignSelf = Just SelfStretch }) (video peerStream ScaleFill)
          else column $
            [ grow (text "")
            , middle (avatar env 120 key Nothing)
            , gap t 24
            , middle (label 28 (pText p) (contactLabel (callContact c) contact))
            , gap t 8
            , middle (label 16 (pMuted p) status)
            ]
            ++ [ middle (styled (\s -> s { wsPadding = Just (dp t 16) }) (label 14 (pError p) n)) | Just n <- [notice] ]
            ++ [ grow (text "") ] )
      -- Who it is and how long it went, over their video.
      : [ styled (\s -> s { wsAlignSelf = Just SelfStretch }) $ column
            [ middle $ styled
                (\s -> s { wsBackgroundColor = Just (pScrim p), wsCornerRadius = Just (dp t 20)
                         , wsPadding = Just (dp t 12), wsMargin = Just (dp t 16) }) $ column
                [ middle (label 20 white (contactLabel (callContact c) contact))
                , middle (label 14 white status)
                ]
            , grow (text "")
            ]
        | peerVideo ]
      ++ [ styled (\s -> s { wsWidth = Just (dp t 108), wsHeight = Just (dp t 144), wsMargin = Just (dp t 16)
                          , wsAlignSelf = Just SelfEnd, wsCornerRadius = Just (dp t 12) })
             (video selfStream ScaleFill)
        | talking && callCamera c ]
      ++ [ styled (\s -> s { wsAlignSelf = Just SelfStart, wsMargin = Just (dp t 8) })
             (iconButton t (hHideCall h) "ic_keyboard_arrow_down")
        | callPhase c /= Ringing ]
  , padded t 32 $ row [grow (column [middle b]) | b <- buttons]
  ]
  where
    talking = case callPhase c of
      Talking _ -> True
      _         -> False
    peerVideo = talking && callPeerVideo c
    white = Color 0xff 0xff 0xff 0xff
    video stream scale = Image ImageConfig { icSource = ImageStream stream, icScaleType = scale }
    t = envTheme env
    p = thPalette t
    ContactId key = callContact c
    status = case callPhase c of
      Calling -> "Calling\x2026"
      Ringing | callPeerVideo c -> "Incoming video call"
              | otherwise -> "Incoming voice call"
      Talking since -> duration (round (diffUTCTime (envNow env) since))
    buttons = case callPhase c of
      Ringing ->
        [ roundButton t (pError p) (hHangUp h) "ic_call_end"
        , roundButton t (pOnline p) (hAnswer h False) "ic_call"
        , roundButton t (pOnline p) (hAnswer h True) "ic_videocam"
        ]
      _ ->
        -- Without an earpiece, as on a tablet, there is nothing to switch.
        [ routeButton | length (callRoutes c) > 1 ]
        ++ [ roundButton t (toggled (callCamera c)) (hCamera h)
               (if callCamera c then "ic_videocam" else "ic_videocam_off") ]
        ++ [ roundButton t (pTheirs p) (hFlipCamera h) "ic_flip_camera" | callCamera c ]
        ++
        [ roundButton t (toggled (callMuted c)) (hMute h)
            (if callMuted c then "ic_mic_off" else "ic_mic")
        , roundButton t (pError p) (hHangUp h) "ic_call_end"
        ]
    toggled on = if on then pMine p else pTheirs p
    routeButton
      | length (callRoutes c) <= 2 = roundButton t (toggled (callRoute c == Speaker)) (hRoute h) "ic_volume_up"
      | otherwise = roundButton t (pTheirs p) (hRoute h) $ case callRoute c of
          Earpiece  -> "ic_call"
          Speaker   -> "ic_volume_up"
          Wired     -> "ic_headset"
          Bluetooth -> "ic_bluetooth"

-- | Whether a file is a picture to show in its chat, by its name.
isPicture :: FilePath -> Bool
isPicture path = map toLower (takeExtension path) `elem` [".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp"]

-- | A file's size, and how far it got.
fileStatus :: Bool -> Attachment -> Text
fileStatus mine file = byteSize (attachmentSize file) <> " \x00B7 " <> case attachmentState file of
  Waiting | mine      -> "Waiting"
          | otherwise -> "Offered"
  Moving -> Text.pack (show (percent (attachmentDone file) (attachmentSize file))) <> "%"
  Complete -> if mine then "Sent" else "Received"
  Interrupted -> "Paused until they're back online"
  Cancelled -> if mine then "Not sent" else "Not received"
  where
    percent done total = if total == 0 then 100 else done * 100 `div` total

-- | A number of bytes, in the largest unit that keeps it above one.
byteSize :: Word64 -> Text
byteSize bytes = case dropWhile ((> fromIntegral bytes) . fst) units of
  (scale, unit) : _ | scale > 1 -> Text.pack (showFFloat (Just 1) (fromIntegral bytes / scale) "") <> " " <> unit
  _ -> Text.pack (show bytes) <> " B"
  where
    units = [(1024 ^ (3 :: Int), "GB"), (1024 ^ (2 :: Int), "MB"), (1024, "KB"), (1 :: Double, "B")]

profile :: Env -> Handlers -> Model -> Widget
profile env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Profile")]
  , grow $ scrollColumn . pure $ padded t 16 $ column $
      [ middle $ Tappable (hPickAvatar h) (avatar env 96 (ownKey model) Nothing)
      , gap t 8
      , middle $ row $
          pill t (pTheirs p) (hPickAvatar h) "Choose picture"
            : [ styled (\s -> s { wsMargin = Just (dp t 4) }) (pill t (pTheirs p) (hRemoveAvatar h) "Remove")
              | isJust (modelAvatar model) ]
      , gap t 16
      , centred (label 16 (pText p) "Nickname")
      , input InputText (hNickname h) "Nickname" (modelNickname model)
      , centred (label 16 (pText p) "Status message")
      , input InputText (hStatus h) "Status message" (modelStatus model)
      ]
      ++ [label 12 (pError p) err | Just err <- [profileError (modelNickname model) (modelStatus model)]]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfCenter, wsMargin = Just (dp t 12) }) $
          pill t (pMine p) (hApplyProfile h) $
            if applied then "Applied" else "Apply changes"
      , row
          [ grow $ middle $ column
              [ label 16 (pText p) "Tox ID"
              , labelIn monospace 14 (pMuted p) (profileToxId current)
              ]
          , middle (iconButton t (hCopyToxId h) "ic_content_copy")
          ]
      , gap t 16
      , middle $ styled (\s -> s { wsWidth = Just (dp t 200), wsHeight = Just (dp t 200) }) $
          Image ImageConfig { icSource = ImageData (envQr env), icScaleType = ScaleFit }
      , gap t 16
      , gap t 16
      , middle (pill t (pTheirs p) (hExport h) "Export profile (.tox)")
      , middle (label 12 (pMuted p) "The file holds your secret key: keep it safe.")
      , gap t 16
      , middle (label 12 (pMuted p) ("hTox " <> version))
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    current = modelProfile model
    applied = Text.strip (modelNickname model) == profileNickname current
      && Text.strip (modelStatus model) == profileStatus current

-- | Every account on the device, to switch to another, add one or take
-- one from another client.
profiles :: Env -> Handlers -> Model -> Widget
profiles env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Profiles")]
  , grow $ scrollColumn $
      [ padded t 16 (label 14 (pError p) notice) | Just notice <- [modelNotice model] ]
      ++ map account (modelProfiles model)
      ++ [ divider t
         , choice (hNewProfile h) "ic_add" "New profile"
         , choice (hImportProfile h) "ic_insert_drive_file" "Take a profile from a .tox file"
         , choice (hOpenLink h) "ic_link" "Link to an account on another device"
         ]
  ]
  where
    t = envTheme env
    p = thPalette t
    account a
      | accountId a == modelProfileId model = padded t 16 $ row
          [ middle (avatar env 48 (ownKey model) Nothing)
          , gap t 16
          , grow $ middle $ column
              [ label 16 (pAccent p) (profileNickname (modelProfile model))
              , labelIn monospace 13 (pMuted p) (Text.take 8 (ownKey model))
              ]
          ]
      | otherwise = Tappable (hSwitchProfile h (accountId a)) $ padded t 16 $ row
          [ middle (avatar env 48 (accountKey a) Nothing)
          , gap t 16
          , grow $ middle $ column
              [ label 16 (pText p) (accountName a)
              , labelIn monospace 13 (pMuted p) (Text.take 8 (accountKey a))
              ]
          , middle (iconButton t (hDeleteProfile h (accountId a)) "ic_delete")
          ]
    choice action name caption = Tappable action $ padded t 16 $ row
      [middle (icon name), gap t 24, grow (middle (label 16 (pText p) caption))]

-- | Linking to an account another device runs, its home, by the pairing
-- link it shows.
linking :: Env -> Handlers -> Model -> Widget
linking env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Link an account")]
  , padded t 16 $ column $
      [ label 14 (pText p) "The other device runs the account on Tox, and this one shows its chats. On it, ask for a pairing link, then paste it here."
      , gap t 16
      , label 12 (pAccent p) "Pairing link"
      , input InputText (hLinkEdited h) "tox:…?pair=…" (modelLink model)
      ]
      ++ [label 12 (pError p) err | Just err <- [modelNotice model]]
  , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsPadding = Just (dp t 16) }) $
      row [pill t (pMine p) (hLink h) "Link"]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | The proxy TCP relays are reached through, as the user edits it.
proxy :: Env -> Handlers -> Model -> Widget
proxy env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Proxy")]
  , grow $ scrollColumn $
      [ kind Nothing "None", kind (Just Tox.Socks5Proxy) "SOCKS5", kind (Just Tox.HttpProxy) "HTTP" ]
      ++ concat
        [ [ padded t 16 $ input InputText (hProxyHost h) "Host" (modelProxyHost model)
          , padded t 16 $ input InputNumber (hProxyPort h) "Port" (modelProxyPort model)
          , padded t 16 $ label 13 (pMuted p) "With a proxy, hTox makes no direct connections and doesn't look for friends on the local network."
          ]
        | isJust (modelProxyKind model) ]
      ++ [ padded t 16 (label 14 (pError p) notice) | Just notice <- [modelNotice model] ]
      ++ [ padded t 16 $ middle (pill t (pMine p) (hApplyProxy h) "Apply") ]
  ]
  where
    t = envTheme env
    p = thPalette t
    kind k name = Tappable (hProxyKind h k) $ padded t 16 $
      label 16 (if modelProxyKind model == k then pAccent p else pText p) name

proxyName :: Tox.Proxy -> Text
proxyName pr = (if Tox.proxyType pr == Tox.HttpProxy then "HTTP " else "SOCKS5 ")
  <> Text.pack (Tox.proxyHost pr) <> ":" <> Text.pack (show (Tox.proxyPort pr))

-- | What the user chose, for the account and for this device.
settings :: Env -> Handlers -> Model -> Widget
settings env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Settings")]
  , grow $ scrollColumn $
      [ heading "Files"
      , choice (hAutoAccept h) "Accept files without asking" $
          case settingAutoAccept (modelAccount model) of
            0    -> "Never"
            size -> "Up to " <> byteSize size
      , heading "Messages"
      ]
      ++ [ choice (hToggleFormat h (Formats.formatKey f)) (Formats.formatName f)
             (onOff (Formats.formatKey f `Set.notMember` settingFormatsOff (modelAccount model)) <> ". " <> Formats.formatAbout f)
         | f <- Formats.formats ]
      ++
      [ heading "On this device"
      , choice (hToggleStay h) "Stay online"
          (if settingStay device then "While hTox is closed, and once the phone starts" else "Only while hTox is open or in a call")
      , choice (hToggleUdp h) "Connect directly (UDP)" (onOff (settingUdp device))
      , choice (hToggleIpv6 h) "Use IPv6" (onOff (settingIpv6 device))
      , choice (hToggleLan h) "Find friends on the local network" (onOff (settingLan device))
      , choice (hOpenProxy h) "Proxy" (maybe "None" proxyName (settingProxy device))
      , padded t 16 $ label 13 (pMuted p) "Network changes apply when hTox starts again."
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    device = modelDevice model
    heading name = padded t 16 $ label 14 (pAccent p) name
    choice action name value = Tappable action $ padded t 16 $ column
      [label 16 (pText p) name, label 14 (pMuted p) value]
    onOff on = if on then "On" else "Off"

-- | Whom to send messages on to: every contact, the latest chat first.
forward :: Env -> Handlers -> Model -> Widget
forward env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Forward to")]
  , grow $ scrollColumn
      [ Tappable (hForwardTo h cid) $ padded t 14 $ row
          [ middle (avatar env 40 key (Just (contactOnline contact)))
          , gap t 16
          , grow (middle (label 16 (pText p) (contactLabel cid contact)))
          ]
      | (cid@(ContactId key), contact) <- contactOrder model ]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | A message, when it went and came, and from whom.
messageInfo :: Env -> Handlers -> ContactId -> Contact -> Message -> Widget
messageInfo env h cid@(ContactId key) contact m = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Message")]
  , grow $ scrollColumn $
      padded t 16 (label 16 (pText p) (preview (contactLabel cid contact) m))
      : concatMap (uncurry fact) facts
  ]
  where
    t = envTheme env
    p = thPalette t
    when' time = localTime env "%b %-d, %Y " time <> localTime env (clockFormat env) time
    fact name value = [padded t 16 $ column [label 16 (pText p) name, label 14 (pMuted p) value]]
    facts
      | messageMine m =
          (if messageDelivery m == Pending then "Sending" else "Sent", when' (messageTime m))
            : [("Delivered", when' d) | Just d <- [messageDelivered m]]
      | otherwise =
          [ ("Received", when' (messageTime m))
          , ("From", contactLabel cid contact <> "\n" <> key) ]

-- | A contact's page: who it is, its key, and what can be done with it.
details :: Env -> Handlers -> ContactId -> Contact -> Widget
details env h cid@(ContactId key) contact = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Contact")]
  , grow $ scrollColumn $
      [ gap t 24
      , middle (avatar env 80 key (Just (contactOnline contact)))
      , gap t 12
      , middle (label 24 (pText p) (contactLabel cid contact))
      ]
      ++ [ middle (padded t 4 (label 14 (pMuted p) (contactStatusMessage contact)))
         | not (Text.null (contactStatusMessage contact)) ]
      ++ [ middle (label 14 (pMuted p) away)
         | contactOnline contact, Just away <- [statusName (contactStatus contact)] ]
      ++
      [ gap t 16
      , Tappable (hMuteChat h cid) $ padded t 16 $ row
          [ middle (icon (if silent then "ic_notifications_off" else "ic_notifications"))
          , gap t 24
          , grow $ middle $ column $
              label 16 (pText p) (if silent then "Unmute notifications" else "Mute notifications")
                : [label 13 (pMuted p) until' | silent, Just until' <- [mutedUntil env (contactMute contact)]]
          ]
      , Tappable (hPictures h cid) $ padded t 16 $ row
          [ middle (icon "ic_image")
          , gap t 24
          , grow $ middle $ column
              [ label 16 (pText p) "Send pictures"
              , label 13 (pMuted p) $ maybe "As they are" (("As " <>) . formatName)
                  (pictureFormat =<< contactPictures contact)
              ]
          ]
      , padded t 16 $ row
          [ grow $ middle $ column
              [ label 16 (pText p) "Public key"
              , labelIn monospace 13 (pMuted p) key
              ]
          , middle (iconButton t (hCopyKey h cid) "ic_content_copy")
          ]
      , Tappable (hDelete h cid) $ padded t 16 $ row
          [ middle (icon "ic_delete")
          , gap t 24
          , grow (middle (label 16 (pError p) "Delete contact"))
          ]
      ]
  ]
  where
    t = envTheme env
    p = thPalette t
    silent = muted (envNow env) contact
    statusName status = case status of
      Tox.Away -> Just "Away"
      Tox.Busy -> Just "Busy"
      _        -> Nothing

addContact :: Env -> Handlers -> Model -> Widget
addContact env h model = column
  [ appBar t [iconButton t (hBack h) "ic_arrow_back", grow (label 22 (pText p) "Add contact")]
  , padded t 16 $ column $
      [ label 12 (pAccent p) "Tox ID"
      , input InputText (hAddress h) "" (modelAddress model)
      ]
      ++ [label 12 (pError p) err | Just err <- [addressError (profileToxId (modelProfile model)) (modelAddress model)]]
      ++
      [ label 12 (pMuted p) "Message"
      , input InputText (hRequest h) "" (modelRequest model)
      ]
      ++ [label 12 (pError p) err | Just err <- [requestError (modelRequest model)]]
      ++ [label 12 (pError p) err | Just err <- [modelAddError model]]
      ++
      [ styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $
          pill t (pMine p) (hAddFriend h) "Add"
      ]
  ]
  where
    t = envTheme env
    p = thPalette t

-- | Why the app couldn't start, with a button to try again.
startFailure :: Theme -> Action -> Text -> Widget
startFailure t retry reason = styled (\s -> s { wsBackgroundColor = Just (pBackground p) }) $
  padded t 24 $ column
    [ label 22 (pText p) "hTox couldn't start"
    , gap t 12
    , label 14 (pText p) reason
    , gap t 12
    , label 14 (pMuted p) "Your chats are kept as they were."
    , styled (\s -> s { wsAlignSelf = Just SelfEnd, wsMargin = Just (dp t 12) }) $
        pill t (pMine p) retry "Try again"
    ]
  where p = thPalette t

-- | Its last message, if any.
lastMessage :: Contact -> Maybe Message
lastMessage contact = case reverse (contactMessages contact) of
  m : _ -> Just m
  []    -> Nothing

-- | Today's time, this week's weekday, or an older date.
-- | The heading of a day's messages.
dayHeading :: Env -> UTCTime -> Text
dayHeading env time
  | days == 0 = "Today"
  | days == 1 = "Yesterday"
  | year time == year (envNow env) = localTime env "%a, %b %-d" time
  | otherwise = localTime env "%b %-d, %Y" time
  where
    days = diffDays (dayOf (envNow env)) (dayOf time)
    dayOf = localDay . utcToLocalTime (envZone env)
    year = localTime env "%Y"

-- | How a time of day is shown.
clockFormat :: Env -> String
clockFormat env = if envClock24 env then "%H:%M" else "%-l:%M %p"

listTime :: Env -> UTCTime -> Text
listTime env time
  | days == 0 = localTime env (clockFormat env) time
  | days < 7 = localTime env "%a" time
  | otherwise = localTime env "%b %-d" time
  where
    days = diffDays (dayOf (envNow env)) (dayOf time)
    dayOf = localDay . utcToLocalTime (envZone env)

localTime :: Env -> String -> UTCTime -> Text
localTime env format =
  Text.pack . formatTime defaultTimeLocale format . utcToLocalTime (envZone env)

ownKey :: Model -> Text
ownKey = Text.take 64 . profileToxId . modelProfile

-- | A key's identicon in a circle of @size@ dp, with a dot while online
-- for a friend.
avatar :: Env -> Double -> Text -> Maybe Bool -> Widget
avatar env size key online = stack $
  styled (\s -> s { wsWidth = Just (dp t size), wsHeight = Just (dp t size)
                  , wsCornerRadius = Just (dp t size / 2) })
    (Image ImageConfig { icSource = ImageData (envAvatar env key), icScaleType = ScaleFill })
  : [ styled (\s -> s { wsWidth = Just (dp t 14), wsHeight = Just (dp t 14)
                    , wsCornerRadius = Just (dp t 7), wsAlignSelf = Just SelfEnd
                    , wsBackgroundColor = Just (pOnline (thPalette t)) })
      (text "")
  | online == Just True ]
  where t = envTheme env

-- | A screen's top row of buttons and title.
appBar :: Theme -> [Widget] -> Widget
appBar t = styled (\s -> s { wsHeight = Just (dp t 64), wsPadding = Just (dp t 4) })
  . row . map middle

-- | An icon in a filled circle.
roundButton :: Theme -> Color -> Action -> Text -> Widget
roundButton t colour action name = Tappable action $ styled
  (\s -> s { wsBackgroundColor = Just colour, wsPadding = Just (dp t 18), wsCornerRadius = Just (dp t 30)
           , wsWidth = Just (dp t 60), wsHeight = Just (dp t 60) })
  (icon name)

iconButton :: Theme -> Action -> Text -> Widget
iconButton t action name = Tappable action $ styled
  (\s -> s { wsPadding = Just (dp t 12), wsCornerRadius = Just (dp t 24) })
  (icon name)

icon :: Text -> Widget
icon name = Image ImageConfig
  { icSource = ImageResource (ResourceName name), icScaleType = ScaleFit }

-- | A rounded, filled button.
pill :: Theme -> Color -> Action -> Text -> Widget
pill t colour action caption = styled
  (\s -> s { wsBackgroundColor = Just colour, wsCornerRadius = Just (dp t 24) })
  (button caption action)

divider :: Theme -> Widget
divider t = styled (\s -> s { wsHeight = Just 1, wsBackgroundColor = Just (pDivider (thPalette t))
                            , wsAlignSelf = Just SelfStretch })
  (text "")

-- | Empty space of @size@ dp.
gap :: Theme -> Double -> Widget
gap t size = styled (\s -> s { wsWidth = Just (dp t size), wsHeight = Just (dp t size) }) (text "")

label :: Double -> Color -> Text -> Widget
label = labelIn id

-- | In a font other than the regular one.
labelIn :: (FontConfig -> FontConfig) -> Double -> Color -> Text -> Widget
labelIn font size colour txt = Text TextConfig
  { tcLabel = txt
  , tcFontConfig = Just (font (fontOfSize size))
  , tcTextColor = Just colour
  }

bold, monospace :: FontConfig -> FontConfig
bold f = f { fontWeight = FontBold }
monospace f = f { fontFamily = FontMonospace }

input :: InputType -> OnChange -> Text -> Text -> Widget
input kind onChange hint value = TextInput TextInputConfig
  { tiInputType = kind
  , tiHint = hint
  , tiValue = value
  , tiOnChange = onChange
  , tiFontConfig = Just (fontOfSize 16)
  , tiTextColor = Nothing
  , tiAutoFocus = False
  }

styled :: (WidgetStyle -> WidgetStyle) -> Widget -> Widget
styled f = Styled (f defaultStyle)

-- | At most @n@ lines of its text, cut with an ellipsis.
maxLines :: Int -> Widget -> Widget
maxLines n = styled (\s -> s { wsMaxLines = Just n })

padded :: Theme -> Double -> Widget -> Widget
padded t size = styled (\s -> s { wsPadding = Just (dp t size) })

-- | Takes the space its row or column has left.
grow :: Widget -> Widget
grow = styled (\s -> s { wsWeight = Just 1 })

-- | Centred across its row or column.
middle :: Widget -> Widget
middle = styled (\s -> s { wsAlignSelf = Just SelfCenter })

centred :: Widget -> Widget
centred = styled (\s -> s { wsTextAlign = Just AlignCenter })
