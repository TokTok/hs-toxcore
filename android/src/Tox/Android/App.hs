{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
-- | The app's state, the handles its widgets fire, and the Tox behind them.
module Tox.Android.App
  ( app
  ) where

import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            threadDelay)
import           Control.Concurrent.Chan   (newChan, readChan, writeList2Chan)
import           Control.Concurrent.MVar   (MVar, modifyMVar, modifyMVar_,
                                            newMVar, readMVar)
import           Control.Exception         (Exception (..), IOException,
                                            SomeException (..), throwIO, try)
import           Control.Monad             (forM_, forever, join, unless, void,
                                            when, (<=<))
import           Crypto.Saltine.Core.Utils (randomByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS
import           Data.Char                 (toLower)
import           Data.Int                  (Int16)
import           Data.IORef                (IORef, atomicModifyIORef',
                                            modifyIORef', newIORef, readIORef,
                                            writeIORef)
import           Data.List                 (find)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (catMaybes, fromMaybe, isJust,
                                            isNothing, listToMaybe)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import           Data.Time                 (NominalDiffTime, UTCTime,
                                            addUTCTime, diffUTCTime,
                                            getCurrentTime, getCurrentTimeZone)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import qualified Data.Vector.Storable      as V
import           Data.Word                 (Word64)
import           Foreign.ForeignPtr        (castForeignPtr)
import           Hatter                    (CallButton (..),
                                            LifecycleEvent (..), MobileApp (..),
                                            MobileContext (..), PickedFile (..),
                                            UserState (..), capturePicture,
                                            copyToClipboard, imageSize,
                                            is24HourClock, isDarkMode,
                                            loggingMobileContext, openFile,
                                            openUri, pickFiles, pickPicture,
                                            platformLog, saveFile)
import           Hatter.Action             (Action, ActionState, createAction,
                                            createOnChange, newActionState,
                                            runActionM)
import           Hatter.BottomSheet        (BottomSheetAction (..),
                                            BottomSheetConfig (..),
                                            showBottomSheet)
import           Hatter.CallAudio          (CallAudio, CallRoute, CallTone (..),
                                            playCallAudio, setCallRoute,
                                            setCallTone, startCallAudio,
                                            stopCallAudio)
import qualified Hatter.CallAudio          as CallAudio
import           Hatter.CallVideo          (CallCamera, VideoFrame (..),
                                            showVideoFrame, startCallCamera,
                                            stopCallCamera)
import           Hatter.DeviceInfo         (DeviceInfo (..), getDeviceInfo)
import           Hatter.Dialog             (DialogAction (..),
                                            DialogConfig (..), showDialog)
import           Hatter.FilesDir           (getAppFilesDir)
import           Hatter.Http               (HttpMethod (..), HttpRequest (..),
                                            HttpResponse (..), performRequest)
import           Hatter.Location           (LocationData (..),
                                            startLocationUpdates,
                                            stopLocationUpdates)
import           Hatter.Notification       (Notification (..),
                                            NotificationKind (..),
                                            allowFullScreen, cancelNotification,
                                            fullScreenAllowed, setOngoingCall,
                                            setRunInBackground,
                                            setServiceStatus,
                                            setShowOverLockScreen,
                                            showNotification, stopBackground)
import           Hatter.Permission         (Permission (..), PermissionState,
                                            PermissionStatus (..),
                                            requestPermission)
import           Hatter.Picture            (PictureKind (..), picturePixels,
                                            shrinkPicture)
import           System.Directory          (createDirectoryIfMissing,
                                            removeDirectory, removeFile,
                                            renameFile)
import           System.FilePath           (dropExtension, takeDirectory,
                                            takeExtension, takeFileName, (</>))
import           System.IO                 (IOMode (..), hFileSize, withFile)
import qualified Tox.Application.Tox       as Tox
import           Tox.AV                    (Picture (..))

import qualified Tox.Android.Avatars       as Avatars
import           Tox.Android.Export        (exportSave)
import qualified Tox.Android.Formats       as Formats
import           Tox.Android.Identicon     (identicon, identiconPng)
import           Tox.Android.Import        (importSave)
import           Tox.Android.Links         (findLinks)
import           Tox.Android.Locations     (location, mapsUri)
import           Tox.Android.Model
import qualified Tox.Android.Network       as Network
import           Tox.Android.Nodes         (Node, builtIn, nodesUrl, readNodes)
import           Tox.Android.Pictures      (PictureFormat (..), pictureFormat,
                                            pictureFormats)
import           Tox.Android.Qoi           (isQoi, qoiBitmap, qoiSize)
import           Tox.Android.QrCode        (toxIdQr)
import qualified Tox.Android.Store         as Store
import           Tox.Android.Store         (Identity (..))
import           Tox.Android.Theme         (dp, theme)
import           Tox.Android.View
import qualified Tox.Sync.Pairing          as Pairing

-- | Show what is new of each contact's unread messages and missed calls,
-- and friend requests, and take away what has been read or answered, and a
-- call while it rings, where the profile has the call.
notify :: Hub -> ProfileId -> Bool -> Model -> Model -> IO ()
notify hub pid owns old new = do
  now <- getCurrentTime
  let missed n = [if n == 1 then "Missed call" else Text.pack (show n) <> " missed calls"]
  forM_ [ (messagesKey, "Messages", "ic_chat_bubble_outline", MessageNotification, notifications now)
        , (missedKey, "Missed calls", "ic_call_missed", MissedCall, fmap missed . missedCalls now) ] $
    \(keyOf, channel, icon, kind, shown) -> do
      let before = shown old
          after = shown new
      forM_ (Map.toList after) $ \(cid, lines') ->
        unless (Map.lookup cid before == Just lines') $
          forM_ (Map.lookup cid (modelContacts new)) $ \contact -> showNotification Notification
            { notificationKey = keyed pid (keyOf cid)
            , notificationChannel = channel
            , notificationIcon = icon
            , notificationTitle = contactLabel cid contact
            , notificationLines = lines'
            , notificationKind = kind
            }
      forM_ (Map.keys (Map.difference before after)) (cancelNotification . keyed pid . keyOf)
  forM_ (Map.toList (modelRequests new)) $ \(cid, message) ->
    unless (Map.lookup cid (modelRequests old) == Just message) $ showNotification Notification
      { notificationKey = keyed pid (requestKey cid)
      , notificationChannel = "Friend requests"
      , notificationIcon = "ic_add"
      , notificationTitle = "Friend request"
      , notificationLines = [message]
      , notificationKind = MessageNotification
      }
  forM_ (Map.keys (Map.difference (modelRequests old) (modelRequests new))) (cancelNotification . keyed pid . requestKey)
  let groupsBefore = groupNotifications now old
      groupsAfter = groupNotifications now new
  forM_ (Map.toList groupsAfter) $ \(gid, lines') ->
    unless (Map.lookup gid groupsBefore == Just lines') $
      forM_ (Map.lookup gid (modelGroups new)) $ \g -> showNotification Notification
        { notificationKey = keyed pid (groupKey gid)
        , notificationChannel = "Messages"
        , notificationIcon = "ic_group"
        , notificationTitle = groupLabel g
        , notificationLines = lines'
        , notificationKind = MessageNotification
        }
  forM_ (Map.keys (Map.difference groupsBefore groupsAfter)) (cancelNotification . keyed pid . groupKey)
  let pending = Map.filter (isNothing . inviteAnswered) . modelInvites
  forM_ (Map.toList (pending new)) $ \(at@(cid, _), invite) ->
    unless (Map.member at (pending old)) $ showNotification Notification
      { notificationKey = keyed pid (inviteKey at)
      , notificationChannel = "Group invites"
      , notificationIcon = "ic_group"
      , notificationTitle = inviteLabel invite
      , notificationLines = [maybe "A friend" (contactLabel cid) (Map.lookup cid (modelContacts new)) <> " invited you"]
      , notificationKind = MessageNotification
      }
  forM_ (Map.keys (Map.difference (pending old) (pending new))) (cancelNotification . keyed pid . inviteKey)
  -- The call going on, which hTox's service shows with its time.
  let ongoing model = case modelCall model of
        Just c | Talking since <- callPhase c ->
          (\contact -> (contactLabel (callContact c) contact, since)) <$> Map.lookup (callContact c) (modelContacts model)
        _ -> Nothing
  when (owns && ongoing old /= ongoing new) $ setOngoingCall (keyed pid callNotification) (ongoing new)
  when (modelConnection old /= modelConnection new) $ connected hub pid (Just (modelConnection new))
  when (settingStay (modelDevice old) /= settingStay (modelDevice new)) $
    setRunInBackground (settingStay (modelDevice new))
  -- Only the call shows over the lock screen, never the chats.
  let overLockScreen = maybe False (not . callHidden) . modelCall
  when (owns && overLockScreen old /= overLockScreen new) $ setShowOverLockScreen (overLockScreen new)
  when owns $ case (ringing old, ringing new) of
    (Nothing, Just c) -> forM_ (Map.lookup (callContact c) (modelContacts new)) $ \contact ->
      showNotification Notification
        { notificationKey = keyed pid callNotification
        , notificationChannel = "Incoming calls"
        , notificationIcon = "ic_call"
        , notificationTitle = contactLabel (callContact c) contact
        , notificationLines = [if callPeerVideo c then "Incoming video call" else "Incoming voice call"]
        , notificationKind = RingingCall
        }
    (Just _, Nothing) -> cancelNotification (keyed pid callNotification)
    _ -> pure ()
  where
    -- From the notification only while hTox isn't in front: then it rings
    -- itself, and nothing covers the call screen.
    ringing model = case modelCall model of
      Just c | callPhase c == Ringing, not (modelShown model) -> Just c
      _                                                       -> Nothing

-- | Tells the service notification of a profile's connection, or of it
-- gone: it shows the best of them.
connected :: Hub -> ProfileId -> Maybe Connection -> IO ()
connected hub pid connection = modifyMVar_ (hubConnections hub) $ \known -> do
  let known' = Map.alter (const connection) pid known
      best = maximum . (Offline :) . Map.elems
  when (best known /= best known') $ setServiceStatus (connectionText (best known'))
  pure known'

-- | What hTox's service notification says of the connection.
connectionText :: Connection -> Text.Text
connectionText = \case
  Offline -> "Connecting\x2026"
  ViaTcp  -> "Online through a relay"
  ViaUdp  -> "Online"

-- | The longest side of pictures sent from the gallery or camera, which
-- lose where they were taken on the way.
pictureSide :: Int
pictureSide = 2048

-- | How long hTox stays online after it is closed, with stay online off.
stayGrace :: NominalDiffTime
stayGrace = 15

-- | The incoming call's notification, which opens the call.
callNotification :: Text.Text
callNotification = "call"

-- | Samples, as the platform's audio has them.
pcmBytes :: V.Vector Int16 -> BS.ByteString
pcmBytes pcm = let (p, n) = V.unsafeToForeignPtr0 pcm in BS.fromForeignPtr0 (castForeignPtr p) (n * 2)

bytesPcm :: BS.ByteString -> V.Vector Int16
bytesPcm bytes = let (p, n) = BS.toForeignPtr0 bytes in V.unsafeFromForeignPtr0 (castForeignPtr p) (n `div` 2)

-- | Our audio goes to Tox in frames of 20 ms.
audioRate, audioFrame :: Int
audioRate = 48000
audioFrame = 960

-- | A picture's planes, one after the other.
pictureFrame :: Picture -> VideoFrame
pictureFrame p = VideoFrame (pictureWidth p) (pictureHeight p) (BS.concat [pictureY p, pictureU p, pictureV p])

framePicture :: VideoFrame -> Picture
framePicture f = Picture w h y u v
  where
    w = frameWidth f
    h = frameHeight f
    (y, chroma) = BS.splitAt (w * h) (frameI420 f)
    (u, v) = BS.splitAt (half w * half h) chroma
    half n = (n + 1) `div` 2

-- | Left and right swapped.
mirrored :: VideoFrame -> VideoFrame
mirrored f = f { frameI420 = BS.concat (rows w y ++ rows half u ++ rows half v) }
  where
    w = frameWidth f
    half = (w + 1) `div` 2
    (y, chroma) = BS.splitAt (w * frameHeight f) (frameI420 f)
    (u, v) = BS.splitAt (BS.length chroma `div` 2) chroma
    rows n plane
      | BS.null plane = []
      | otherwise = let (row, rest) = BS.splitAt n plane in BS.reverse row : rows n rest

-- | What a call's media goes through while it lasts.
data Media = Media
  { mediaAudio  :: IORef (Maybe CallAudio)
  , mediaCamera :: IORef (Maybe CallCamera)
  , mediaMuted  :: IORef Bool
  }

-- | Record and play a call's audio while it is going, telling where it
-- goes, and send what the camera sees while it is on, showing it too.
callMedia :: Network.Network -> Media -> (Route -> [Route] -> IO ()) -> Model -> Model -> IO ()
callMedia network media onRoutes old new = do
  case (talking old, talking new) of
    (Nothing, Just cid) -> do
      let speaker = maybe False ((== Speaker) . callRoute) (modelCall new)
          sending frame = do
            silent <- readIORef (mediaMuted media)
            unless silent $ Network.sendAudio network cid audioRate 1 (bytesPcm frame)
      started <- startCallAudio audioRate audioFrame speaker sending $ \route routes ->
        onRoutes (fromCallRoute route) (map fromCallRoute routes)
      writeIORef (mediaAudio media) (Just started)
    (Just _, Nothing) -> stopping (mediaAudio media) stopCallAudio
    _ -> pure ()
  -- Started again with the other camera when the user flips it.
  when (filming old /= filming new) $ do
    stopping (mediaCamera media) stopCallCamera
    forM_ (filming new) $ \(cid, backCamera) -> do
      started <- startCallCamera (not backCamera) $ \frame -> do
        -- As a mirror shows the user, while the friend sees them as they are.
        showVideoFrame selfStream (if backCamera then frame else mirrored frame)
        Network.sendVideo network cid (framePicture frame)
      writeIORef (mediaCamera media) (Just started)
  writeIORef (mediaMuted media) (maybe False callMuted (modelCall new))
  let tone model = case callPhase <$> modelCall model of
        Just Ringing | modelShown model -> Ringtone
        Just Calling                    -> Ringback
        _                               -> Quiet
  when (tone old /= tone new) $ setCallTone (tone new)
  where
    talking model = case modelCall model of
      Just c | Talking _ <- callPhase c -> Just (callContact c)
      _                                 -> Nothing
    filming model = case modelCall model of
      Just c | callCamera c -> (, callBackCamera c) <$> talking model
      _                     -> Nothing
    stopping ref stop = readIORef ref >>= mapM_ stop >> writeIORef ref Nothing

fromCallRoute :: CallRoute -> Route
fromCallRoute = \case
  CallAudio.Earpiece  -> Earpiece
  CallAudio.Speaker   -> Speaker
  CallAudio.Wired     -> Wired
  CallAudio.Bluetooth -> Bluetooth

toCallRoute :: Route -> CallRoute
toCallRoute = \case
  Earpiece  -> CallAudio.Earpiece
  Speaker   -> CallAudio.Speaker
  Wired     -> CallAudio.Wired
  Bluetooth -> CallAudio.Bluetooth

-- | The keys of a contact's notifications, by what they are of.
messagesKey, missedKey, requestKey :: ContactId -> Text.Text
messagesKey (ContactId key) = key
missedKey (ContactId key) = "missed-" <> key
requestKey (ContactId key) = "request-" <> key

groupKey :: GroupId -> Text.Text
groupKey (GroupId chat) = "group-" <> chat

inviteKey :: (ContactId, GroupId) -> Text.Text
inviteKey (ContactId friend, GroupId chat) = "invite-" <> friend <> "-" <> chat

-- | A profile's notification's key, which says whose it is.
keyed :: ProfileId -> Text.Text -> Text.Text
keyed (ProfileId i) key = Text.pack (show i) <> ":" <> key

-- | Whose a notification is, by its key, and its key in the profile.
-- Those shown before profiles were online together say no profile.
profileOf :: Text.Text -> (Maybe ProfileId, Text.Text)
profileOf key = case Text.breakOn ":" key of
  (i, rest) | not (Text.null rest), [(n, "")] <- reads (Text.unpack i) -> (Just (ProfileId n), Text.drop 1 rest)
  _ -> (Nothing, key)

-- | What a notification's key opens.
notificationOf :: Text.Text -> Maybe Screen
notificationOf key
  | Just k <- Text.stripPrefix "missed-" key = Just (Chat (ContactId k))
  | Just _ <- Text.stripPrefix "request-" key = Just Contacts
  | Just k <- Text.stripPrefix "group-" key = Just (GroupChat (GroupId k))
  | Just k <- Text.stripPrefix "invite-" key, (friend, chat) <- Text.breakOn "-" k =
      Just (InviteScreen (ContactId friend) (GroupId (Text.drop 1 chat)))
  | key == callNotification = Nothing
  | otherwise = Just (Chat (ContactId key))

-- | Who may speak, as offered.
voices :: [(Text.Text, Tox.Voice)]
voices = [("Everyone but observers", Tox.VoiceAll), ("Moderators", Tox.VoiceModerators), ("The founder", Tox.VoiceFounder)]

-- | What the user calls doing it.
memberActionName :: MemberAction -> Text.Text
memberActionName = \case
  MakeModerator -> "Make moderator"
  MakeUser      -> "Make a member who can speak"
  MakeObserver  -> "Make observer"
  Kick          -> "Remove from the group"

-- | What the + button adds, as offered.
adding :: [(Text.Text, Screen)]
adding = [("Contact", AddContact), ("Group", NewGroupScreen), ("Public group by its chat ID", JoinGroupScreen)]

-- | The sizes offered for taking files without asking, with what hTox
-- took before.
autoAccepts :: [(Text.Text, Word64)]
autoAccepts =
  ("Never", 0) : [(Text.pack (show n) <> " MB", n * 1024 * 1024) | n <- [1, 4, 16, 32, 64, 256]]

-- | What the user calls where a call's audio goes.
routeName :: Route -> Text.Text
routeName = \case
  Earpiece  -> "Phone"
  Speaker   -> "Speaker"
  Wired     -> "Headset"
  Bluetooth -> "Bluetooth"

-- | How long a mute can last, as offered.
muteChoices :: [(Text.Text, Maybe NominalDiffTime)]
muteChoices =
  [("For 1 hour", Just 3600), ("For 8 hours", Just 28800), ("For 1 week", Just 604800), ("Always", Nothing)]

toMillis :: UTCTime -> Integer
toMillis = round . (* 1000) . utcTimeToPOSIXSeconds

-- | What can be done to one contact.
data ContactActions = ContactActions
  { caOpen      :: Action
  , caAccept    :: Action
  , caReject    :: Action
  , caDetails   :: Action
  , caMute      :: Action
  , caDelete    :: Action
  , caCopyKey   :: Action
  , caForwardTo :: Action
  , caCallBack  :: Action
  , caPictures  :: Action
  }

-- | What can be done to one group.
data GroupActions = GroupActions
  { gaOpen      :: Action
  , gaDetails   :: Action
  , gaMute      :: Action
  , gaLeave     :: Action
  , gaDelete    :: Action
  , gaCopyId    :: Action
  , gaTopic     :: Action
  , gaMentions  :: Action
  , gaInvite    :: Action
  , gaSettings  :: Action
  , gaPrivacy   :: Action
  , gaTopicLock :: Action
  , gaVoice     :: Action
  , gaRelayBot  :: Action
    -- ^ Offers whose messages to read as relayed.
  , gaStopRelay :: Action
  }

-- | And to one invite.
data InviteActions = InviteActions
  { iaOpen    :: Action
  , iaJoin    :: Action
  , iaDecline :: Action
  }

-- | Tox didn't start.
newtype ToxFailed = ToxFailed Tox.NewError
  deriving (Show)

instance Exception ToxFailed where
  displayException (ToxFailed err) = "Tox didn't start: " <> show err

-- | Every profile online, each on a Tox of its own, and which one is shown.
data Hub = Hub
  { hubDatabase    :: Store.Database
  , hubDir         :: FilePath
  , hubSessions    :: MVar (Map.Map ProfileId Session)
  , hubShown       :: IORef ProfileId
  , hubRedraw      :: IORef (IO ())
  , hubNodes       :: IORef [Node]
  , hubFetched     :: IORef Bool
    -- ^ Whether fresh nodes were asked for.
  , hubMedia       :: Media
  , hubCall        :: IORef (Maybe ProfileId)
    -- ^ The profile with the call: there is one at most, as the
    -- microphone and speaker can't take more.
  , hubConnections :: MVar (Map.Map ProfileId Connection)
  , hubWatch       :: ThreadId
  }

-- | A profile online: its model, saved as it changes, and its Tox.
data Session = Session
  { sessionModel   :: MVar Model
  , sessionNetwork :: Network.Network
  , sessionFire    :: Event -> IO ()
  , sessionStop    :: IO ()
  }

-- | Every profile online, the one shown first, and those that can't
-- start left offline.
newHub :: IORef (IO ()) -> IO Hub
newHub redraw = do
  dir <- getAppFilesDir
  database <- Store.open (dir </> "tox.db")
  mapM_ (createDirectoryIfMissing True) [dir </> "received", dir </> "sent"]
  shown <- Store.activeProfile database
  -- Those fetched last, kept for now.
  cached <- either (const Nothing) readNodes <$> try @IOException (BS.readFile (dir </> "nodes.json"))
  sessions <- newMVar Map.empty
  media <- Media <$> newIORef Nothing <*> newIORef Nothing <*> newIORef False
  -- With stay online off, hTox stops a while after it is closed, unless a
  -- call goes on.
  watchThread <- forkIO $ do
    let watch since = do
          threadDelay 5000000
          models <- mapM (readMVar . sessionModel) . Map.elems =<< readMVar sessions
          now <- getCurrentTime
          if any (\m -> settingStay (modelDevice m) || modelShown m || isJust (modelCall m)) models
            then watch now
            else if diffUTCTime now since >= stayGrace then stopBackground else watch since
    getCurrentTime >>= watch
  hub <- Hub database dir sessions
    <$> newIORef shown <*> pure redraw <*> newIORef (fromMaybe builtIn cached) <*> newIORef False
    <*> pure media <*> newIORef Nothing <*> newMVar Map.empty <*> pure watchThread
  setServiceStatus (connectionText Offline)
  void (sessionFor hub shown)
  others <- filter (/= shown) . map accountId <$> Store.profiles database
  forM_ others $ \pid -> try @SomeException (sessionFor hub pid) >>= \case
    Left err -> platformLog ("A profile didn't go online: " <> Text.pack (displayException err))
    Right _ -> pure ()
  setRunInBackground . settingStay . modelDevice =<< readMVar . sessionModel =<< sessionFor hub shown
  pure hub

-- | Every profile offline.
stopHub :: Hub -> IO ()
stopHub hub = do
  killThread (hubWatch hub)
  modifyMVar_ (hubSessions hub) $ \sessions -> Map.empty <$ mapM_ sessionStop sessions

-- | The profile online, gone online now if it wasn't.
sessionFor :: Hub -> ProfileId -> IO Session
sessionFor hub pid = modifyMVar (hubSessions hub) $ \sessions -> case Map.lookup pid sessions of
  Just session -> pure (sessions, session)
  Nothing -> (\session -> (Map.insert pid session sessions, session)) <$> online hub pid

-- | A profile gone offline, with its notifications.
goOffline :: Hub -> ProfileId -> IO ()
goOffline hub pid = modifyMVar_ (hubSessions hub) $ \sessions -> do
  mapM_ sessionStop (Map.lookup pid sessions)
  pure (Map.delete pid sessions)

-- | Draws again where the profile is shown.
redrawIfShown :: Hub -> ProfileId -> IO ()
redrawIfShown hub pid = readIORef (hubShown hub) >>= \shown -> when (shown == pid) (join (readIORef (hubRedraw hub)))

-- | A profile online on its own Tox.
online :: Hub -> ProfileId -> IO Session
online hub pid = do
  let store = Store.profileStore (hubDatabase hub) pid
      redraw = redrawIfShown hub pid
  -- Saved as it changes, one event at a time.
  model <- newMVar =<< Store.load store
  identity <- Store.identity store
  settings <- modelDevice <$> readMVar model
  -- A call ringing when hTox last stopped rings no more.
  cancelNotification (keyed pid callNotification)
  nodes <- readIORef (hubNodes hub)
  network <- either (throwIO . ToxFailed) pure =<< Network.start identity settings (hubDir hub </> "received") nodes
  unless (isJust identity) $ Network.identityOf network >>= Store.saveIdentity store
  Network.onMedia network Network.Media
    { Network.playAudio = \rate channels pcm -> playCallAudio rate channels (pcmBytes pcm)
    , Network.showVideo = showVideoFrame peerStream . pictureFrame
    }
  -- What Tox is asked, in order, from a thread of its own: Tox may take a
  -- while to answer, and a tap shouldn't wait for it.
  asked <- newChan
  let fire event = do
        (commands, busy, device) <- modifyMVar model $ \old -> do
          let (new, commands) = update event old
          Store.save store old new
          owns <- claimCall hub pid old new
          notify hub pid owns old new
          -- The platform tells of routes also while the model is held.
          when owns $ callMedia network (hubMedia hub)
            (\route routes -> void . forkIO $ fire (RoutesChanged route routes) >> redraw)
            old new
          let busy = not owns && isNothing (modelCall old) && isJust (modelCall new)
          pure (new, (commands, busy, if modelDevice old /= modelDevice new then Just (modelDevice new) else Nothing))
        writeList2Chan asked commands
        -- Another profile has the call.
        when busy $ getCurrentTime >>= fire . CallElsewhere
        -- The settings are the device's, so every profile has them.
        forM_ device $ \d -> do
          others <- Map.elems . Map.delete pid <$> readMVar (hubSessions hub)
          forM_ others (`sessionFire` SetDevice d)
  askingThread <- Network.looping "Asking Tox" $
    readChan asked >>= Network.perform network >>= mapM_ fire >> redraw
  networkThreads <- Network.run network (\event -> fire event >> redraw)
  -- A call's time, counted on screen and until it is given up.
  tickThread <- forkIO . forever $ do
    threadDelay 1000000
    calling <- isJust . modelCall <$> readMVar model
    when calling $ getCurrentTime >>= fire . Tick >> redraw
  let stop = modifyMVar_ model $ \current -> do
        mapM_ killThread (askingThread : tickThread : networkThreads)
        Network.stop network
        -- Its notifications go with it.
        owns <- claimCall hub pid current initial
        notify hub pid owns current initial { modelDevice = modelDevice current, modelProfile = modelProfile current }
        connected hub pid Nothing
        pure current
  pure (Session model network fire stop)

-- | Whether the profile has the call, which it takes when it has one and
-- no other profile has one, and gives up when it has none.
claimCall :: Hub -> ProfileId -> Model -> Model -> IO Bool
claimCall hub pid old new = atomicModifyIORef' (hubCall hub) $ \owner -> case (modelCall old, modelCall new) of
  (_, Just _) | maybe True (== pid) owner -> (Just pid, True)
  (Just _, Nothing) | owner == Just pid   -> (Nothing, True)
  _                                       -> (owner, False)

-- | The app once it started, or a screen saying why it didn't, with a
-- button to try again.
app :: IO MobileApp
app = do
  actions <- newActionState
  -- With how it stops, to show another profile.
  started <- newIORef Nothing
  failure <- newIORef Text.empty
  redraw <- newIORef (pure ())
  hub <- newIORef Nothing
  let start = try (hubbed >>= \h -> running actions h switch restart) >>= \case
        Right a -> writeIORef started (Just a)
        Left err@(SomeException inner) -> do
          platformLog ("hTox didn't start: " <> Text.pack (displayException err))
          -- Without the backtrace, which the log has.
          writeIORef failure (Text.pack (displayException inner))
      hubbed = readIORef hub >>= \case
        Just h -> pure h
        Nothing -> newHub redraw >>= \h -> h <$ writeIORef hub (Just h)
      -- The profile now chosen shown, every profile staying online.
      switch = do
        readIORef started >>= mapM_ snd
        writeIORef started Nothing
        start
        inApp (`onLifecycle` Start)
        join (readIORef redraw)
      -- Every profile online again, as Tox takes some settings only when
      -- it starts.
      restart = do
        readIORef hub >>= mapM_ stopHub
        writeIORef hub Nothing
        switch
      inApp f = readIORef started >>= mapM_ (f . maContext . fst)
  start
  -- Pressed while the app is in front of the user, which it then tells the
  -- app that started.
  retry <- runActionM actions (createAction (start >> inApp (`onLifecycle` Start)))
  device <- getDeviceInfo
  pure MobileApp
    { maContext = loggingMobileContext
        { onLifecycle = \event -> inApp (`onLifecycle` event)
        , onBack = readIORef started >>= maybe (pure False) (onBack . maContext . fst)
        , onNotificationOpened = \key -> inApp (`onNotificationOpened` key)
        , onCallButton = \key button -> inApp (\c -> onCallButton c key button)
        , onFilePicked = \picked -> inApp (`onFilePicked` picked)
        , onPicturePicked = \png -> inApp (`onPicturePicked` png)
        }
    , maActionState = actions
    , maView = \state -> writeIORef redraw (userRequestRedraw state) >> readIORef started >>= \case
        Just (a, _) -> maView a state
        Nothing -> do
          dark <- isDarkMode
          startFailure (theme (diScreenDensity device) (fromIntegral (diScreenWidth device)) dark) retry
            <$> readIORef failure
    }

-- | The app showing a profile, and how it stops showing it. It shows the
-- profile then chosen with the first function given, and starts every
-- profile again with the second.
running :: ActionState -> Hub -> IO () -> IO () -> IO (MobileApp, IO ())
running actions hub switch restart = do
  let database = hubDatabase hub
      dir = hubDir hub
      sent = dir </> "sent"
      nodesFile = dir </> "nodes.json"
      redraw = hubRedraw hub
  shownId <- Store.activeProfile database
  session <- sessionFor hub shownId
  writeIORef (hubShown hub) shownId
  let model = sessionModel session
      network = sessionNetwork session
      fire = sessionFire session
  -- Profiles made or deleted while it wasn't shown.
  Store.profiles database >>= fire . ProfilesChanged
  settings <- modelDevice <$> readMVar model
  -- Asked for when a call starts, which needs a screen.
  permissions <- newIORef (Nothing :: Maybe PermissionState)
  -- The platform's dialogs, which need a screen.
  user <- newIORef (Nothing :: Maybe UserState)
  -- Whether a location fix is awaited.
  locating <- newIORef False
  -- What is picked next is: a picture from the gallery or camera, which
  -- may go in the chat's way for pictures, or a profile to take.
  picturing <- newIORef False
  importing <- newIORef False
  let stop = fire (Shown False)
      -- Another profile shown, unless a call goes on, which would hide.
      switching next = readMVar model >>= \current -> case modelCall current of
        Just _  -> fire (Notify "Not during a call") >> join (readIORef redraw)
        Nothing -> next >>= \pid -> if pid == shownId then join (readIORef redraw) else Store.setActive database pid >> switch
  let withUser f = readIORef user >>= mapM_ f
      redraw' = join (readIORef redraw)
      shareLocation = withUser $ \u -> requestPermission (userPermissionState u) PermissionLocation $ \case
        PermissionDenied -> fire (Notify "hTox may not use your location") >> redraw'
        PermissionGranted -> do
          fire (Notify "Finding where you are\x2026") >> redraw'
          writeIORef locating True
          startLocationUpdates (userLocationState u) $ \fix -> do
            waiting <- atomicModifyIORef' locating (False,)
            when waiting $ do
              stopLocationUpdates (userLocationState u)
              fire (ShareLocation (ldLatitude fix) (ldLongitude fix)) >> redraw'
          void . forkIO $ do
            threadDelay 30000000
            waiting <- atomicModifyIORef' locating (False,)
            when waiting $ do
              stopLocationUpdates (userLocationState u)
              fire (Notify "Couldn't find where you are") >> redraw'
      takePicture = withUser $ \u -> requestPermission (userPermissionState u) PermissionCamera $ \case
        PermissionDenied -> fire (Notify "hTox may not use the camera") >> redraw'
        PermissionGranted -> capturePicture
      attaching = withUser $ \u ->
        showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Attach" (map fst choices)) $ \case
          BottomSheetItemSelected n | (_, chosen) : _ <- drop (fromIntegral n) choices -> chosen
          _ -> pure ()
        where
          choices =
            [ ("Gallery", writeIORef picturing True >> pickFiles "image/*")
            , ("Camera", writeIORef picturing True >> takePicture)
            , ("File", writeIORef picturing False >> pickFiles "*/*"), ("Location", shareLocation) ]
  -- No call without the microphone, and without the camera one without
  -- video. Hatter draws nothing again after an answer, so this does.
  let allowing camera next = readIORef permissions >>= \case
        Nothing -> next camera
        Just state -> requestPermission state PermissionMicrophone $ \case
          PermissionDenied -> fire (Notify "Calls need the microphone") >> join (readIORef redraw)
          PermissionGranted
            | camera -> requestPermission state PermissionCamera $ \case
                PermissionGranted -> next True >> join (readIORef redraw)
                PermissionDenied -> do
                  fire (Notify "hTox may not use the camera, so the call goes on without it")
                  next False >> join (readIORef redraw)
            | otherwise -> next False >> join (readIORef redraw)
  handlers <- runActionM actions $ do
    back' <- createAction (fire Back)
    send <- createAction (getCurrentTime >>= fire . Send)
    openDrawer <- createAction (fire OpenDrawer)
    openProfile <- createAction (fire (Open ProfileScreen))
    openAddContact <- createAction $ withUser $ \u ->
      showBottomSheet (userBottomSheetState u) (BottomSheetConfig "New" (map fst adding)) $ \case
        BottomSheetItemSelected n | (_, screen') : _ <- drop (fromIntegral n) adding -> fire (Open screen') >> redraw'
        _ -> pure ()
    openChats <- createAction (fire (Open Contacts))
    openCalls <- createAction (fire (Open CallsScreen))
    allowFullScreen' <- createAction allowFullScreen
    applyProfile <- createAction (fire ApplyProfile)
    addFriend <- createAction (fire AddFriend)
    copyToxId <- createAction $
      readMVar model >>= copyToClipboard . profileToxId . modelProfile
    attach <- createAction attaching
    openSettings <- createAction (fire (Open SettingsScreen))
    openProfiles <- createAction (fire (Open ProfilesScreen))
    newProfile <- createAction (switching (Store.newProfile database Nothing (modelProfile initial)))
    importProfile <- createAction (writeIORef importing True >> pickFiles "*/*")
    -- Sides tried in turn, for a PNG small enough for Tox clients.
    pickAvatar <- createAction (pickPicture [256, 192, 128, 96, 64] Avatars.maxSize)
    removeAvatar <- createAction (fire RemoveAvatar)
    -- Written where the app keeps its files, then copied where the user
    -- chooses.
    exportProfile <- createAction $ do
      current <- readMVar model
      me <- Network.identityOf network
      forM_ (exportSave me current) $ \save -> do
        let path = dir </> "export.tox"
        BS.writeFile path save
        saveFile path (profileNickname (modelProfile current) <> ".tox") "application/octet-stream"
    chooseAutoAccept <- createAction $ withUser $ \u ->
      showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Accept files without asking" (map fst autoAccepts)) $ \case
        BottomSheetItemSelected n | (_, size) : _ <- drop (fromIntegral n) autoAccepts -> fire (SetAutoAccept size) >> redraw'
        _ -> pure ()
    let toggling f = createAction (readMVar model >>= fire . SetDevice . f . modelDevice)
    toggleUdp <- toggling (\d -> d { settingUdp = not (settingUdp d) })
    toggleIpv6 <- toggling (\d -> d { settingIpv6 = not (settingIpv6 d) })
    toggleLan <- toggling (\d -> d { settingLan = not (settingLan d) })
    toggleStay <- toggling (\d -> d { settingStay = not (settingStay d) })
    toggleFormats <- Map.fromList <$> traverse
      (\f -> (,) (Formats.formatKey f) <$> createAction (fire (ToggleFormat (Formats.formatKey f)))) Formats.formats
    openProxy <- createAction (fire (Open ProxyScreen))
    proxyKinds <- (,,)
      <$> createAction (fire (ProxyKindChosen Nothing))
      <*> createAction (fire (ProxyKindChosen (Just Tox.Socks5Proxy)))
      <*> createAction (fire (ProxyKindChosen (Just Tox.HttpProxy)))
    proxyHost <- createOnChange (fire . ProxyHostChanged)
    proxyPort <- createOnChange (fire . ProxyPortChanged)
    -- Tox takes a proxy only when it starts, so it starts again, unless
    -- that would cut a call.
    applyProxy <- createAction $ do
      before <- settingProxy . modelDevice <$> readMVar model
      fire ApplyProxy
      current <- readMVar model
      if settingProxy (modelDevice current) == before || isJust (modelCall current)
        then redraw'
        else restart
    draft <- createOnChange (fire . DraftChanged)
    nickname <- createOnChange (fire . NicknameChanged)
    status <- createOnChange (fire . StatusChanged)
    address <- createOnChange (fire . AddressChanged)
    request <- createOnChange (fire . RequestChanged)
    let twice f = (,) <$> createAction (f False) <*> createAction (f True)
    calls <- twice $ \camera -> allowing camera (\allowed -> getCurrentTime >>= \now -> fire (StartCall now allowed))
    answers <- twice $ \camera -> allowing camera (\allowed -> getCurrentTime >>= \now -> fire (AnswerCall now allowed))
    hangUp <- createAction (getCurrentTime >>= fire . HangUp)
    mute <- createAction (fire ToggleMute)
    camera' <- createAction (fire ToggleCamera)
    -- The platform tells where the audio then goes.
    route <- createAction $ readMVar model >>= mapM_ (\c -> case callRoutes c of
      routes | length routes <= 2 -> setCallRoute (toCallRoute (if callRoute c == Speaker then Earpiece else Speaker))
             | otherwise -> withUser $ \u ->
                 showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Audio" (map routeName routes)) $ \case
                   BottomSheetItemSelected n | r : _ <- drop (fromIntegral n) routes -> setCallRoute (toCallRoute r)
                   _ -> pure ()) . modelCall
    flipCamera <- createAction (fire FlipCamera)
    hideCall <- createAction (fire HideCall)
    showCall <- createAction (fire ShowCall)
    scrolled <- createOnChange (fire . ScrolledAway . (== "away"))
    jump <- createAction (fire JumpToEnd)
    unselect <- createAction (fire Unselect)
    -- Of the selected messages of the open chat.
    let selected f = readMVar model >>= \current -> case modelScreen current of
          Chat cid -> f current cid (Set.toList (modelSelected current))
          _        -> pure ()
    copySelected <- createAction $ selected $ \current cid is ->
      copyToClipboard (copiedText current cid is) >> fire Unselect
    forwardSelected <- createAction $ selected $ \_ cid is -> fire (Open (Forward cid is))
    cancelReply <- createAction (fire CancelReply)
    password <- createOnChange (fire . PasswordChanged)
    topic <- createOnChange (fire . TopicEdited)
    applyTopic <- createAction (fire ApplyTopic)
    foundings <- Map.fromList <$> traverse (\f -> (,) f <$> createAction (fire (FoundingChosen f))) [minBound .. maxBound]
    groupName' <- createOnChange (fire . GroupNameEdited)
    found <- createAction (fire Found)
    chatId <- createOnChange (fire . ChatIdEdited)
    joinPublic <- createAction (fire JoinPublic)
    endPrivate <- createAction (fire (PrivatelyTo Nothing))
    peerLimit <- createOnChange (fire . PeerLimitEdited)
    groupPassword <- createOnChange (fire . GroupPasswordEdited)
    applyGroupSettings <- createAction (fire ApplyGroupSettings)
    openLink <- createAction (fire (Open LinkScreen))
    linkEdited <- createOnChange (fire . LinkEdited)
    -- The link's account as a new profile, online at once.
    link <- createAction $ readMVar model >>= \current ->
      case (linkError (modelLink current), Pairing.parseLink (modelLink current)) of
        (Nothing, Just pairing) -> do
          now <- getCurrentTime
          switching (Store.newLinkedProfile database (Pairing.linkAddress pairing) (Pairing.requestMessage pairing) now)
        (why, _) -> fire (Notify (fromMaybe "That isn't a pairing link" why)) >> redraw'
    pure $ \perContact perMessage perProfile perGroup perInvite perInviting perMember perStaged -> Handlers
      { hBack = back'
      , hSend = send
      , hOpenDrawer = openDrawer
      , hOpenProfile = openProfile
      , hOpenAddContact = openAddContact
      , hOpenChats = openChats
      , hOpenCalls = openCalls
      , hCallBack = caCallBack . perContact
      , hAllowFullScreen = allowFullScreen'
      , hApplyProfile = applyProfile
      , hAddFriend = addFriend
      , hCopyToxId = copyToxId
      , hAttach = attach
      , hTapMessage = \cid i -> let (tap, _, _) = perMessage Map.! (cid, i) in tap
      , hHoldMessage = \cid i -> let (_, hold, _) = perMessage Map.! (cid, i) in hold
      , hCancelFile = \cid i -> let (_, _, cancel) = perMessage Map.! (cid, i) in cancel
      , hUnselect = unselect
      , hCopySelected = copySelected
      , hForwardSelected = forwardSelected
      , hCancelReply = cancelReply
      , hUnstage = curry perStaged
      , hOpenSettings = openSettings
      , hOpenProfiles = openProfiles
      , hSwitchProfile = fst . perProfile
      , hDeleteProfile = snd . perProfile
      , hNewProfile = newProfile
      , hImportProfile = importProfile
      , hPickAvatar = pickAvatar
      , hRemoveAvatar = removeAvatar
      , hExport = exportProfile
      , hAutoAccept = chooseAutoAccept
      , hToggleUdp = toggleUdp
      , hToggleIpv6 = toggleIpv6
      , hToggleLan = toggleLan
      , hToggleStay = toggleStay
      , hToggleFormat = (toggleFormats Map.!)
      , hOpenProxy = openProxy
      , hProxyKind = \case
          Nothing              -> let (none, _, _) = proxyKinds in none
          Just Tox.Socks5Proxy -> let (_, socks, _) = proxyKinds in socks
          Just Tox.HttpProxy   -> let (_, _, http) = proxyKinds in http
      , hProxyHost = proxyHost
      , hProxyPort = proxyPort
      , hApplyProxy = applyProxy
      , hForwardTo = caForwardTo . perContact
      , hOpenChat = caOpen . perContact
      , hAccept = caAccept . perContact
      , hReject = caReject . perContact
      , hDetails = caDetails . perContact
      , hMuteChat = caMute . perContact
      , hDelete = caDelete . perContact
      , hCopyKey = caCopyKey . perContact
      , hPictures = caPictures . perContact
      , hDraft = draft
      , hNickname = nickname
      , hStatus = status
      , hAddress = address
      , hRequest = request
      , hCall = \camera -> (if camera then snd else fst) calls
      , hAnswer = \camera -> (if camera then snd else fst) answers
      , hHangUp = hangUp
      , hMute = mute
      , hCamera = camera'
      , hRoute = route
      , hFlipCamera = flipCamera
      , hHideCall = hideCall
      , hShowCall = showCall
      , hScrolled = scrolled
      , hJump = jump
      , hOpenGroup = gaOpen . perGroup
      , hGroupDetails = gaDetails . perGroup
      , hMuteGroup = gaMute . perGroup
      , hLeaveGroup = gaLeave . perGroup
      , hDeleteGroup = gaDelete . perGroup
      , hCopyChatId = gaCopyId . perGroup
      , hOpenInvite = \cid gid -> iaOpen (perInvite (cid, gid))
      , hJoin = \cid gid -> iaJoin (perInvite (cid, gid))
      , hDecline = \cid gid -> iaDecline (perInvite (cid, gid))
      , hPassword = password
      , hEditTopic = gaTopic . perGroup
      , hToggleMentions = gaMentions . perGroup
      , hTopic = topic
      , hApplyTopic = applyTopic
      , hFounding = (foundings Map.!)
      , hGroupName = groupName'
      , hFound = found
      , hChatId = chatId
      , hJoinPublic = joinPublic
      , hInviteFriends = gaInvite . perGroup
      , hInviteFriend = curry perInviting
      , hMember = curry perMember
      , hEndPrivate = endPrivate
      , hGroupSettings = gaSettings . perGroup
      , hPrivacy = gaPrivacy . perGroup
      , hTopicLock = gaTopicLock . perGroup
      , hVoice = gaVoice . perGroup
      , hRelayBot = gaRelayBot . perGroup
      , hStopRelay = gaStopRelay . perGroup
      , hPeerLimit = peerLimit
      , hGroupPassword = groupPassword
      , hApplyGroupSettings = applyGroupSettings
      , hOpenLink = openLink
      , hLinkEdited = linkEdited
      , hLink = link
      }
  let redrawn = join (readIORef redraw)
      muting cid = do
        now <- getCurrentTime
        contact <- Map.lookup cid . modelContacts <$> readMVar model
        case contact of
          Just c | muted now c -> fire (UnmuteChat cid) >> redrawn
          Just _ -> readIORef user >>= mapM_ (\u ->
            showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Mute notifications" (map fst muteChoices)) $ \case
              BottomSheetItemSelected n | (_, for) : _ <- drop (fromIntegral n) muteChoices -> do
                from <- getCurrentTime
                fire (MuteChat cid ((`addUTCTime` from) <$> for)) >> redrawn
              _ -> pure ())
          Nothing -> pure ()
      deleting cid = do
        contact <- Map.lookup cid . modelContacts <$> readMVar model
        forM_ contact $ \c -> readIORef user >>= mapM_ (\u ->
          showDialog (userDialogState u) DialogConfig
            { dcTitle = "Delete " <> contactLabel cid c <> "?"
            , dcMessage = "Their chat can stay, and comes back if you add them again."
            , dcButton1 = "Keep chat"
            , dcButton2 = Just "Delete chat"
            , dcButton3 = Just "Cancel"
            } $ \case
              DialogButton1 -> fire (DeleteContact cid True) >> redrawn
              DialogButton2 -> fire (DeleteContact cid False) >> redrawn
              _ -> pure ())
      -- A founder's setting changed from how it stands.
      changing gid f = do
        wanted <- (groupSettings <=< Map.lookup gid) . modelGroups <$> readMVar model
        forM_ wanted $ \current -> fire (ChangeSettings gid (f current)) >> redrawn
      mutingGroup gid = do
        now <- getCurrentTime
        g <- Map.lookup gid . modelGroups <$> readMVar model
        case g of
          Just g' | groupMuted now g' -> fire (UnmuteGroup gid) >> redrawn
          Just _ -> withUser $ \u ->
            showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Mute notifications" (map fst muteChoices)) $ \case
              BottomSheetItemSelected n | (_, for) : _ <- drop (fromIntegral n) muteChoices -> do
                from <- getCurrentTime
                fire (MuteGroup gid ((`addUTCTime` from) <$> for)) >> redrawn
              _ -> pure ()
          Nothing -> pure ()
      asking title message yes event = withUser $ \u ->
        showDialog (userDialogState u) DialogConfig
          { dcTitle = title, dcMessage = message, dcButton1 = yes, dcButton2 = Just "Cancel", dcButton3 = Nothing } $ \case
            DialogButton1 -> fire event >> redrawn
            _ -> pure ()
  groupsMade <- newIORef Map.empty
  let groupActionsFor gid@(GroupId chat) = readIORef groupsMade >>= \known -> case Map.lookup gid known of
        Just these -> pure these
        Nothing -> do
          these <- runActionM actions $ GroupActions
            <$> createAction (fire (Open (GroupChat gid)))
            <*> createAction (fire (Open (GroupDetails gid)))
            <*> createAction (mutingGroup gid)
            <*> createAction (do
                  kind <- maybe "group" (kindName . groupKind) . Map.lookup gid . modelGroups <$> readMVar model
                  asking ("Leave the " <> kind <> "?") "Its chat stays here. To join again, ask for a new invite."
                    "Leave" (LeaveGroup gid))
            <*> createAction (asking "Delete the chat?" "Its messages go too, and you leave the group if you're in it."
                                "Delete" (DeleteGroup gid))
            <*> createAction (copyToClipboard chat)
            <*> createAction (fire (Open (TopicScreen gid)))
            <*> createAction (fire (ToggleMentions gid))
            <*> createAction (fire (Open (InviteFriendsScreen gid)))
            <*> createAction (fire (Open (GroupSettingsScreen gid)))
            <*> createAction (changing gid (\s -> s { Tox.groupPrivate = not (Tox.groupPrivate s) }))
            <*> createAction (changing gid (\s -> s { Tox.groupTopicLock = not (Tox.groupTopicLock s) }))
            <*> createAction (withUser $ \u ->
                  showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Who may speak" (map fst voices)) $ \case
                    BottomSheetItemSelected n | (_, v) : _ <- drop (fromIntegral n) voices ->
                      changing gid (\s -> s { Tox.groupVoice = v })
                    _ -> pure ())
            <*> createAction (do
                  current <- readMVar model
                  forM_ (Map.lookup gid (modelGroups current)) $ \g -> withUser $ \u -> do
                    let senders = relayCounts g
                        choice (m, relays, total) =
                          memberLabel m <> ": " <> Text.pack (show relays) <> " of " <> Text.pack (show total) <> " messages relayed"
                    showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Which member is the relay bot?" (map choice senders)) $ \case
                      BottomSheetItemSelected n | (m, _, _) : _ <- drop (fromIntegral n) senders ->
                        fire (ChooseRelayBot gid (Just (memberKey m))) >> redrawn
                      _ -> pure ())
            <*> createAction (fire (ChooseRelayBot gid Nothing))
          modifyIORef' groupsMade (Map.insert gid these)
          pure these
  invitesMade <- newIORef Map.empty
  -- Each friend's invite to each group, once its screen shows them.
  invitingMade <- newIORef Map.empty
  -- Each staged item's X, by its chat and place.
  unstagingMade <- newIORef Map.empty
  let unstagingFor at@(cid, k) = readIORef unstagingMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions (createAction (fire (Unstage cid k)))
          modifyIORef' unstagingMade (Map.insert at this)
          pure this
  -- Each member's handle, offering what we may do with them as it stands.
  membersMade <- newIORef Map.empty
  let memberFor at@(gid, n) = readIORef membersMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions $ createAction $ do
            g <- Map.lookup gid . modelGroups <$> readMVar model
            forM_ g $ \g' -> withUser $ \u -> do
              let theirs = Map.findWithDefault Tox.User n (groupRoles g')
                  choices = ("Message privately", fire (PrivatelyTo (Just n)) >> fire (Open (GroupChat gid)))
                    : [ (memberActionName permission, fire (Moderate gid n permission))
                      | groupKind g' == NgcGroup, permission <- memberActions (groupSelfRole g') theirs ]
                  title = maybe "Member" memberLabel (Map.lookup n (groupMembers g'))
              showBottomSheet (userBottomSheetState u) (BottomSheetConfig title (map fst choices)) $ \case
                BottomSheetItemSelected k | (_, chosen) : _ <- drop (fromIntegral k) choices -> chosen >> redrawn
                _ -> pure ()
          modifyIORef' membersMade (Map.insert at this)
          pure this
  let invitingFor at@(gid, cid) = readIORef invitingMade >>= \known -> case Map.lookup at known of
        Just this -> pure this
        Nothing -> do
          this <- runActionM actions (createAction (fire (InviteFriend gid cid)))
          modifyIORef' invitingMade (Map.insert at this)
          pure this
  let inviteActionsFor at@(cid, gid) = readIORef invitesMade >>= \known -> case Map.lookup at known of
        Just these -> pure these
        Nothing -> do
          these <- runActionM actions $ InviteActions
            <$> createAction (fire (Open (InviteScreen cid gid)))
            <*> createAction (fire (AnswerInvite cid gid True))
            <*> createAction (fire (AnswerInvite cid gid False))
          modifyIORef' invitesMade (Map.insert at these)
          pure these
  -- Handles live as long as the app, so each contact gets its own once.
  made <- newIORef Map.empty
  let actionsFor cid = do
        known <- readIORef made
        case Map.lookup cid known of
          Just these -> pure these
          Nothing -> do
            these <- runActionM actions $ ContactActions
              <$> createAction (fire (Open (Chat cid)))
              <*> createAction (fire (Accept cid))
              <*> createAction (fire (Reject cid))
              <*> createAction (fire (Open (Details cid)))
              <*> createAction (muting cid)
              <*> createAction (deleting cid)
              <*> createAction (copyToClipboard (let ContactId key = cid in key))
              <*> createAction (getCurrentTime >>= fire . ForwardTo cid)
              -- From its chat, which the call then covers.
              <*> createAction (allowing False (\allowed -> do
                    fire (Open (Chat cid))
                    getCurrentTime >>= \now -> fire (StartCall now allowed)))
              <*> createAction (withUser $ \u ->
                    let ways = ("As they are", Nothing) : [ ("As " <> formatName f, Just (formatKey f)) | f <- pictureFormats ]
                    in showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Send pictures" (map fst ways)) $ \case
                         BottomSheetItemSelected n | (_, way) : _ <- drop (fromIntegral n) ways ->
                           fire (SendPictures cid way) >> redrawn
                         _ -> pure ())
            modifyIORef' made (Map.insert cid these)
            pure these
  -- And each message, once it is shown: a tap selects while others are,
  -- or opens or takes a file, and holding it offers what can be done.
  messageActions <- newIORef Map.empty
  let messageAction ref@(cid, i) = do
        known <- readIORef messageActions
        case Map.lookup ref known of
          Just these -> pure these
          Nothing -> do
            let message current = Map.lookup cid (modelContacts current) >>= listToMaybe . drop i . contactMessages
            these <- runActionM actions $ (,,)
              <$> createAction (do
                current <- readMVar model
                if not (Set.null (modelSelected current)) then fire (Select i)
                else forM_ (message current) $ \m -> do
                 forM_ (location (messageText m)) $ \(latitude, longitude) ->
                   when (isNothing (messageFile m)) $ openUri (mapsUri latitude longitude)
                 forM_ (messageFile m) $ \file -> case attachmentState file of
                  Complete                      -> openFile (attachmentPath file)
                  Waiting | not (messageMine m) -> fire (Download cid i)
                  _                             -> pure ())
              <*> createAction (do
                current <- readMVar model
                forM_ (message current) $ \m -> do
                  let call = isJust (messageCall m)
                      choices =
                        [ ("Open " <> written, openUri uri)
                        | not call, isNothing (messageFile m), (written, uri) <- take 3 (findLinks (messageText m)) ]
                        ++ [("Reply", fire (Reply i)) | not call]
                        ++ [("Forward", fire (Open (Forward cid [i]))) | forwardable m]
                        ++ [("Copy", copyToClipboard (copiedText current cid [i])) | not call]
                        ++ [("Select", fire (Select i)), ("Info", fire (Open (MessageInfo cid i)))]
                  readIORef user >>= mapM_ (\u ->
                    showBottomSheet (userBottomSheetState u) (BottomSheetConfig "Message" (map fst choices)) $ \case
                      BottomSheetItemSelected n | (_, chosen) : _ <- drop (fromIntegral n) choices -> chosen >> redrawn
                      _ -> pure ()))
              <*> createAction (fire (Cancel cid i))
            modifyIORef' messageActions (Map.insert ref these)
            pure these
  device <- getDeviceInfo
  -- Each key's identicon, made once at the largest size shown.
  avatars <- newIORef Map.empty
  let avatarSize = dp (theme (diScreenDensity device) 0 False) 48
      avatarOf key = atomicModifyIORef' avatars $ \known ->
        case Map.lookup key known of
          Just picture -> (known, picture)
          Nothing ->
            let picture = identiconPng (ceiling avatarSize) (identicon (unhex key))
            in (Map.insert key picture known, picture)
  -- Each picture's size, read once it is all there.
  pictureSizes <- newIORef Map.empty
  let sizeOf path = readIORef pictureSizes >>= \known -> case Map.lookup path known of
        Just size -> pure size
        Nothing -> do
          size <- if isQoi path
            then either (const Nothing) qoiSize <$> try @IOException (withFile path ReadMode (`BS.hGet` 14))
            else imageSize path
          modifyIORef' pictureSizes (Map.insert path size)
          pure size
  -- QOI pictures as bitmaps, decoded off the UI thread: none yet while
  -- decoding, or when the file is no QOI picture.
  decoded <- newIORef (Map.empty :: Map.Map FilePath (Maybe BS.ByteString))
  let decodedOf path = do
        known <- readIORef decoded
        unless (Map.member path known) $ do
          modifyIORef' decoded (Map.insert path Nothing)
          void . forkIO $ do
            bitmap <- try @IOException (BS.readFile path)
            let shown = either (const Nothing) (either (const Nothing) Just . qoiBitmap (diScreenWidth device)) bitmap
            forM_ shown $ \b -> atomicModifyIORef' decoded ((, ()) . Map.insert path (Just b)) >> join (readIORef redraw)
        pure (join (Map.lookup path known))
  -- Each account's handles: to go online as it, and to delete it.
  profileActions <- newIORef Map.empty
  let profileActionsFor pid = readIORef profileActions >>= \known -> case Map.lookup pid known of
        Just these -> pure these
        Nothing -> do
          these <- runActionM actions $ (,)
            <$> createAction (switching (pure pid))
            <*> createAction (withUser $ \u -> do
                  name <- maybe "this profile" accountName . find ((== pid) . accountId) . modelProfiles <$> readMVar model
                  showDialog (userDialogState u) DialogConfig
                    { dcTitle = "Delete " <> name <> "?"
                    , dcMessage = "Its chats and keys go too. Export it first to keep it."
                    , dcButton1 = "Delete"
                    , dcButton2 = Just "Cancel"
                    , dcButton3 = Nothing
                    } $ \case
                      DialogButton1 -> do
                        goOffline hub pid
                        Store.deleteProfile database pid
                        Store.profiles database >>= fire . ProfilesChanged
                        redrawn
                      _ -> pure ())
          modifyIORef' profileActions (Map.insert pid these)
          pure these
      -- A profile from a .tox file, online at once, unless hTox has it.
      taken path = do
        bytes <- BS.readFile path
        void (try @IOException (removeFile path >> removeDirectory (takeDirectory path)))
        case importSave bytes of
          Left why -> fire (Notify why) >> redrawn
          Right (known, imported) -> do
            others <- mapM (Store.identity . Store.profileStore database . accountId) =<< Store.profiles database
            if identitySecretKey known `elem` map identitySecretKey (catMaybes others)
              then fire (Notify "hTox has that profile already") >> redrawn
              else switching $ do
                pid <- Store.newProfile database (Just known) (modelProfile imported)
                Store.save (Store.profileStore database pid) initial imported
                pure pid
  -- What a notification's key says of a profile online, the one shown
  -- where it says none.
  let forProfile key f = do
        let (whose, key') = profileOf key
            pid = fromMaybe shownId whose
        sessions <- readMVar (hubSessions hub)
        forM_ (Map.lookup pid sessions) $ \s -> f pid s key'
      showing pid = if pid == shownId then join (readIORef redraw) else switching (pure pid)
  -- Our Tox ID's QR code, made again only when it changes.
  qr <- newIORef (Text.empty, BS.empty)
  let qrOf address = atomicModifyIORef' qr $ \known@(shown, picture) ->
        if shown == address then (known, picture)
        else let new = toxIdQr (ceiling (dp (theme (diScreenDensity device) 0 False) 200)) address
             in ((address, new), new)
  pure (MobileApp
    { maContext = loggingMobileContext
        { onBack = modifyMVar model $ \current ->
            pure $ maybe (current, False) (, True) (back current)
        , onLifecycle = \case
            -- Also back from the setting that lets calls ring there.
            Start -> fullScreenAllowed >>= fire . FullScreenChecked >> fire (Shown True)
            Stop -> fire (Shown False)
            _ -> pure ()
        , onNotificationOpened = \key -> forProfile key $ \pid s key' -> case notificationOf key' of
            -- The call's own screen, where it rings or goes on.
            Nothing -> showing pid
            Just opened -> do
              current <- readMVar (sessionModel s)
              let known = case opened of
                    Chat cid             -> Map.member cid (modelContacts current)
                    GroupChat gid        -> Map.member gid (modelGroups current)
                    InviteScreen cid gid -> Map.member (cid, gid) (modelInvites current)
                    _                    -> True
              when known $ sessionFire s (Open opened) >> showing pid
        , onCallButton = \key button -> forProfile key $ \pid s key' -> when (key' == callNotification) $ do
            call <- modelCall <$> readMVar (sessionModel s)
            forM_ call $ \c -> case button of
              Answer -> allowing (callPeerVideo c) $ \allowed -> do
                now <- getCurrentTime
                sessionFire s (AnswerCall now allowed) >> showing pid
              Decline -> getCurrentTime >>= sessionFire s . HangUp >> join (readIORef redraw)
              End -> getCurrentTime >>= sessionFire s . HangUp >> join (readIORef redraw)
        , onPicturePicked = fire . AvatarPicked
        , onFilePicked = \picked -> do
            taking <- atomicModifyIORef' importing (False,)
            if taking then taken (pickedPath picked) else do
              -- Kept with the app's files, as the cache is the system's to clear.
              now <- getCurrentTime
              let path = sent </> (show (toMillis now) <> "-" <> takeFileName (pickedPath picked))
              renameFile (pickedPath picked) path
              -- And the directory it was copied to alone.
              void (try (removeDirectory (takeDirectory (pickedPath picked))) :: IO (Either IOException ()))
              fileId <- randomByteString 32
              picture <- readIORef picturing
              current <- readMVar model
              let shrinkable = picture && map toLower (takeExtension path) /= ".gif"
                  way = case modelScreen current of
                    Chat cid -> pictureFormat =<< contactPictures =<< Map.lookup cid (modelContacts current)
                    _        -> Nothing
                  attach p name size = fire (Attach p name size fileId) >> redrawn
                  original = attach path (pickedName picked) (fromInteger (pickedSize picked))
                  -- In place of the original, with its name's extension.
                  instead extension write = do
                    let converted = dropExtension path <> extension
                    written <- write converted
                    if not written then original else do
                      -- A JPEG shrunk as a JPEG has taken its place already.
                      when (converted /= path) (removeFile path)
                      size <- withFile converted ReadMode hFileSize
                      attach converted (Text.pack (dropExtension (Text.unpack (pickedName picked)) <> extension)) (fromInteger size)
              -- Made off the UI thread, or sent as it is if it can't be.
              if not shrinkable then original else void . forkIO $ case way of
                Nothing -> do
                  let out = dropExtension path <> ".tmp"
                  shrinkPicture path out pictureSide >>= \case
                    Nothing -> original
                    Just kind -> instead (if kind == Jpeg then ".jpg" else ".png") (\converted -> True <$ renameFile out converted)
                Just f -> do
                  encoded <- (>>= \(w, h, rgba) -> formatEncode f w h rgba)
                    <$> picturePixels path (min (formatMaxPixels f) (pictureSide * pictureSide))
                  case encoded of
                    Nothing    -> original
                    Just bytes -> instead (formatExtension f) (\converted -> True <$ BS.writeFile converted bytes)
        }
    , maActionState = actions
    , maView = \state -> do
        writeIORef redraw (userRequestRedraw state)
        writeIORef permissions (Just (userPermissionState state))
        writeIORef user (Just state)
        first <- not <$> atomicModifyIORef' (hubFetched hub) (True,)
        -- Fresh nodes once there is a screen to fetch them with, but not
        -- around a proxy.
        when (first && isNothing (settingProxy settings)) $ performRequest (userHttpState state) (HttpRequest HttpGet nodesUrl [] BS.empty) $ \case
          Right response | hrStatusCode response == 200, Just nodes <- readNodes (hrRespBody response) -> do
            BS.writeFile nodesFile (hrRespBody response)
            writeIORef (hubNodes hub) nodes
            readMVar (hubSessions hub) >>= mapM_ ((`Network.useNodes` nodes) . sessionNetwork)
          other -> platformLog ("No nodes fetched: " <> Text.pack (either show (show . hrStatusCode) other))
        current <- readMVar model
        let contactIds = Map.keys (modelContacts current) ++ Map.keys (modelRequests current)
        perContact <- Map.fromList <$> traverse (\cid -> (,) cid <$> actionsFor cid) contactIds
        perMessage <- Map.fromList <$> traverse (\ref -> (,) ref <$> messageAction ref)
          [ (cid, i) | Chat cid <- [modelScreen current]
                     , contact <- maybe [] pure (Map.lookup cid (modelContacts current))
                     , i <- [0 .. length (contactMessages contact) - 1] ]
        perGroup <- Map.fromList <$> traverse (\gid -> (,) gid <$> groupActionsFor gid) (Map.keys (modelGroups current))
        perInvite <- Map.fromList <$> traverse (\at -> (,) at <$> inviteActionsFor at) (Map.keys (modelInvites current))
        perStaged <- Map.fromList <$> traverse (\at -> (,) at <$> unstagingFor at)
          [ (cid, k) | Chat cid <- [modelScreen current]
                     , k <- [0 .. length (Map.findWithDefault [] cid (modelStaged current)) - 1] ]
        perMember <- Map.fromList <$> traverse (\at -> (,) at <$> memberFor at)
          [ (gid, n) | GroupDetails gid <- [modelScreen current]
                     , g <- maybe [] pure (Map.lookup gid (modelGroups current)), n <- Map.keys (groupMembers g) ]
        perInviting <- Map.fromList <$> traverse (\at -> (,) at <$> invitingFor at)
          [ (gid, cid) | InviteFriendsScreen gid <- [modelScreen current]
                       , (cid, c) <- Map.toList (modelContacts current), contactOnline c ]
        identicons <- traverse avatarOf $ Map.fromList
          [ (key, key)
          | key <- Text.take 64 (profileToxId (modelProfile current))
                 : [key | ContactId key <- contactIds]
                 ++ [accountKey a | a <- modelProfiles current, not (Text.null (accountKey a))]
                 ++ [chat | GroupId chat <- Map.keys (modelGroups current)]
                 ++ [chat | (_, GroupId chat) <- Map.keys (modelInvites current)]
                 ++ [ memberKey m | GroupDetails gid <- [modelScreen current]
                                  , g <- maybe [] pure (Map.lookup gid (modelGroups current))
                                  , m <- Map.elems (groupMembers g), not (Text.null (memberKey m)) ] ]
        -- The picture each shows, or its identicon.
        let pictures = Map.fromList $
              [ (Text.take 64 (profileToxId (modelProfile current)), png)
              | Just png <- [(`Map.lookup` modelAvatars current) =<< modelAvatar current] ]
              ++ [ (key, png) | (ContactId key, c) <- Map.toList (modelContacts current)
                              , Just png <- [(`Map.lookup` modelAvatars current) =<< contactAvatar c] ]
            keys = Map.union pictures identicons
        code <- qrOf (profileToxId (modelProfile current))
        sizes <- traverse sizeOf $ Map.fromList $
          [ (attachmentPath a, attachmentPath a)
          | Chat cid <- [modelScreen current], c <- maybe [] pure (Map.lookup cid (modelContacts current))
          , m <- contactMessages c, Just a <- [messageFile m]
          , messageMine m || attachmentState a == Complete ]
          -- And those waiting to go.
          ++ [ (path, path) | Chat cid <- [modelScreen current]
                            , StagedFile path _ _ _ <- Map.findWithDefault [] cid (modelStaged current) ]
        bitmaps <- Map.traverseWithKey (\path _ -> decodedOf path) (Map.filterWithKey (\path _ -> isQoi path) sizes)
        dark <- isDarkMode
        clock24 <- is24HourClock
        now <- getCurrentTime
        zone <- getCurrentTimeZone
        let env = Env
              { envTheme = theme (diScreenDensity device)
                  (fromIntegral (diScreenWidth device)) dark
              , envNow = now
              , envZone = zone
              , envAvatar = \key -> Map.findWithDefault BS.empty key keys
              , envQr = code
              , envClock24 = clock24
              , envPictureSize = \path -> Map.findWithDefault Nothing path sizes
              , envDecoded = \path -> join (Map.lookup path bitmaps)
              }
        perProfile <- Map.fromList <$> traverse (\a -> (,) (accountId a) <$> profileActionsFor (accountId a)) (modelProfiles current)
        pure $ view env (handlers (perContact Map.!) perMessage (perProfile Map.!) (perGroup Map.!) (perInvite Map.!) (perInviting Map.!) (perMember Map.!) (perStaged Map.!)) current
    }, stop)
