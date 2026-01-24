{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- | The Tox instance behind the app: what it tells the model, and how it
-- does what the model asks.
module Tox.Android.Network
  ( Network
  , start
  , identityOf
  , run
  , looping
  , stop
  , perform
  , Media (..)
  , onMedia
  , useNodes
  , sendAudio
  , sendVideo
  ) where

import           Control.Concurrent        (ThreadId, forkIO, threadDelay)
import           Control.Exception         (IOException,
                                            SomeAsyncException (..),
                                            displayException, fromException,
                                            try)
import           Control.Monad             (forM_, unless, void, when)
import           Control.Monad.Logger      (LogLevel (..))
import qualified Crypto.Saltine.Class      as Sodium
import           Crypto.Saltine.Core.Utils (randomByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import           Data.Char                 (isDigit)
import           Data.IORef                (IORef, atomicModifyIORef',
                                            modifyIORef', newIORef, readIORef,
                                            writeIORef)
import           Data.List                 (find, sortOn)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (isJust, isNothing)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time                 (getCurrentTime)
import           Data.Word                 (Word32)
import           Hatter                    (platformLog)
import           System.Directory          (removeFile)
import qualified Tox.Application.Tox       as Tox
import qualified Tox.AV                    as AV
import           Tox.AV                    (BitRates (..), Pcm, Picture)
import qualified Tox.AV.MSI                as MSI
import           Tox.Crypto.Core.Key       (PublicKey)

import qualified Tox.Android.Avatars       as Avatars
import qualified Tox.Android.Files         as Files
import           Tox.Android.Messages      (parts)
import           Tox.Android.Model
import           Tox.Android.Nodes         (Node (..), usable)
import qualified Tox.Android.Outgoing      as Outgoing
import           Tox.Android.Store         (Identity (..))
import qualified Tox.Sync.Frame            as Frame

data Network = Network
  { networkTox         :: Tox.Tox
  , networkOutgoing    :: Outgoing.Outgoing
  , networkFiles       :: Files.Transfers
  , networkAvatars     :: Avatars.Avatars
  , networkReceived    :: FilePath
    -- ^ Where friends' files go.
  , networkAV          :: AV.ToxAV PublicKey
  , networkMedia       :: IORef Media
  , networkNodes       :: IORef [Node]
    -- ^ Those to join the network through.
  , networkProxied     :: Bool
    -- ^ Whether relays are reached through a proxy, so no name may be
    -- looked up outside it.
  , networkGroups      :: IORef (Map Word32 GroupId)
    -- ^ Tox's number for each group it is in this time.
  , networkConferences :: IORef (Map Tox.GroupNumber GroupId)
    -- ^ And for each conference.
  , networkSync        :: IORef (Map PublicKey [BS.ByteString])
    -- ^ Sync packets for each friend, in order, until Tox takes them: it
    -- refuses them while its queue is full.
  }

-- | Where a call's audio and video go.
data Media = Media
  { playAudio :: Int -> Int -> Pcm -> IO ()
    -- ^ The sampling rate, the channels and the samples.
  , showVideo :: Picture -> IO ()
  }

-- | Kilobits per second of what we send in a call.
audioRate, videoRate :: Word32
audioRate = 48
videoRate = 1000

-- | Microseconds between bootstraps while offline.
bootstrapInterval :: Int
bootstrapInterval = 30000000

-- | In a random order.
shuffled :: [a] -> IO [a]
shuffled xs = do
  keys <- BS.unpack <$> randomByteString (4 * length xs)
  pure . map snd . sortOn fst $ zip (chunks keys) xs
  where
    chunks bytes = case splitAt 4 bytes of
      ([], _)   -> []
      (k, rest) -> k : chunks rest

-- | How many of them each bootstrap tries.
bootstrapCount :: Int
bootstrapCount = 8

-- | Start Tox as who we were, or as someone new, with friends' files going
-- to a directory, joining the network through some nodes.
start :: Maybe Identity -> DeviceSettings -> FilePath -> [Node] -> IO (Either Tox.NewError Network)
start known device received known' = Tox.new options >>= \case
  Left err -> pure (Left err)
  Right tox -> do
    forM_ known (Tox.selfSetNospam tox . identityNospam)
    outgoing <- Outgoing.newOutgoing
    files <- Files.newTransfers
    avatars <- Avatars.newAvatars
    av <- AV.new AV.Transport
      { AV.sendLossless = Tox.friendQueueLosslessPacket tox
      , AV.sendLossy = Tox.friendQueueLossyPacket tox
      }
    media <- newIORef (Media (\_ _ _ -> pure ()) (const (pure ())))
    nodes <- newIORef known'
    groups <- newIORef Map.empty
    conferences <- newIORef Map.empty
    sync <- newIORef Map.empty
    pure (Right (Network tox outgoing files avatars received av media nodes (isJust (settingProxy device)) groups conferences sync))
  where
    options = Tox.defaultOptions
      { Tox.optSaveData = maybe Tox.NoSaveData Tox.SecretKey
          (Sodium.decode . identitySecretKey =<< known)
      , Tox.optLogger = logged
      -- Through a proxy only TCP goes, so nothing else may.
      , Tox.optUdpEnabled = settingUdp device && isNothing (settingProxy device)
      , Tox.optIpv6Enabled = settingIpv6 device
      , Tox.optLocalDiscovery = settingLan device && isNothing (settingProxy device)
      , Tox.optProxy = settingProxy device
      }
    logged level message = when (level >= LevelWarn) (platformLog message)

-- | Join the network through some of its nodes, in a different order each
-- time, so a few that are down can't take every place.
bootstrap :: Network -> IO ()
bootstrap network = do
  nodes <- readIORef (networkNodes network)
  chosen <- take bootstrapCount . usable . filter reachable <$> shuffled nodes
  forM_ chosen $ \n -> forM_ (Sodium.decode (nodeKey n)) $ \pk -> do
    forM_ (nodeTcpPort n) $ \tcp -> Tox.resolveNode (nodeHost n) tcp pk >>= mapM_ (Tox.addTcpRelay tox)
    Tox.resolveNode (nodeHost n) (nodePort n) pk >>= mapM_ (Tox.bootstrap tox)
  where
    tox = networkTox network
    -- Through a proxy, those named by address only: Tox would look names
    -- up outside it.
    reachable n = not (networkProxied network) || isAddress (nodeHost n)
    isAddress host = let octets = dotted host in
      length octets == 4 && all (\p -> not (null p) && length p <= 3 && all isDigit p && read p < (256 :: Int)) octets
    dotted s = case break (== '.') s of
      (part, _ : rest) -> part : dotted rest
      (part, [])       -> [part]

-- | Join through these from now on, at once if offline.
useNodes :: Network -> [Node] -> IO ()
useNodes network nodes = do
  writeIORef (networkNodes network) nodes
  status <- Tox.selfConnectionStatus (networkTox network)
  when (status == Tox.NotConnected) $ bootstrap network

-- | Tox gone, and with it the loops that ask it.
stop :: Network -> IO ()
stop = Tox.kill . networkTox

identityOf :: Network -> IO Identity
identityOf network =
  Identity (Sodium.encode (Tox.selfSecretKey tox)) <$> Tox.selfGetNospam tox
  where tox = networkTox network

-- | Report Tox's ID, join the network, and then report what happens on it,
-- in threads that end with Tox or when killed.
run :: Network -> (Event -> IO ()) -> IO [ThreadId]
run network report = do
  Tox.selfAddress tox >>= report . Started . hex
  sequence
    -- Again and again while offline, as Tox drops the nodes and relays it
    -- can't reach, and after an outage may have none left.
    [ looping "Bootstrap" $ do
        status <- Tox.selfConnectionStatus tox
        when (status == Tox.NotConnected) $ bootstrap network
        threadDelay bootstrapInterval
    , looping "Tox events" $ Tox.nextEvent tox >>= translate network >>= mapM_ report
    , looping "Call events" $ AV.nextEvent (networkAV network) >>= called network >>= mapM_ report
    , looping "Sending" $
        threadDelay Files.retryInterval >> Files.flush tox (networkFiles network) >>= mapM_ report
          >> Avatars.flush tox (networkAvatars network)
          >> Outgoing.flush tox (networkOutgoing network) >>= mapM_ report
          >> flushSync network
    ]
  where tox = networkTox network

-- | Sends each friend's sync packets in order until Tox refuses one for a
-- full queue, which goes again later. A friend gone takes theirs along.
flushSync :: Network -> IO ()
flushSync network = do
  queued <- atomicModifyIORef' (networkSync network) (Map.empty,)
  left <- Map.traverseMaybeWithKey send queued
  modifyIORef' (networkSync network) (Map.unionWith (++) left)
  where
    send _ [] = pure Nothing
    send pk (packet : rest) = Tox.friendSendLosslessPacket (networkTox network) pk packet >>= \case
      Right () -> send pk rest
      Left Tox.CustomSendQueueFull -> pure (Just (packet : rest))
      Left err -> Nothing <$ platformLog ("Sync packets dropped: " <> Text.pack (show err))

-- | Do something again and again in a thread of its own, going on after a
-- failure, so that one bad event doesn't end what comes after it. Only
-- Tox stopping or the thread being killed ends it.
looping :: Text.Text -> IO () -> IO ThreadId
looping name action = forkIO go
  where
    go = try action >>= \case
      Right () -> go
      Left failure
        | Just Tox.ToxStopped <- fromException failure -> platformLog (name <> ": Tox stopped")
        -- Killed, as when hTox goes online as another account.
        | Just (SomeAsyncException _) <- fromException failure -> pure ()
        | otherwise -> platformLog (name <> ": " <> Text.pack (displayException failure)) >> go

-- | What an event from Tox means to the model.
translate :: Network -> Tox.Event -> IO [Event]
translate network = \case
  Tox.SelfConnectionChanged status -> do
    platformLog ("Tox connection: " <> Text.pack (show status))
    pure [Connected (connection status)]
  Tox.FriendConnectionChanged pk status -> do
    when (status == Tox.NotConnected) $ do
      Files.friendGone files pk
      Avatars.friendGone avatars pk
      Outgoing.friendGone (networkOutgoing network) pk
      -- A session starts again with the next connection.
      modifyIORef' (networkSync network) (Map.delete pk)
    AV.friendConnection (networkAV network) pk (status /= Tox.NotConnected)
    pure [FriendOnline (contactId pk) (status /= Tox.NotConnected)]
  Tox.FriendName pk name -> pure [FriendNamed (contactId pk) (utf8 name)]
  Tox.FriendStatusMessage pk message -> pure [FriendStatusMessage (contactId pk) (utf8 message)]
  Tox.FriendStatus pk status -> pure [FriendStatus (contactId pk) status]
  Tox.FriendMessage pk kind message -> do
    now <- getCurrentTime
    pure [Received (contactId pk) now kind message]
  Tox.FriendReadReceipt pk receipt -> Outgoing.receipt (networkOutgoing network) pk receipt
  Tox.FriendRequest pk message -> pure [Requested (contactId pk) (utf8 message)]
  Tox.FileReceive pk n kind size fileId name
    | kind == Tox.fileKindAvatar -> Avatars.offered tox avatars (contactId pk) pk n size fileId
    | otherwise -> do
        now <- getCurrentTime
        let cid = contactId pk
        path <- Files.offered tox files (networkReceived network) now cid pk n kind size (utf8 name)
        pure [FileOffered cid (Receiving n) now (utf8 name) size p fileId | Just p <- [path]]
  Tox.FileReceiveChunk pk n position bytes -> avatarOr pk (Tox.Incoming n)
    (Avatars.received tox avatars pk n position bytes) (Files.received tox files pk n position bytes)
  Tox.FileChunkRequest pk n position len -> avatarOr pk (Tox.Outgoing n)
    ([] <$ Avatars.requested tox avatars pk n position len) (Files.requested tox files pk n position len)
  Tox.FileControlChanged pk ref control -> avatarOr pk ref
    ([] <$ when (control == Tox.FileCancel) (Avatars.cancelled avatars pk ref))
    (Files.controlled files pk ref control)
  Tox.FriendLosslessPacket pk packet
    | BS.take 1 packet == BS.singleton Frame.packetId -> pure [SyncReceived (contactId pk) packet]
    | otherwise -> [] <$ forward AV.losslessIds pk packet
  Tox.FriendLossyPacket pk packet -> [] <$ forward AV.lossyIds pk packet
  -- The invite starts with the group's chat ID.
  Tox.GroupInvite pk invite name ->
    pure [Invited (contactId pk) (GroupId (hex (BS.take chatIdLength invite))) NgcGroup (utf8 name) invite]
  -- The cookie holds the inviter's number for it, then its type and ID.
  Tox.ConferenceInvite pk kind cookie ->
    pure [ Invited (contactId pk) (GroupId (hex (BS.drop 2 cookie)))
             (if kind == audioConference then AudioConference else TextConference) "" cookie ]
  Tox.ConferenceConnected g -> ofConference g $ \gid -> do
    now <- getCurrentTime
    title <- Tox.conferenceTitle tox g
    (++) ([GroupNamed gid (utf8 t) | Just t <- [title], not (BS.null t)] ++ [GroupConnected gid now])
      <$> conferencePeers g gid
  Tox.ConferenceTitle g _ title -> ofConference g $ \gid ->
    (GroupNamed gid (utf8 title) :) <$> savedConference tox g gid
  Tox.ConferencePeerName g n name -> ofConference g $ \gid -> pure [MemberNamed gid (fromIntegral n) (utf8 name)]
  Tox.ConferencePeerListChanged g -> ofConference g (conferencePeers g)
  Tox.ConferenceMessage g n kind message -> ofConference g $ \gid -> do
    now <- getCurrentTime
    pure [GroupReceived gid now (fromIntegral n) kind (utf8 message)]
  Tox.GroupSelfJoin g -> ofGroup g $ \gid -> do
    name <- Tox.groupName tox g
    topic <- Tox.groupTopic tox g
    now <- getCurrentTime
    (++ [GroupNamed gid (utf8 n) | Just n <- [name]] ++ [TopicChanged gid now Nothing (utf8 t) | Just t <- [topic]]
         ++ [GroupConnected gid now]) <$> groupStanding tox g gid
  Tox.GroupJoinFail g why -> ofGroup g $ \gid -> pure [GroupRefused gid why]
  Tox.GroupPeerJoin g n pk -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    name <- maybe "" (utf8 . Tox.groupPeerName) . find ((== n) . Tox.groupPeerId) <$> Tox.groupPeers tox g
    -- Peers Tox would find again are part of what it keeps.
    (MemberJoined gid now n (Member (hex (Sodium.encode pk)) name) :) <$> groupStanding tox g gid
  Tox.GroupPeerExit g n how _ _ -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    pure [MemberLeft gid now n how]
  Tox.GroupPeerName g n name -> ofGroup g $ \gid -> pure [MemberNamed gid n (utf8 name)]
  Tox.GroupMessage g n kind message _ -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    pure [GroupReceived gid now n kind (utf8 message)]
  -- What Tox keeps of the group changed.
  Tox.GroupTopic g setter topic -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    (TopicChanged gid now setter (utf8 topic) :) <$> saved g gid
  Tox.GroupModeration g setter target what -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    (GroupModerated gid now setter target what :) <$> groupStanding tox g gid
  Tox.GroupPrivateMessage g n kind message _ -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    pure [PrivateReceived gid now n kind (utf8 message)]
  Tox.GroupPrivacy g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupPeerLimit g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupPassword g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupTopicLock g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupVoiceState g _ -> ofGroup g (groupStanding tox g)
  _ -> pure []
  where
    tox = networkTox network
    files = networkFiles network
    avatars = networkAvatars network
    -- Of an avatar's transfer, or else of a file's.
    avatarOr pk ref forAvatar forFile = Avatars.has avatars pk ref >>= \avatar ->
      if avatar then forAvatar else forFile
    connection = \case
      Tox.NotConnected -> Offline
      Tox.ConnectedTcp -> ViaTcp
      Tox.ConnectedUdp -> ViaUdp
    forward ids pk packet = forM_ (BS.uncons packet) $ \(ident, _) ->
      when (ident `elem` ids) $ AV.receivePacket (networkAV network) pk packet
    ofGroup g f = readIORef (networkGroups network) >>= maybe (pure []) f . Map.lookup g
    ofConference g f = readIORef (networkConferences network) >>= maybe (pure []) f . Map.lookup g
    -- Who is there but us, and what Tox would rejoin with.
    conferencePeers g gid = do
      now <- getCurrentTime
      peers <- Tox.conferencePeers tox g
      let others = [ (fromIntegral (Tox.conferencePeerNumber p), Member (hex (Sodium.encode (Tox.conferencePeerPublicKey p))) (utf8 (Tox.conferencePeerName p)))
                   | p <- peers, Tox.conferencePeerPublicKey p /= Tox.selfPublicKey tox ]
      (MembersListed gid now others :) <$> savedConference tox g gid
    audioConference = 1
    saved = savedGroup tox
    chatIdLength = 32

-- | What a call event means to the model. The media goes straight to where
-- it is played.
called :: Network -> AV.Event PublicKey -> IO [Event]
called network = \case
  AV.Invited pk caps -> do
    now <- getCurrentTime
    pure [CallInvited (contactId pk) now (AV.sendsVideo caps)]
  AV.PeerChanged pk caps -> do
    now <- getCurrentTime
    pure [CallPeer (contactId pk) now (AV.sendsVideo caps)]
  AV.Ended pk end -> do
    platformLog ("Call ended: " <> Text.pack (show end))
    now <- getCurrentTime
    -- A call placed at both ends at once, which either end may notice.
    pure $ if end `elem` [AV.Failed MSI.InvalidState, AV.PeerFailed MSI.InvalidState]
      then [CallGlared (contactId pk) now]
      else [CallEnded (contactId pk) now]
  AV.AudioReceived _ rate channels pcm -> do
    media <- readIORef (networkMedia network)
    [] <$ playAudio media rate channels pcm
  AV.VideoReceived _ picture -> do
    media <- readIORef (networkMedia network)
    [] <$ showVideo media picture

-- | Why Tox wouldn't add a friend, for the user.
notAdded :: Maybe Tox.AddFriendError -> Text.Text
notAdded = \case
  Nothing                     -> "Not a valid Tox ID"
  Just Tox.MessageEmpty       -> "The message can't be empty"
  Just Tox.MessageTooLong     -> "The message is too long"
  Just Tox.OwnKey             -> "This is your own Tox ID"
  Just Tox.AlreadyFriend      -> "Already a contact"

-- | Do what the model asks, and say what came of it.
perform :: Network -> Command -> IO [Event]
perform network = \case
  SetProfile nickname status -> do
    named <- Tox.selfSetName tox (Text.encodeUtf8 nickname)
    stated <- Tox.selfSetStatusMessage tox (Text.encodeUtf8 status)
    unless (named && stated) $ platformLog "Tox refused the nickname or status message"
    -- Groups each have our name in them.
    groups <- Map.keys <$> readIORef (networkGroups network)
    [] <$ forM_ groups (\g -> Tox.groupSetNick tox g (Text.encodeUtf8 (groupNick (Profile nickname status ""))))
  Befriend cid -> [] <$ forM_ (key cid) (Tox.friendAddNoRequest tox)
  SendSync cid packets -> [] <$ forM_ (key cid) (\pk -> do
    modifyIORef' (networkSync network) (Map.insertWith (flip (++)) pk packets)
    flushSync network)
  Unfriend cid -> [] <$ forM_ (key cid) (\pk -> do
    Files.friendGone files pk
    Avatars.friendGone (networkAvatars network) pk
    Outgoing.friendGone (networkOutgoing network) pk
    Tox.friendDelete tox pk)
  SendAvatar cid png -> [] <$ forM_ (key cid) (\pk -> Avatars.send tox (networkAvatars network) pk png)
  RequestFriend address message -> do
    let adding = Tox.friendAdd tox (unhex address) (Text.encodeUtf8 message)
    added <- adding >>= \case
      -- Tox drops requests from friends, so one who deleted us hears from
      -- us only as someone new.
      Left (Just Tox.AlreadyFriend) | Just (pk, _) <- Tox.parseAddress (unhex address) ->
        Tox.friendDelete tox pk >> adding
      result -> pure result
    pure [AddFailed address (notAdded err) | Left err <- [added]]
  SendMessage cid i message confirmed -> case key cid of
    Nothing -> pure []
    Just pk -> Outgoing.send tox (networkOutgoing network) cid pk i message confirmed
  SendFile cid i path name size fileId -> case key cid of
    Nothing -> pure []
    Just pk -> do
      started <- Files.offer tox files cid pk path name size fileId
      pure [FileStarted cid i ref | Just ref <- [started]]
  AcceptFile cid ref path -> maybe (pure []) (\pk -> Files.accept tox files pk ref path) (key cid)
  CancelFile cid ref -> [] <$ forM_ (key cid) (\pk -> Files.cancel tox files pk ref)
  ForgetFile path -> [] <$ (try (removeFile path) :: IO (Either IOException ()))
  CallFriend cid camera -> calling cid (\pk -> AV.call av pk (rates camera))
  AnswerFriend cid camera -> calling cid (\pk -> AV.answer av pk (rates camera))
  HangUpCall cid -> [] <$ forM_ (key cid) (\pk -> AV.control av pk AV.Cancel)
  CameraCall cid camera ->
    [] <$ forM_ (key cid) (\pk -> AV.setBitRate av pk AV.Video (if camera then fromIntegral videoRate else 0))
  JoinGroup cid gid invite nick password -> case key cid of
    Nothing -> pure []
    Just pk -> Tox.groupInviteAccept tox pk invite (Text.encodeUtf8 nick) (Text.encodeUtf8 password) >>= \case
      Right g -> [] <$ modifyIORef' (networkGroups network) (Map.insert g gid)
      Left Tox.AcceptAlreadyJoined -> pure []
      Left refusal -> do
        platformLog ("Group invite refused: " <> Text.pack (show refusal))
        pure [GroupRefused gid Tox.JoinFailUnknown]
  JoinConference cid gid cookie -> case key cid of
    Nothing -> pure []
    Just pk -> Tox.conferenceJoin tox pk cookie >>= \case
      Right g -> [] <$ modifyIORef' (networkConferences network) (Map.insert g gid)
      Left Tox.JoinAlreadyJoined -> pure []
      Left refusal -> do
        platformLog ("Conference invite refused: " <> Text.pack (show refusal))
        pure [GroupRefused gid Tox.JoinFailUnknown]
  RestoreGroup NgcGroup gid bytes -> Tox.groupRestore tox bytes >>= \case
    Just g -> [] <$ modifyIORef' (networkGroups network) (Map.insert g gid)
    Nothing -> do
      platformLog "Tox couldn't rejoin a group from what it kept"
      pure [GroupRefused gid Tox.JoinFailUnknown]
  -- A conference is rejoined as its peers come online, without telling.
  RestoreGroup _ gid bytes -> Tox.conferenceRestore tox bytes >>= \case
    Just g -> do
      modifyIORef' (networkConferences network) (Map.insert g gid)
      now <- getCurrentTime
      pure [GroupConnected gid now]
    Nothing -> do
      platformLog "Tox couldn't rejoin a conference from what it kept"
      pure [GroupRefused gid Tox.JoinFailUnknown]
  SendGroupMessage gid i message -> numberOf gid >>= \case
    Just g -> do
      sent <- mapM (Tox.groupSendMessage tox g Tox.MessageNormal) (parts message)
      case sequence sent of
        Right _ -> pure [GroupSent gid i]
        Left refusal -> [] <$ platformLog ("Group message refused: " <> Text.pack (show refusal))
    Nothing -> conferenceOf gid >>= \case
      Nothing -> pure []
      -- Nobody to send it to yet: it waits for the peers.
      Just g -> do
        sent <- mapM (Tox.conferenceSendMessage tox g Tox.MessageNormal) (parts message)
        pure [GroupSent gid i | and sent]
  SetGroupTopic gid topic -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> Tox.groupSetTopic tox g (Text.encodeUtf8 topic) >>= \case
      False -> pure [Notify "You may not change this group's topic"]
      True -> do
        now <- getCurrentTime
        (TopicSetByUs gid now topic :) <$> savedGroup tox g gid
  FoundGroup NewConference name _ -> Tox.conferenceNew tox >>= \case
    Nothing -> pure [Notify "Tox couldn't make the conference"]
    Just g -> do
      void $ Tox.conferenceSetTitle tox g (Text.encodeUtf8 name)
      Tox.conferenceId tox g >>= \case
        Nothing -> pure []
        Just conference -> do
          let gid = GroupId (hex conference)
          modifyIORef' (networkConferences network) (Map.insert g gid)
          now <- getCurrentTime
          (GroupCreated gid TextConference name now :) <$> savedConference tox g gid
  FoundGroup founding name nick ->
    Tox.groupNew tox (founding == PrivateGroup) (Text.encodeUtf8 name) (Text.encodeUtf8 nick) >>= \case
      Nothing -> pure [Notify "Tox couldn't make the group"]
      Just g -> Tox.groupChatId tox g >>= \case
        Nothing -> pure []
        Just chat -> do
          let gid = GroupId (hex chat)
          modifyIORef' (networkGroups network) (Map.insert g gid)
          now <- getCurrentTime
          (GroupCreated gid NgcGroup name now :) <$> groupStanding tox g gid
  JoinPublicGroup gid@(GroupId chat) nick password ->
    Tox.groupJoin tox (unhex chat) (Text.encodeUtf8 nick) (Text.encodeUtf8 password) >>= \case
      Right g -> [] <$ modifyIORef' (networkGroups network) (Map.insert g gid)
      Left Tox.AcceptAlreadyJoined -> pure []
      Left refusal -> do
        platformLog ("Public group refused: " <> Text.pack (show refusal))
        pure [GroupRefused gid Tox.JoinFailUnknown]
  -- Tox can invite only friends online, to a group it is connected to.
  InviteToGroup gid cid -> case key cid of
    Nothing -> pure []
    Just pk -> do
      invited <- numberOf gid >>= \case
        Just g -> Tox.groupInviteFriend tox g pk
        Nothing -> conferenceOf gid >>= maybe (pure False) (Tox.conferenceInvite tox pk)
      pure [Notify (if invited then "Invite sent" else "Couldn't invite them now")]
  SendPrivate gid i n message -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> do
      sent <- mapM (Tox.groupSendPrivateMessage tox g n Tox.MessageNormal) (parts message)
      case sequence sent of
        Right _ -> pure [GroupSent gid i]
        Left refusal -> [] <$ platformLog ("Private message refused: " <> Text.pack (show refusal))
  -- Tox tells of what we do only by its result.
  ModerateMember gid n permission -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> do
      done <- case permission of
        Kick          -> Tox.groupKickPeer tox g n
        MakeModerator -> Tox.groupSetRole tox g n Tox.Moderator
        MakeUser      -> Tox.groupSetRole tox g n Tox.User
        MakeObserver  -> Tox.groupSetRole tox g n Tox.Observer
      now <- getCurrentTime
      case done of
        Left refusal -> pure [Notify ("Tox refused: " <> Text.pack (show refusal))]
        Right () -> (\standing -> GroupModerated gid now Nothing (Just n) (moderation permission)
                                   : [MemberLeft gid now n Tox.ExitKicked | permission == Kick] ++ standing)
                      <$> groupStanding tox g gid
  -- Only what differs, as each change goes to every peer.
  SetGroupSettings gid wanted -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> Tox.groupSettings tox g >>= \case
      Nothing -> pure []
      Just now -> do
        let changes =
              [ Tox.groupSetPrivacy tox g (Tox.groupPrivate wanted) | Tox.groupPrivate wanted /= Tox.groupPrivate now ]
              ++ [ Tox.groupSetPassword tox g (Tox.groupPassword wanted) | Tox.groupPassword wanted /= Tox.groupPassword now ]
              ++ [ Tox.groupSetPeerLimit tox g (Tox.groupPeerLimit wanted) | Tox.groupPeerLimit wanted /= Tox.groupPeerLimit now ]
              ++ [ Tox.groupSetTopicLock tox g (Tox.groupTopicLock wanted) | Tox.groupTopicLock wanted /= Tox.groupTopicLock now ]
              ++ [ Tox.groupSetVoice tox g (Tox.groupVoice wanted) | Tox.groupVoice wanted /= Tox.groupVoice now ]
        taken <- and <$> sequence changes
        (++) [Notify "Tox refused a setting" | not taken] <$> groupStanding tox g gid
  QuitGroup gid -> numberOf gid >>= \case
    Just g -> do
      modifyIORef' (networkGroups network) (Map.delete g)
      [] <$ Tox.groupLeave tox g ""
    Nothing -> conferenceOf gid >>= \case
      Nothing -> pure []
      Just g -> do
        modifyIORef' (networkConferences network) (Map.delete g)
        [] <$ Tox.conferenceDelete tox g
  where
    numberOf gid = fmap fst . find ((== gid) . snd) . Map.toList <$> readIORef (networkGroups network)
    conferenceOf gid = fmap fst . find ((== gid) . snd) . Map.toList <$> readIORef (networkConferences network)
    av = networkAV network
    rates camera = BitRates audioRate (if camera then videoRate else 0)
    -- A call Tox refuses is over before it started, but for one the friend
    -- placed first, which comes as an invitation.
    calling cid begin = case key cid of
      Nothing -> ended cid
      Just pk -> begin pk >>= \case
        Left AV.AlreadyInCall -> [] <$ platformLog "Call refused: the friend's is there"
        Left refusal -> platformLog ("Call refused: " <> Text.pack (show refusal)) >> ended cid
        Right () -> pure []
    ended cid = (\now -> [CallEnded cid now]) <$> getCurrentTime
    tox = networkTox network
    files = networkFiles network
    key = contactKey

-- | What Tox keeps of a group now, to rejoin it with.
savedGroup :: Tox.Tox -> Word32 -> GroupId -> IO [Event]
savedGroup tox g gid = maybe [] (pure . GroupSaved gid) <$> Tox.groupSave tox g

-- | Our role, the members', the founder's settings and what we may do, as
-- they stand, with what Tox keeps of the group.
groupStanding :: Tox.Tox -> Word32 -> GroupId -> IO [Event]
groupStanding tox g gid = do
  self <- Tox.groupSelfRole tox g
  peers <- Tox.groupPeers tox g
  settings <- Tox.groupSettings tox g
  allowed <- Tox.groupMayTopic tox g
  (++) ([RolesKnown gid self [(Tox.groupPeerId p, Tox.groupPeerRole p) | p <- peers], TopicAllowed gid allowed]
        ++ [SettingsKnown gid s | Just s <- [settings]])
    <$> savedGroup tox g gid

-- | The moderation a permission is used for.
moderation :: MemberAction -> Tox.Moderation
moderation = \case
  Kick          -> Tox.ModKick
  MakeModerator -> Tox.ModModerator
  MakeUser      -> Tox.ModUser
  MakeObserver  -> Tox.ModObserver

-- | What Tox keeps of a conference now, to rejoin it with.
savedConference :: Tox.Tox -> Tox.GroupNumber -> GroupId -> IO [Event]
savedConference tox g gid = maybe [] (pure . GroupSaved gid) <$> Tox.conferenceSave tox g

-- | Have a call's media played and shown there.
onMedia :: Network -> Media -> IO ()
onMedia network = writeIORef (networkMedia network)

-- | A frame of our audio for the friend we have a call with.
sendAudio :: Network -> ContactId -> Int -> Int -> Pcm -> IO ()
sendAudio network cid rate channels pcm = forM_ (contactKey cid) $ \pk ->
  void $ AV.sendAudio (networkAV network) pk rate channels pcm

sendVideo :: Network -> ContactId -> Picture -> IO ()
sendVideo network cid picture = forM_ (contactKey cid) $ \pk ->
  void $ AV.sendVideo (networkAV network) pk picture

contactKey :: ContactId -> Maybe PublicKey
contactKey (ContactId text) = publicKey (Text.encodeUtf8 text)

publicKey :: BS.ByteString -> Maybe PublicKey
publicKey = either (const Nothing) Sodium.decode . Base16.decode

contactId :: PublicKey -> ContactId
contactId = ContactId . hex . Sodium.encode

hex :: BS.ByteString -> Text.Text
hex = Text.toUpper . Text.decodeUtf8 . Base16.encode

utf8 :: BS.ByteString -> Text.Text
utf8 = Text.decodeUtf8Lenient
