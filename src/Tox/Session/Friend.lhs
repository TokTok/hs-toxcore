\begin{code}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tox.Session.Friend where

import           Control.Monad             (filterM, forM_, guard, void, when,
                                            zipWithM)
import qualified Crypto.Saltine.Class      as Sodium
import qualified Data.Binary.Get           as Get
import           Data.Bits                 (xor)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (mapAccumL)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Word                 (Word32, Word64, Word8)

import qualified Data.Binary.Put           as Put
import qualified Tox.Core.Time             as Time
import           Tox.Core.Time             (TimeDiff, Timestamp)
import qualified Tox.Core.Timed            as Timed
import           Tox.Crypto.Core.Key       (Key (..), PublicKey)
import qualified Tox.Crypto.Core.KeyPair   as KeyPair
import qualified Tox.Onion.Operation       as Onion
import qualified Tox.Session.Connection    as Connection
import           Tox.Session.Connection    (ConnectionEvent (..))
import qualified Tox.Transport.Reliability as Reliability

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

packetIdOnline, packetIdOffline, packetIdNickname, packetIdStatusMessage,
  packetIdUserStatus, packetIdTyping, packetIdMessage, packetIdAction,
  packetIdFileSendRequest, packetIdFileControl, packetIdFileData :: Word8
packetIdOnline        = 0x18
packetIdOffline       = 0x19
packetIdNickname      = 0x30
packetIdStatusMessage = 0x31
packetIdUserStatus    = 0x32
packetIdTyping        = 0x33
packetIdMessage       = 0x40
packetIdAction        = 0x41
packetIdFileSendRequest = 0x50
packetIdFileControl     = 0x51
packetIdFileData        = 0x52

packetIds :: [Word8]
packetIds =
  [ packetIdOnline, packetIdOffline, packetIdNickname, packetIdStatusMessage
  , packetIdUserStatus, packetIdTyping, packetIdMessage, packetIdAction
  , packetIdFileSendRequest, packetIdFileControl, packetIdFileData
  ]

maxNameLength, maxStatusMessageLength, maxMessageLength, maxFriendRequestLength :: Int
maxNameLength          = 128
maxStatusMessageLength = 1007
maxMessageLength       = 1372
maxFriendRequestLength = 921

maxFileNameLength, maxFileDataSize, fileIdLength, maxFileTransfers :: Int
maxFileNameLength = 255
maxFileDataSize   = 1371
fileIdLength      = 32
maxFileTransfers  = 256

-- | Packets the congestion control lets us send that we keep for other data
-- than files, as toxcore does.
fileReservedSlots :: Int
fileReservedSlots = 16

-- | File kinds.  Avatars are files of kind 1.
fileKindData, fileKindAvatar :: Word32
fileKindData   = 0
fileKindAvatar = 1

-- | First wait for the friend to connect after a friend request.  It doubles
-- each time the request is sent again.
friendRequestTimeout :: TimeDiff
friendRequestTimeout = Time.seconds 5

-- | How many senders of received friend requests are remembered, to ignore
-- repeated requests.
maxReceivedRequests :: Int
maxReceivedRequests = 32

data UserStatus = Online | Away | Busy
  deriving (Eq, Show, Enum, Bounded)

data MessageType = MessageNormal | MessageAction
  deriving (Eq, Show)

-- | A file transfer: one we send or one we receive, by its number.
data FileRef = Outgoing Word8 | Incoming Word8
  deriving (Eq, Ord, Show)

data FileControl = FileResume | FilePause | FileCancel
  deriving (Eq, Show, Enum, Bounded)

data TransferStatus
  = NotAccepted
  | Transferring
  | Finished Word32 -- ^ All data sent, in the packet with this number.
  deriving (Eq, Show)

data Transfer = Transfer
  { tfKind        :: Word32
  , tfSize        :: Word64
  , tfStatus      :: TransferStatus
  , tfPausedUs    :: Bool
  , tfPausedOther :: Bool
  , tfTransferred :: Word64
  , tfRequested   :: Word64 -- ^ How far we asked our client for data to send.
  } deriving (Eq, Show)

data FileError
  = FileFriendNotFound
  | FileFriendNotConnected
  | FileNotFound
  | FileNameTooLong
  | FileTooMany
  | FileBadState      -- ^ The control or seek does not fit the transfer.
  | FileBadChunk      -- ^ Wrong position or length.
  | FileSendQueueFull
  deriving (Eq, Show)

data Friend = Friend
  { friendRealPk        :: PublicKey
  , friendName          :: ByteString
  , friendStatus        :: UserStatus
  , friendMsg           :: ByteString
  , friendTyping        :: Bool
  , friendOnline        :: Bool -- ^ Set by the friend's ONLINE packet.
  , friendRequest       :: Maybe OutgoingRequest -- ^ Until the friend connects.
  , friendLastMessageId :: Word32
  , friendReceipts      :: [(Word32, Word32)] -- ^ Message id and packet number, oldest first.
  , friendSending       :: Map Word8 Transfer
  , friendReceiving     :: Map Word8 Transfer
  , friendLastSeen      :: Word64 -- ^ Unix time we last saw the friend online, 0 for never.
  , friendInfoUnsent    :: [Word8] -- ^ Our info packets the friend is still to get, by packet id.
  } deriving (Show)

-- | A friend request we keep sending until the friend connects.
data OutgoingRequest = OutgoingRequest
  { orNospam   :: ByteString -- ^ From the friend's Tox ID.
  , orMessage  :: ByteString
  , orLastSent :: Maybe Timestamp
  , orTimeout  :: TimeDiff
  } deriving (Show)

data Messenger = Messenger
  { messengerFriends :: Map PublicKey Friend
  , selfName         :: ByteString
  , selfStatus       :: UserStatus
  , selfMsg          :: ByteString
  , selfNospam       :: Word32
  , receivedRequests :: [PublicKey] -- ^ Recent friend request senders.
  }

data AddFriendError
  = MessageEmpty
  | MessageTooLong
  | OwnKey
  | AlreadyFriend
  deriving (Eq, Show)

data SendMessageError
  = SendFriendNotFound
  | SendFriendNotConnected
  | SendEmpty
  | SendTooLong
  | SendQueueFull
  deriving (Eq, Show)

data CustomPacketError
  = CustomFriendNotFound
  | CustomFriendNotConnected
  | CustomEmpty
  | CustomInvalid -- ^ The packet id is not one for clients.
  | CustomTooLong
  | CustomSendQueueFull
  deriving (Eq, Show)

-- | What Messenger reports to the client.
data MessengerEvent
  = FriendConnectionStatus PublicKey Bool
  | FriendMessage PublicKey MessageType ByteString
  | FriendNameChanged PublicKey ByteString
  | FriendStatusMessageChanged PublicKey ByteString
  | FriendUserStatusChanged PublicKey UserStatus
  | FriendTypingChanged PublicKey Bool
  | FriendRequestReceived PublicKey ByteString -- ^ Sender and message.
  | FriendReadReceipt PublicKey Word32          -- ^ The friend received the message with this id.
  | FileReceived PublicKey Word8 Word32 Word64 ByteString ByteString -- ^ A file offer: number, kind, size, id and name.
  | FileChunkReceived PublicKey Word8 Word64 ByteString   -- ^ Data at a position, empty when the file is complete.
  | FileChunkRequested PublicKey Word8 Word64 Int         -- ^ Send this much data from this position, nothing when the friend has it all.
  | FileControlReceived PublicKey FileRef FileControl
  deriving (Eq, Show)

class (Monad m, Connection.ConnectionMonad m) => MessengerMonad m where
  getMessenger :: m Messenger
  putMessenger :: Messenger -> m ()

getsMessenger :: MessengerMonad m => (Messenger -> a) -> m a
getsMessenger f = f <$> getMessenger

modifyMessenger :: MessengerMonad m => (Messenger -> Messenger) -> m ()
modifyMessenger f = getMessenger >>= putMessenger . f

modifyFriend :: MessengerMonad m => PublicKey -> (Friend -> Friend) -> m ()
modifyFriend pk f = modifyMessenger $ \s -> s { messengerFriends = Map.adjust f pk (messengerFriends s) }

-- | Initialise a new Messenger.
initMessenger :: Word32 -> Messenger
initMessenger nospam = Messenger
  { messengerFriends = Map.empty
  , selfName         = ""
  , selfStatus       = Online
  , selfMsg          = ""
  , selfNospam       = nospam
  , receivedRequests = []
  }

-- | Add a friend without sending a friend request.
addFriend :: MessengerMonad m => PublicKey -> m ()
addFriend pk = addFriendEntry pk Nothing

addFriendEntry :: MessengerMonad m => PublicKey -> Maybe OutgoingRequest -> m ()
addFriendEntry pk request = do
  modifyMessenger $ \s -> s { messengerFriends = Map.insert pk (Friend pk "" Online "" False False request 0 [] Map.empty Map.empty 0 []) (messengerFriends s) }
  Connection.addFriend pk
  -- A conference may have connected us already.
  connected <- Connection.getsConn (maybe False Connection.fcOnline . Map.lookup pk . Connection.friends)
  when connected . void $ sendPacketId pk packetIdOnline BS.empty

-- | Stop being friends. The connection stays for whoever else uses it, so
-- the caller lets it go when nobody does.
deleteFriend :: MessengerMonad m => PublicKey -> m Bool
deleteFriend pk = do
  known <- getsMessenger $ Map.member pk . messengerFriends
  when known $ do
    _ <- sendPacketId pk packetIdOffline BS.empty
    modifyMessenger $ \s -> s { messengerFriends = Map.delete pk (messengerFriends s) }
  return known

-- | Add a friend by Tox ID and send them a friend request with a message.
addFriendWithRequest :: MessengerMonad m => PublicKey -> ByteString -> ByteString -> m (Either AddFriendError ())
addFriendWithRequest pk nospam message = do
  ourPk <- KeyPair.publicKey <$> Connection.getsConn Connection.ourRealKeys
  known <- getsMessenger $ Map.member pk . messengerFriends
  if | BS.null message                          -> return $ Left MessageEmpty
     | BS.length message > maxFriendRequestLength -> return $ Left MessageTooLong
     | pk == ourPk                              -> return $ Left OwnKey
     | known                                    -> return $ Left AlreadyFriend
     | otherwise -> Right <$> addFriendEntry pk (Just $ OutgoingRequest nospam message Nothing friendRequestTimeout)

-- | Our nospam as it appears in our Tox ID: little-endian, as toxcore writes
-- it on the platforms it runs on.
nospamBytes :: Messenger -> ByteString
nospamBytes = LBS.toStrict . Put.runPut . Put.putWord32le . selfNospam

toxIdLength, nospamLength, toxIdChecksumLength :: Int
toxIdLength = 32 + nospamLength + toxIdChecksumLength
nospamLength = 4
toxIdChecksumLength = 2

-- | A Tox ID: public key, nospam and a checksum of both.
toxId :: PublicKey -> Messenger -> ByteString
toxId pk m = body <> toxIdChecksum body
  where
    body = Sodium.encode (unKey pk) <> nospamBytes m

-- | The public key and nospam of a Tox ID with a valid checksum.
parseToxId :: ByteString -> Maybe (PublicKey, ByteString)
parseToxId bytes = do
  guard $ BS.length bytes == toxIdLength
  let (body, checksum) = BS.splitAt (toxIdLength - toxIdChecksumLength) bytes
      (pkBytes, nospam) = BS.splitAt (BS.length body - nospamLength) body
  guard $ toxIdChecksum body == checksum
  pk <- Key <$> Sodium.decode pkBytes
  return (pk, nospam)

toxIdChecksum :: ByteString -> ByteString
toxIdChecksum body = BS.pack [foldr xor 0 (evens body), foldr xor 0 (evens $ BS.drop 1 body)]
  where
    evens bs = [ BS.index bs i | i <- [0, 2 .. BS.length bs - 1] ]

-- | Periodic work: send friend requests, again if the friend did not connect
-- in time, and report messages that arrived.
doMessenger :: (MessengerMonad m, Onion.OnionNodeMonad m) => m [MessengerEvent]
doMessenger = do
  sendFriendRequests
  friends <- getsMessenger $ Map.elems . messengerFriends
  receipts <- concat <$> mapM checkReceipts friends
  mapM_ (sendInfoUnsent . friendRealPk) (filter friendOnline friends)
  chunks <- concat <$> mapM (requestChunks . friendRealPk) (filter friendOnline friends)
  return $ receipts ++ chunks
  where
    checkReceipts f = do
      delivered <- takeWhileM (Connection.isDelivered (friendRealPk f) . snd) (friendReceipts f)
      if null delivered then return [] else do
        modifyFriend (friendRealPk f) $ \f' -> f' { friendReceipts = drop (length delivered) (friendReceipts f') }
        return [ FriendReadReceipt (friendRealPk f) msgId | (msgId, _) <- delivered ]
    takeWhileM _ [] = return []
    takeWhileM p (x : xs) = p x >>= \ok -> if ok then (x :) <$> takeWhileM p xs else return []

sendFriendRequests :: (MessengerMonad m, Onion.OnionNodeMonad m) => m ()
sendFriendRequests = do
  now <- Timed.askTime
  pending <- getsMessenger $ \s -> [ (friendRealPk f, r) | f <- Map.elems (messengerFriends s), Just r <- [friendRequest f] ]
  forM_ pending $ \(pk, r) -> case orLastSent r of
    Nothing -> do
      sent <- Connection.sendFriendRequestPacket pk (orNospam r <> orMessage r)
      when sent $ setRequest pk r { orLastSent = Just now }
    Just t | Time.addTime t (orTimeout r) < now ->
      setRequest pk r { orLastSent = Nothing, orTimeout = orTimeout r * 2 }
    Just _ -> return ()
  where
    setRequest pk r = modifyFriend pk $ \f -> f { friendRequest = Just r }

-- | A friend request is valid if it carries our nospam and comes from someone
-- who is not a friend and did not send one recently.
handleFriendRequest :: MessengerMonad m => PublicKey -> ByteString -> m [MessengerEvent]
handleFriendRequest pk bytes = do
  m <- getMessenger
  let (nospam, message) = BS.splitAt 4 bytes
  if nospam /= nospamBytes m || BS.null message || Map.member pk (messengerFriends m) || pk `elem` receivedRequests m
  then return []
  else do
    putMessenger m { receivedRequests = take maxReceivedRequests (pk : receivedRequests m) }
    return [FriendRequestReceived pk message]

isFriendOnline :: MessengerMonad m => PublicKey -> m Bool
isFriendOnline pk = getsMessenger $ maybe False friendOnline . Map.lookup pk . messengerFriends

-- | Send an online friend a lossless or lossy packet with an id clients may
-- use, the id included.
sendCustomPacket :: MessengerMonad m => (Word8 -> Bool) -> Bool -> PublicKey -> ByteString -> m (Either CustomPacketError ())
sendCustomPacket clientId lossless pk bytes = do
  mFriend <- getsMessenger $ Map.lookup pk . messengerFriends
  case (mFriend, BS.uncons bytes) of
    (Nothing, _) -> return $ Left CustomFriendNotFound
    (Just f, _) | not (friendOnline f) -> return $ Left CustomFriendNotConnected
    (_, Nothing) -> return $ Left CustomEmpty
    (_, Just (ident, _))
      | not (clientId ident) -> return $ Left CustomInvalid
      | BS.length bytes > Reliability.maxCryptoDataSize -> return $ Left CustomTooLong
      | otherwise -> do
          sent <- if lossless then Connection.sendFriendData pk bytes else Connection.sendFriendLossy pk bytes
          return $ if sent then Right () else Left CustomSendQueueFull

-- | Send a message to an online friend: its id, which a 'FriendReadReceipt'
-- reports once the friend received it.
sendMessage :: MessengerMonad m => PublicKey -> MessageType -> ByteString -> m (Either SendMessageError Word32)
sendMessage pk ty msg = do
  mFriend <- getsMessenger $ Map.lookup pk . messengerFriends
  case mFriend of
    Nothing -> return $ Left SendFriendNotFound
    Just f
      | not (friendOnline f)            -> return $ Left SendFriendNotConnected
      | BS.null msg                     -> return $ Left SendEmpty
      | BS.length msg > maxMessageLength -> return $ Left SendTooLong
      | otherwise -> Connection.sendFriendDataNumbered pk (BS.cons (messagePacketId ty) msg) >>= \case
          Nothing -> return $ Left SendQueueFull
          Just num -> do
            let msgId = friendLastMessageId f + 1
            modifyFriend pk $ \f' -> f' { friendLastMessageId = msgId, friendReceipts = friendReceipts f' ++ [(msgId, num)] }
            return $ Right msgId
  where
    messagePacketId MessageNormal = packetIdMessage
    messagePacketId MessageAction = packetIdAction

setTyping :: MessengerMonad m => PublicKey -> Bool -> m Bool
setTyping pk typing = do
  online <- isFriendOnline pk
  if online then sendPacketId pk packetIdTyping (BS.singleton $ if typing then 1 else 0) else return False

setSelfName :: MessengerMonad m => ByteString -> m Bool
setSelfName name
  | BS.length name > maxNameLength = return False
  | otherwise = do
      modifyMessenger $ \s -> s { selfName = name }
      True <$ selfInfoChanged packetIdNickname

setSelfStatusMessage :: MessengerMonad m => ByteString -> m Bool
setSelfStatusMessage msg
  | BS.length msg > maxStatusMessageLength = return False
  | otherwise = do
      modifyMessenger $ \s -> s { selfMsg = msg }
      True <$ selfInfoChanged packetIdStatusMessage

setSelfStatus :: MessengerMonad m => UserStatus -> m ()
setSelfStatus status = do
  modifyMessenger $ \s -> s { selfStatus = status }
  selfInfoChanged packetIdUserStatus

-- | Every friend is to get the info packet again, and those online try now.
selfInfoChanged :: MessengerMonad m => Word8 -> m ()
selfInfoChanged ident = do
  pks <- getsMessenger $ Map.keys . messengerFriends
  forM_ pks $ \pk -> modifyFriend pk $ \f -> f { friendInfoUnsent = owe ident (friendInfoUnsent f) }
  mapM_ sendInfoUnsent pks
  where owe i is = filter (/= i) is ++ [i]

sendPacketId :: MessengerMonad m => PublicKey -> Word8 -> ByteString -> m Bool
sendPacketId pk ident = Connection.sendFriendData pk . BS.cons ident

-- | Our name, status message and user status, which a friend gets when it
-- comes online and whenever they change.
selfInfo :: [Word8]
selfInfo = [packetIdNickname, packetIdStatusMessage, packetIdUserStatus]

-- | Send an online friend the info packets it is still to get, keeping
-- those its queue has no room for, to try again, as toxcore does.
sendInfoUnsent :: MessengerMonad m => PublicKey -> m ()
sendInfoUnsent pk = do
  m <- getMessenger
  forM_ (Map.lookup pk $ messengerFriends m) $ \f -> when (friendOnline f && not (null $ friendInfoUnsent f)) $ do
    left <- filterM (\ident -> not <$> sendPacketId pk ident (payload m ident)) (friendInfoUnsent f)
    modifyFriend pk $ \f' -> f' { friendInfoUnsent = left }
  where
    payload m ident
      | ident == packetIdNickname = selfName m
      | ident == packetIdStatusMessage = selfMsg m
      | otherwise = BS.singleton . fromIntegral . fromEnum $ selfStatus m

setOnline :: MessengerMonad m => PublicKey -> Bool -> m [MessengerEvent]
setOnline pk online = do
  wasOnline <- isFriendOnline pk
  -- Like toxcore, transfers end when the friend goes offline.
  modifyFriend pk $ \f -> f
    { friendOnline = online
    , friendReceipts = if online then friendReceipts f else []
    , friendSending = if online then friendSending f else Map.empty
    , friendReceiving = if online then friendReceiving f else Map.empty
    }
  if wasOnline == online then return [] else do
    when online $ do
      modifyFriend pk $ \f -> f { friendInfoUnsent = selfInfo }
      sendInfoUnsent pk
    return [FriendConnectionStatus pk online]

-- | Handle what the friend connections report.
handleConnectionEvent :: MessengerMonad m => ConnectionEvent -> m [MessengerEvent]
handleConnectionEvent = \case
  FriendOnline pk      -> do
    -- Conference peers that are not our friends have friend connections too.
    known <- getsMessenger $ Map.member pk . messengerFriends
    when known $ do
      modifyFriend pk $ \f -> f { friendRequest = Nothing }
      void $ sendPacketId pk packetIdOnline BS.empty
    return []
  FriendOffline pk     -> setOnline pk False
  FriendData pk bytes  -> handleFriendPacket pk bytes
  FriendLossyData _ _  -> return []
  FriendRequest pk bytes -> handleFriendRequest pk bytes

handleFriendPacket :: MessengerMonad m => PublicKey -> ByteString -> m [MessengerEvent]
handleFriendPacket pk bytes = do
  known <- getsMessenger $ Map.member pk . messengerFriends
  online <- isFriendOnline pk
  case BS.uncons bytes of
    Nothing -> return []
    Just (ident, payload)
      | not known -> return []
      | not online -> if ident == packetIdOnline && BS.null payload
          then do
            _ <- sendPacketId pk packetIdOnline BS.empty
            setOnline pk True
          else return []
      | ident == packetIdOffline -> setOnline pk False
      | ident == packetIdNickname && BS.length payload <= maxNameLength -> do
          modifyFriend pk $ \f -> f { friendName = payload }
          return [FriendNameChanged pk payload]
      | ident == packetIdStatusMessage && BS.length payload <= maxStatusMessageLength -> do
          modifyFriend pk $ \f -> f { friendMsg = payload }
          return [FriendStatusMessageChanged pk payload]
      | ident == packetIdUserStatus, [status] <- BS.unpack payload
      , fromIntegral status <= fromEnum (maxBound :: UserStatus) -> do
          let userStatus = toEnum $ fromIntegral status
          modifyFriend pk $ \f -> f { friendStatus = userStatus }
          return [FriendUserStatusChanged pk userStatus]
      | ident == packetIdTyping, [typing] <- BS.unpack payload -> do
          modifyFriend pk $ \f -> f { friendTyping = typing /= 0 }
          return [FriendTypingChanged pk (typing /= 0)]
      | ident == packetIdMessage && not (BS.null payload) ->
          return [FriendMessage pk MessageNormal payload]
      | ident == packetIdAction && not (BS.null payload) ->
          return [FriendMessage pk MessageAction payload]
      | ident == packetIdFileSendRequest -> handleFileSendRequest pk payload
      | ident == packetIdFileControl -> handleFileControl pk payload
      | ident == packetIdFileData -> handleFileData pk payload
      | otherwise -> return []


{-------------------------------------------------------------------------------
 -
 - :: File transfers.
 -
 ------------------------------------------------------------------------------}

encodeWord32 :: Word32 -> ByteString
encodeWord32 = LBS.toStrict . Put.runPut . Put.putWord32be

encodeWord64 :: Word64 -> ByteString
encodeWord64 = LBS.toStrict . Put.runPut . Put.putWord64be

decodeWord64 :: ByteString -> Word64
decodeWord64 = Get.runGet Get.getWord64be . LBS.fromStrict

chunksBetween :: Word64 -> Word64 -> Int
chunksBetween from to = fromIntegral $ d `div` chunk + (if d `mod` chunk == 0 then 0 else 1)
  where
    d = to - from
    chunk = fromIntegral maxFileDataSize

getTransfer :: MessengerMonad m => PublicKey -> FileRef -> m (Maybe Transfer)
getTransfer pk ref = getsMessenger $ \s -> Map.lookup pk (messengerFriends s) >>= \f -> case ref of
  Outgoing n -> Map.lookup n (friendSending f)
  Incoming n -> Map.lookup n (friendReceiving f)

setTransfer :: MessengerMonad m => PublicKey -> FileRef -> Maybe Transfer -> m ()
setTransfer pk ref t = modifyFriend pk $ \f -> case ref of
  Outgoing n -> f { friendSending = Map.alter (const t) n (friendSending f) }
  Incoming n -> f { friendReceiving = Map.alter (const t) n (friendReceiving f) }

fileControlId :: FileControl -> Word8
fileControlId = fromIntegral . fromEnum

fileControlSeek :: Word8
fileControlSeek = 3

-- | A file control for one of our transfers.  Its first byte says whether
-- we receive the file.
sendFileControl :: MessengerMonad m => PublicKey -> FileRef -> Word8 -> ByteString -> m Bool
sendFileControl pk ref control payload = sendPacketId pk packetIdFileControl $
  BS.pack [if incoming then 1 else 0, n, control] <> payload
  where
    (incoming, n) = case ref of
      Outgoing i -> (False, i)
      Incoming i -> (True, i)

-- | Offer a friend a file.  The id names the file, so a transfer can be
-- resumed.  Its data is asked for with 'FileChunkRequested' once the friend
-- accepts.
sendFile :: MessengerMonad m => PublicKey -> Word32 -> Word64 -> ByteString -> ByteString -> m (Either FileError Word8)
sendFile pk kind size fileId name = do
  mFriend <- getsMessenger $ Map.lookup pk . messengerFriends
  case mFriend of
    Nothing -> return $ Left FileFriendNotFound
    Just f
      | BS.length name > maxFileNameLength -> return $ Left FileNameTooLong
      | otherwise -> case filter (`Map.notMember` friendSending f) [0 .. fromIntegral (maxFileTransfers - 1)] of
          [] -> return $ Left FileTooMany
          n : _ -> do
            sent <- sendPacketId pk packetIdFileSendRequest $
              BS.singleton n <> encodeWord32 kind <> encodeWord64 size <> fileId <> name
            if not sent then return $ Left FileFriendNotConnected else do
              setTransfer pk (Outgoing n) $ Just (Transfer kind size NotAccepted False False 0 0)
              return $ Right n

-- | Resume (or accept), pause or cancel a transfer.
controlFile :: MessengerMonad m => PublicKey -> FileRef -> FileControl -> m (Either FileError ())
controlFile pk ref control = do
  online <- isFriendOnline pk
  mTransfer <- getTransfer pk ref
  case mTransfer of
    _ | not online -> return $ Left FileFriendNotConnected
    Nothing -> return $ Left FileNotFound
    Just t -> case control of
      FilePause | tfPausedUs t || tfStatus t /= Transferring -> return $ Left FileBadState
      FileResume | not (canResume t) -> return $ Left FileBadState
      _ -> do
        sent <- sendFileControl pk ref (fileControlId control) BS.empty
        if not sent then return $ Left FileFriendNotConnected else Right <$> setTransfer pk ref (case control of
          FileCancel -> Nothing
          FilePause  -> Just t { tfPausedUs = True }
          FileResume -> Just t { tfStatus = if tfStatus t == NotAccepted then Transferring else tfStatus t, tfPausedUs = False })
  where
    -- Only the receiver accepts a transfer, and we only resume what we paused.
    canResume t = case (tfStatus t, ref) of
      (Transferring, _)         -> tfPausedUs t
      (NotAccepted, Incoming _) -> True
      _                         -> False

-- | Ask the sender to start at a position, before accepting.
seekFile :: MessengerMonad m => PublicKey -> Word8 -> Word64 -> m (Either FileError ())
seekFile pk n position = do
  mTransfer <- getTransfer pk (Incoming n)
  case mTransfer of
    Nothing -> return $ Left FileNotFound
    Just t
      | tfStatus t /= NotAccepted || position >= tfSize t -> return $ Left FileBadState
      | otherwise -> do
          sent <- sendFileControl pk (Incoming n) fileControlSeek (encodeWord64 position)
          if sent
          then Right <$> setTransfer pk (Incoming n) (Just t { tfTransferred = position })
          else return $ Left FileFriendNotConnected

-- | Send the data a 'FileChunkRequested' asked for.
sendFileChunk :: MessengerMonad m => PublicKey -> Word8 -> Word64 -> ByteString -> m (Either FileError ())
sendFileChunk pk n position bytes = do
  online <- isFriendOnline pk
  mTransfer <- getTransfer pk (Outgoing n)
  queued <- Connection.sendQueueLength pk
  let len = fromIntegral (BS.length bytes)
  case mTransfer of
    _ | not online -> return $ Left FileFriendNotConnected
    Nothing -> return $ Left FileNotFound
    Just t
      | tfStatus t /= Transferring -> return $ Left FileBadState
      | BS.length bytes > maxFileDataSize || tfSize t - tfTransferred t < len
        || (tfSize t /= maxBound && BS.length bytes /= maxFileDataSize && tfTransferred t + len /= tfSize t)
        || position /= tfTransferred t || (tfRequested t <= position && tfSize t /= 0) -> return $ Left FileBadChunk
      | queued > fromIntegral Reliability.bufferSize - fileReservedSlots -> return $ Left FileSendQueueFull
      | otherwise -> Connection.sendFriendDataCongested pk (BS.pack [packetIdFileData, n] <> bytes) >>= \case
          Nothing -> return $ Left FileSendQueueFull
          Just num -> do
            let transferred = tfTransferred t + len
                done = BS.length bytes /= maxFileDataSize || transferred == tfSize t
            Right <$> setTransfer pk (Outgoing n) (Just t
              { tfTransferred = transferred
              , tfStatus = if done then Finished num else Transferring
              })

-- | Ask our client for data while the friend's queue has room, and report
-- finished transfers once the friend has all of their data.
requestChunks :: MessengerMonad m => PublicKey -> m [MessengerEvent]
requestChunks pk = do
  sending <- getsMessenger $ maybe Map.empty friendSending . Map.lookup pk . messengerFriends
  free <- Connection.freeSendSlots pk
  let asked = sum [ chunksBetween (tfTransferred t) (tfRequested t) | t <- Map.elems sending, tfStatus t == Transferring ]
      open = [ (n, t) | (n, t) <- Map.toList sending, tfStatus t == Transferring, not (paused t)
                      , tfSize t /= 0, tfRequested t < tfSize t ]
      shares = roundRobin (free - fileReservedSlots - asked) [ chunksBetween (tfRequested t) (tfSize t) | (_, t) <- open ]
  finished <- concat <$> mapM finish (Map.toList sending)
  requested <- concat <$> zipWithM request open shares
  return $ finished ++ requested
  where
    paused t = tfPausedUs t || tfPausedOther t
    finish (n, t) = case tfStatus t of
      Finished num -> do
        delivered <- Connection.isDelivered pk num
        if not delivered then return [] else do
          setTransfer pk (Outgoing n) Nothing
          return [FileChunkRequested pk n (tfTransferred t) 0]
      Transferring | tfSize t == 0 && not (paused t) -> [] <$ sendFileChunk pk n 0 BS.empty
      _ -> return []
    request _ 0 = return []
    request (n, t) chunks = do
      let positions = take chunks [tfRequested t, tfRequested t + fromIntegral maxFileDataSize ..]
          requests = [ FileChunkRequested pk n p (fromIntegral $ min (fromIntegral maxFileDataSize) (tfSize t - p)) | p <- positions ]
      setTransfer pk (Outgoing n) (Just t { tfRequested = min (tfSize t) (tfRequested t + fromIntegral (chunks * maxFileDataSize)) })
      return requests

-- | Share a budget out one at a time to each of the wants in turn, as
-- toxcore does between file transfers.
roundRobin :: Int -> [Int] -> [Int]
roundRobin budget wants
  | budget <= 0 || all (<= 0) wants = map (const 0) wants
  | otherwise = zipWith (+) given (roundRobin (budget - sum given) (zipWith (-) wants given))
  where
    given = snd $ mapAccumL (\b w -> if b > 0 && w > 0 then (b - 1, 1) else (b, 0)) budget wants

handleFileSendRequest :: MessengerMonad m => PublicKey -> ByteString -> m [MessengerEvent]
handleFileSendRequest pk payload
  | BS.length payload < 1 + 4 + 8 + fileIdLength || BS.length name > maxFileNameLength = return []
  | otherwise = do
      existing <- getTransfer pk (Incoming n)
      case existing of
        Just _ -> return []
        Nothing -> do
          setTransfer pk (Incoming n) $ Just (Transfer kind size NotAccepted False False 0 0)
          return [FileReceived pk n kind size fileId name]
  where
    n = BS.head payload
    kind = Get.runGet Get.getWord32be . LBS.fromStrict $ BS.take 4 (BS.drop 1 payload)
    size = decodeWord64 $ BS.take 8 (BS.drop 5 payload)
    fileId = BS.take fileIdLength (BS.drop (1 + 4 + 8) payload)
    name = BS.drop (1 + 4 + 8 + fileIdLength) payload

handleFileControl :: MessengerMonad m => PublicKey -> ByteString -> m [MessengerEvent]
handleFileControl pk payload = case BS.unpack (BS.take 3 payload) of
  [direction, n, control] -> do
    -- The friend's incoming files are our outgoing ones.
    let ref = if direction == 1 then Outgoing n else Incoming n
        rest = BS.drop 3 payload
    mTransfer <- getTransfer pk ref
    case mTransfer of
      Nothing -> do
        when (control /= fileControlId FileCancel) $
          void $ sendFileControl pk ref (fileControlId FileCancel) BS.empty
        return []
      Just t
        | control == fileControlId FileResume -> case (ref, tfStatus t) of
            (Outgoing _, NotAccepted) -> resumed ref t { tfStatus = Transferring }
            _ | tfPausedOther t -> resumed ref t { tfPausedOther = False }
            _ -> return []
        | control == fileControlId FilePause ->
            if tfPausedOther t || tfStatus t /= Transferring then return [] else do
              setTransfer pk ref (Just t { tfPausedOther = True })
              return [FileControlReceived pk ref FilePause]
        | control == fileControlId FileCancel -> do
            setTransfer pk ref Nothing
            return [FileControlReceived pk ref FileCancel]
        | control == fileControlSeek, Outgoing _ <- ref, BS.length rest == 8, tfStatus t == NotAccepted
        , decodeWord64 rest < tfSize t -> do
            setTransfer pk ref (Just t { tfRequested = decodeWord64 rest, tfTransferred = decodeWord64 rest })
            return []
        | otherwise -> return []
  _ -> return []
  where
    resumed ref t = do
      setTransfer pk ref (Just t)
      return [FileControlReceived pk ref FileResume]

handleFileData :: MessengerMonad m => PublicKey -> ByteString -> m [MessengerEvent]
handleFileData pk payload = case BS.uncons payload of
  Nothing -> return []
  Just (n, bytes) -> do
    mTransfer <- getTransfer pk (Incoming n)
    case mTransfer of
      Just t | tfStatus t == Transferring -> do
        let remaining = tfSize t - tfTransferred t
            chunk = if remaining < fromIntegral (BS.length bytes) then BS.take (fromIntegral remaining) bytes else bytes
            transferred = tfTransferred t + fromIntegral (BS.length chunk)
            complete = not (BS.null chunk) && (transferred >= tfSize t || BS.length chunk /= maxFileDataSize)
        setTransfer pk (Incoming n) $ if complete || BS.null chunk then Nothing else Just t { tfTransferred = transferred }
        return $ FileChunkReceived pk n (tfTransferred t) chunk : [ FileChunkReceived pk n transferred BS.empty | complete ]
      _ -> return []
\end{code}

\chapter{Messenger}

Messenger is the module at the top of all the other modules.  It sits on top of
\texttt{friend\_connection} in the hierarchy of toxcore.

Messenger takes care of sending and receiving messages using the connection
provided by \texttt{friend\_connection}.  The module provides a way for friends
to connect and makes it usable as an instant messenger.  For example, Messenger
lets users set a nickname and status message which it then transmits to friends
when they are online.  It also allows users to send messages to friends and
builds an instant messenging system on top of the lower level
\texttt{friend\_connection} module.

Messenger offers two methods to add a friend.  The first way is to add a friend
with only their long term public key, this is used when a friend needs to be
added but for some reason a friend request should not be sent.  The friend
should only be added.  This method is most commonly used to accept friend
requests but could also be used in other ways.  If two friends add each other
using this function they will connect to each other.  Adding a friend using
this method just adds the friend to \texttt{friend\_connection} and creates a
new friend entry in Messenger for the friend.

The Tox ID is used to identify peers so that they can be added as friends in
Tox.  In order to add a friend, a Tox user must have the friend's Tox ID. The
Tox ID contains the long term public key of the peer (32 bytes) followed by the
4 byte nospam (see: \texttt{friend\_requests}) value and a 2 byte XOR checksum.
The method of sending the Tox ID to others is up to the user and the client but
the recommended way is to encode it in hexadecimal format and have the user
manually send it to the friend using another program.

Tox ID:

\begin{figure}
\includegraphics{res/images/tox-id.png}
\caption{Tox ID}
\end{figure}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{32}        & long term public key \\
  \texttt{4}         & nospam \\
  \texttt{2}         & checksum \\
\end{tabular}

The checksum is calculated by XORing the first two bytes of the ID with the
next two bytes, then the next two bytes until all the 36 bytes have been XORed
together.  The result is then appended to the end to form the Tox ID.

The user must make sure the Tox ID is not intercepted and replaced in transit
by a different Tox ID, which would mean the friend would connect to a malicious
person instead of the user, though taking reasonable precautions as this is
outside the scope of Tox.  Tox assumes that the user has ensured that they are
using the correct Tox ID, belonging to the intended person, to add a friend.

The second method to add a friend is by using their Tox ID and a message to be
sent in a friend request.  This way of adding friends will try to send a friend
request, with the set message, to the peer whose Tox ID was added.  The method
is similar to the first one, except that a friend request is crafted and sent
to the other peer.

When a friend connection associated to a Messenger friend goes online, a ONLINE
packet will be sent to them.  Friends are only set as online if an ONLINE
packet is received.

As soon as a friend goes online, Messenger will stop sending friend requests to
that friend, if it was sending them, as they are redundant for this friend.

Friends will be set as offline if either the friend connection associated to
them goes offline or if an OFFLINE packet is received from the friend.

Messenger packets are sent to the friend using the online friend connection to
the friend.

Should Messenger need to check whether any of the non lossy packets in the
following list were received by the friend, for example to implement receipts
for text messages, \texttt{net\_crypto} can be used.  The \texttt{net\_crypto}
packet number, used to send the packets, should be noted and then
\texttt{net\_crypto} checked later to see if the bottom of the send array is
after this packet number.  If it is, then the friend has received them.  Note
that \texttt{net\_crypto} packet numbers could overflow after a long time, so
checks should happen within 2**32 \texttt{net\_crypto} packets sent with the
same friend connection.

Message receipts for action messages and normal text messages are implemented
by adding the \texttt{net\_crypto} packet number of each message, along with the
receipt number, to the top of a linked list that each friend has as they are
sent.  Every Messenger loop, the entries are read from the bottom and entries
are removed and passed to the client until an entry that refers to a packet not
yet received by the other is reached, when this happens it stops.

List of Messenger packets:

\section{\texttt{ONLINE}}

length: 1 byte

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x18) \\
\end{tabular}

Sent to a friend when a connection is established to tell them to mark us as
online in their friends list.  This packet and the OFFLINE packet are necessary
as \texttt{friend\_connections} can be established with non-friends who are part
of a groupchat.  The two packets are used to differentiate between these peers,
connected to the user through groupchats, and actual friends who ought to be
marked as online in the friendlist.

On receiving this packet, Messenger will show the peer as being online.

\section{\texttt{OFFLINE}}

length: 1 byte

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x19) \\
\end{tabular}

Sent to a friend when deleting the friend.  Prevents a deleted friend from
seeing us as online if we are connected to them because of a group chat.

On receiving this packet, Messenger will show this peer as offline.

\section{\texttt{NICKNAME}}

length: 1 byte to 129 bytes.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x30) \\
  \texttt{[0, 128]}  & Nickname as a UTF8 byte string \\
\end{tabular}

Used to send the nickname of the peer to others.  This packet should be sent
every time to each friend every time they come online and each time the
nickname is changed.

\section{\texttt{STATUSMESSAGE}}

length: 1 byte to 1008 bytes.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x31) \\
  \texttt{[0, 1007]} & Status message as a UTF8 byte string \\
\end{tabular}

Used to send the status message of the peer to others.  This packet should be
sent every time to each friend every time they come online and each time the
status message is changed.

\section{\texttt{USERSTATUS}}

length: 2 bytes

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x32) \\
  \texttt{1}         & \texttt{uint8\_t} status (0 = online, 1 = away, 2 = busy) \\
\end{tabular}

Used to send the user status of the peer to others.  This packet should be sent
every time to each friend every time they come online and each time the user
status is changed.

\section{\texttt{TYPING}}

length: 2 bytes

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x33) \\
  \texttt{1}         & \texttt{uint8\_t} typing status (0 = not typing, 1 = typing) \\
\end{tabular}

Used to tell a friend whether the user is currently typing or not.

\section{\texttt{MESSAGE}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x40) \\
  \texttt{[0, 1372]} & Message as a UTF8 byte string \\
\end{tabular}

Used to send a normal text message to the friend.

\section{\texttt{ACTION}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x41) \\
  \texttt{[0, 1372]} & Action message as a UTF8 byte string \\
\end{tabular}

Used to send an action message (like an IRC action) to the friend.

\section{\texttt{MSI}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x45) \\
  \texttt{?}         & data \\
\end{tabular}

Reserved for Tox AV usage.

\section{File Transfer Related Packets}

\subsection{\texttt{FILE\_SENDREQUEST}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x50) \\
  \texttt{1}         & \texttt{uint8\_t} file number \\
  \texttt{4}         & \texttt{uint32\_t} file type \\
  \texttt{8}         & \texttt{uint64\_t} file size \\
  \texttt{32}        & file id (32 bytes) \\
  \texttt{[0, 255]}  & filename as a UTF8 byte string \\
\end{tabular}

Note that file type and file size are sent in big endian/network byte format.

\subsection{\texttt{FILE\_CONTROL}}

length: 4 bytes if \texttt{control\_type} isn't seek.  12 bytes if
\texttt{control\_type} is seek.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x51) \\
  \texttt{1}         & \texttt{uint8\_t} \texttt{send\_receive} \\
  \texttt{1}         & \texttt{uint8\_t} file number \\
  \texttt{1}         & \texttt{uint8\_t} \texttt{control\_type} \\
  \texttt{8}         & \texttt{uint64\_t} seek parameter \\
\end{tabular}

\texttt{send\_receive} is 0 if the control targets a file being sent (by the
peer sending the file control), and 1 if it targets a file being received.

\texttt{control\_type} can be one of: 0 = accept, 1 = pause, 2 = kill, 3 = seek.

The seek parameter is only included when \texttt{control\_type} is seek (3).

Note that if it is included the seek parameter will be sent in big
endian/network byte format.

\subsection{\texttt{FILE\_DATA}}

length: 2 to 1373 bytes.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x52) \\
  \texttt{1}         & \texttt{uint8\_t} file number \\
  \texttt{[0, 1371]} & file data piece \\
\end{tabular}

Files are transferred in Tox using File transfers.

To initiate a file transfer, the friend creates and sends a
\texttt{FILE\_SENDREQUEST} packet to the friend it wants to initiate a file
transfer to.

The first part of the \texttt{FILE\_SENDREQUEST} packet is the file number.  The
file number is the number used to identify this file transfer.  As the file
number is represented by a 1 byte number, the maximum amount of concurrent
files Tox can send to a friend is 256.  256 file transfers per friend is enough
that clients can use tricks like queueing files if there are more files needing
to be sent.

256 outgoing files per friend means that there is a maximum of 512 concurrent
file transfers, between two users, if both incoming and outgoing file transfers
are counted together.

As file numbers are used to identify the file transfer, the Tox instance must
make sure to use a file number that isn't used for another outgoing file
transfer to that same friend when creating a new outgoing file transfer.  File
numbers are chosen by the file sender and stay unchanged for the entire
duration of the file transfer.  The file number is used by both
\texttt{FILE\_CONTROL} and \texttt{FILE\_DATA} packets to identify which file
transfer these packets are for.

The second part of the file transfer request is the file type.  This is simply
a number that identifies the type of file.  for example, tox.h defines the file
type 0 as being a normal file and type 1 as being an avatar meaning the Tox
client should use that file as an avatar.  The file type does not effect in any
way how the file is transfered or the behavior of the file transfer.  It is set
by the Tox client that creates the file transfers and send to the friend
untouched.

The file size indicates the total size of the file that will be transfered.  A
file size of \texttt{UINT64\_MAX} (maximum value in a \texttt{uint64\_t}) means
that the size of the file is undetermined or unknown.  For example if someone
wanted to use Tox file transfers to stream data they would set the file size to
\texttt{UINT64\_MAX}.  A file size of 0 is valid and behaves exactly like a
normal file transfer.

The file id is 32 bytes that can be used to uniquely identify the file
transfer.  For example, avatar transfers use it as the hash of the avatar so
that the receiver can check if they already have the avatar for a friend which
saves bandwidth.  It is also used to identify broken file transfers across
toxcore restarts (for more info see the file transfer section of tox.h).  The
file transfer implementation does not care about what the file id is, as it is
only used by things above it.

The last part of the file transfer is the optional file name which is used to
tell the receiver the name of the file.

When a \texttt{FILE\_SENDREQUEST} packet is received, the implementation
validates and sends the info to the Tox client which decides whether they
should accept the file transfer or not.

To refuse or cancel a file transfer, they will send a \texttt{FILE\_CONTROL}
packet with \texttt{control\_type} 2 (kill).

\texttt{FILE\_CONTROL} packets are used to control the file transfer.
\texttt{FILE\_CONTROL} packets are used to accept/unpause, pause, kill/cancel
and seek file transfers.  The \texttt{control\_type} parameter denotes what the
file control packet does.

The \texttt{send\_receive} and file number are used to identify a specific file
transfer.  Since file numbers for outgoing and incoming files are not related
to each other, the \texttt{send\_receive} parameter is used to identify if the
file number belongs to files being sent or files being received.  If
\texttt{send\_receive} is 0, the file number corresponds to a file being sent by
the user sending the file control packet.  If \texttt{send\_receive} is 1, it
corresponds to a file being received by the user sending the file control
packet.

\texttt{control\_type} indicates the purpose of the \texttt{FILE\_CONTROL}
packet.  \texttt{control\_type} of 0 means that the \texttt{FILE\_CONTROL} packet
is used to tell the friend that the file transfer is accepted or that we are
unpausing a previously paused (by us) file transfer.  \texttt{control\_type} of
1 is used to tell the other to pause the file transfer.

If one party pauses a file transfer, that party must be the one to unpause it.
Should both sides pause a file transfer, both sides must unpause it before the
file can be resumed.  For example, if the sender pauses the file transfer, the
receiver must not be able to unpause it.  To unpause a file transfer,
\texttt{control\_type} 0 is used.  Files can only be paused when they are in
progress and have been accepted.

\texttt{control\_type} 2 is used to kill, cancel or refuse a file transfer.
When a \texttt{FILE\_CONTROL} is received, the targeted file transfer is
considered dead, will immediately be wiped and its file number can be reused.
The peer sending the \texttt{FILE\_CONTROL} must also wipe the targeted file
transfer from their side.  This control type can be used by both sides of the
transfer at any time.

\texttt{control\_type} 3, the seek control type is used to tell the sender of
the file to start sending from a different index in the file than 0.  It can
only be used right after receiving a \texttt{FILE\_SENDREQUEST} packet and
before accepting the file by sending a \texttt{FILE\_CONTROL} with
\texttt{control\_type} 0.  When this \texttt{control\_type} is used, an extra 8
byte number in big endian format is appended to the \texttt{FILE\_CONTROL} that
is not present with other control types.  This number indicates the index in
bytes from the beginning of the file at which the file sender should start
sending the file.  The goal of this control type is to ensure that files can be
resumed across core restarts.  Tox clients can know if they have received a
part of a file by using the file id and then using this packet to tell the
other side to start sending from the last received byte.  If the seek position
is bigger or equal to the size of the file, the seek packet is invalid and the
one receiving it will discard it.

To accept a file Tox will therefore send a seek packet, if it is needed, and
then send a \texttt{FILE\_CONTROL} packet with \texttt{control\_type} 0 (accept)
to tell the file sender that the file was accepted.

Once the file transfer is accepted, the file sender will start sending file
data in sequential chunks from the beginning of the file (or the position from
the \texttt{FILE\_CONTROL} seek packet if one was received).

File data is sent using \texttt{FILE\_DATA} packets.  The file number
corresponds to the file transfer that the file chunks belong to.  The receiver
assumes that the file transfer is over as soon as a chunk with the file data
size not equal to the maximum size (1371 bytes) is received.  This is how the
sender tells the receiver that the file transfer is complete in file transfers
where the size of the file is unknown (set to \texttt{UINT64\_MAX}).  The
receiver also assumes that if the amount of received data equals to the file
size received in the \texttt{FILE\_SENDREQUEST}, the file sending is finished
and has been successfully received.  Immediately after this occurs, the
receiver frees up the file number so that a new incoming file transfer can use
that file number.  The implementation should discard any extra data received
which is larger than the file size received at the beginning.

In 0 filesize file transfers, the sender will send one \texttt{FILE\_DATA}
packet with a file data size of 0.

The sender will know if the receiver has received the file successfully by
checking if the friend has received the last \texttt{FILE\_DATA} packet sent
(containing the last chunk of the file).  \texttt{net\_crypto} can be used to
check whether packets sent through it have been received by storing the packet
number of the sent packet and verifying later in \texttt{net\_crypto} to see
whether it was received or not.  As soon as \texttt{net\_crypto} says the other
received the packet, the file transfer is considered successful, wiped and the
file number can be reused to send new files.

\texttt{FILE\_DATA} packets should be sent as fast as the \texttt{net\_crypto}
connection can handle it respecting its congestion control.

If the friend goes offline, all file transfers are cleared in toxcore.  This
makes it simpler for toxcore as it does not have to deal with resuming file
transfers.  It also makes it simpler for clients as the method for resuming
file transfers remains the same, even if the client is restarted or toxcore
loses the connection to the friend because of a bad internet connection.

\section{Group Chat Related Packets}

\begin{tabular}{l|l}
  Packet ID & Packet Name \\
  \hline
  0x60      & \texttt{INVITE\_GROUPCHAT} \\
  0x61      & \texttt{ONLINE\_PACKET} \\
  0x62      & \texttt{DIRECT\_GROUPCHAT} \\
  0x63      & \texttt{MESSAGE\_GROUPCHAT} \\
  0xC7      & \texttt{LOSSY\_GROUPCHAT} \\
\end{tabular}

Messenger also takes care of saving the friends list and other friend
information so that it's possible to close and start toxcore while keeping all
your friends, your long term key and the information necessary to reconnect to
the network.

Important information messenger stores includes: the long term private key, our
current nospam value, our friends' public keys and any friend requests the user
is currently sending.  The network DHT nodes, TCP relays and some onion nodes
are stored to aid reconnection.

In addition to this, a lot of optional data can be stored such as the usernames
of friends, our current username, status messages of friends, our status
message, etc... can be stored.  The exact format of the toxcore save is
explained later.

The TCP server is run from the toxcore messenger module if the client has
enabled it.  TCP server is usually run independently as part of the bootstrap
node package but it can be enabled in clients.  If it is enabled in toxcore,
Messenger will add the running TCP server to the TCP relay.

Messenger is the module that transforms code that can connect to friends based
on public key into a real instant messenger.

\chapter{Friend requests}

When a Tox user adds someone with Tox, toxcore will try sending a friend
request to that person.  A friend request contains the long term public key of
the sender, a nospam number and a message.

Transmitting the long term public key is the primary goal of the friend request
as it is what the peer needs to find and establish a connection to the sender.
The long term public key is what the receiver adds to his friends list if he
accepts the friend request.

The nospam is a number used to prevent someone from spamming the network with
valid friend requests.  It makes sure that the only people who have seen the
Tox ID of a peer are capable of sending them a friend request.  The nospam is
one of the components of the Tox ID.

The nospam is a number or a list of numbers set by the peer, only received
friend requests that contain a nospam that was set by the peer are sent to the
client to be accepted or refused by the user.  The nospam prevents random peers
in the network from sending friend requests to non friends.  The nospam is not
long enough to be secure meaning an extremely resilient attacker could manage
to send a spam friend request to someone.  4 bytes is large enough to prevent
spam from random peers in the network.  The nospam could also allow Tox users
to issue different Tox IDs and even change Tox IDs if someone finds a Tox ID
and decides to send it hundreds of spam friend requests.  Changing the nospam
would stop the incoming wave of spam friend requests without any negative
effects to the users friends list.  For example if users would have to change
their public key to prevent them from receiving friend requests it would mean
they would have to essentially abandon all their current friends as friends are
tied to the public key.  The nospam is not used at all once the friends have
each other added which means changing it won't have any negative effects.

Friend request:

\begin{verbatim}
[uint32_t nospam][Message (UTF8) 1 to ONION_CLIENT_MAX_DATA_SIZE bytes]
\end{verbatim}

Friend request packet when sent as an onion data packet:

\begin{verbatim}
[uint8_t (32)][Friend request]
\end{verbatim}

Friend request packet when sent as a \texttt{net\_crypto} data packet (If we are
directly connected to the peer because of a group chat but are not friends with
them):

\begin{verbatim}
[uint8_t (18)][Friend request]
\end{verbatim}

When a friend is added to toxcore with their Tox ID and a message, the friend
is added in \texttt{friend\_connection} and then toxcore tries to send friend
requests.

When sending a friend request, toxcore will check if the peer which a friend
request is being sent to is already connected to using a \texttt{net\_crypto}
connection which can happen if both are in the same group chat.  If this is the
case the friend request will be sent as a \texttt{net\_crypto} packet using that
connection.  If not, it will be sent as an onion data packet.

Onion data packets contain the real public key of the sender and if a
\texttt{net\_crypto} connection is established it means the peer knows our real
public key.  This is why the friend request does not need to contain the real
public key of the peer.

Friend requests are sent with exponentially increasing interval of 2 seconds, 4
seconds, 8 seconds, etc... in toxcore.  This is so friend requests get resent
but eventually get resent in intervals that are so big that they essentially
expire.  The sender has no way of knowing if a peer refuses a friend requests
which is why friend requests need to expire in some way.  Note that the
interval is the minimum timeout, if toxcore cannot send that friend request it
will try again until it manages to send it.  One reason for not being able to
send the friend request would be that the onion has not found the friend in the
onion and so cannot send an onion data packet to them.

Received friend requests are passed to the client, the client is expected to
show the message from the friend request to the user and ask the user if they
want to accept the friend request or not.  Friend requests are accepted by
adding the peer sending the friend request as a friend and refused by simply
ignoring it.

Friend requests are sent multiple times meaning that in order to prevent the
same friend request from being sent to the client multiple times toxcore keeps
a list of the last real public keys it received friend requests from and
discards any received friend requests that are from a real public key that is
in that list.  In toxcore this list is a simple circular list.  There are many
ways this could be improved and made more efficient as a circular list isn't
very efficient however it has worked well in toxcore so far.

Friend requests from public keys that are already added to the friends list
should also be discarded.
