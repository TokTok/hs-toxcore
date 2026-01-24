\section{Onion Operation}

\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Tox.Onion.Operation where

import           Control.Monad                      (forM_, replicateM, unless,
                                                     when)
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.State                (StateT, runStateT)
import qualified Crypto.Saltine.Class               as Sodium
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Get                    as Get
import qualified Data.Binary.Put                    as Put
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Function                      (on)
import           Data.List                          (foldl', nubBy, sortOn)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust, fromMaybe,
                                                     isJust)
import           Data.Text                          (pack)
import           Data.Word                          (Word32, Word64, Word8)

import qualified Tox.Core.PingArray                 as PingArray
import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (TimeDiff, Timestamp)
import           Tox.Core.Timed                     (Timed (..))
import qualified Tox.Crypto.Core.Box                as Box
import           Tox.Crypto.Core.Box                (PlainText (..))
import qualified Tox.Crypto.Core.Hash               as Hash
import           Tox.Crypto.Core.Key                (CombinedKey, Key (..),
                                                     PublicKey, unKey)
import           Tox.Crypto.Core.Keyed              (Keyed (..))
import qualified Tox.Crypto.Core.KeyPair            as KeyPair
import           Tox.Crypto.Core.KeyPair            (KeyPair)
import           Tox.Crypto.Core.MonadRandomBytes   (MonadRandomBytes (..),
                                                     randomKey, randomNonce,
                                                     randomWord64)
import qualified Tox.DHT.DhtPacket                  as DhtPacket
import           Tox.DHT.DhtRequestPacket           (DhtRequestPacket (..))
import qualified Tox.DHT.Distance                   as Distance
import           Tox.Network.Core.Networked         (Networked (..))
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..),
                                                     RawPayload (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import qualified Tox.Network.Core.SocketAddress     as SocketAddress
import           Tox.Network.Core.SocketAddress     (SocketAddress)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))
import qualified Tox.Onion.Path                     as Path
import           Tox.Onion.Path                     (OnionPath, OnionPathState)
import qualified Tox.Onion.RPC                      as RPC
import qualified Tox.Onion.Tunnel                   as Tunnel


{-------------------------------------------------------------------------------
 -
 - :: Constants.
 -
 ------------------------------------------------------------------------------}

-- | Nodes we announce ourselves to.
maxAnnounceNodes :: Int
maxAnnounceNodes = 12

-- | Nodes we keep asking about each friend.
maxFriendNodes :: Int
maxFriendNodes = 8

-- | Nodes put in announce responses and DHT public key packets.
maxSentNodes :: Int
maxSentNodes = 4

-- | Announce interval for nodes that do and do not store our entry yet.
announceIntervalStored, announceIntervalUnstored :: TimeDiff
announceIntervalStored   = Time.seconds 15
announceIntervalUnstored = Time.seconds 3

-- | Nodes that missed this many responses in a row are not asked again, and
-- are dropped once 'onionNodeTimeout' has passed too.
maxUnansweredRequests :: Int
maxUnansweredRequests = 3

onionNodeTimeout :: TimeDiff
onionNodeTimeout = Time.seconds 15

dhtPkSendInterval, dhtPkDhtSendInterval :: TimeDiff
dhtPkSendInterval = Time.seconds 30
dhtPkDhtSendInterval = Time.seconds 20

-- | How long a ping id and an announcement stay valid.
announceTimeout :: TimeDiff
announceTimeout = Time.seconds 300

maxAnnounceEntries :: Int
maxAnnounceEntries = 160

packetIdDhtPk, packetIdFriendRequest :: Word8
packetIdDhtPk         = 156
packetIdFriendRequest = 32

-- | Request and response lengths without the packet kind.
announceRequestLength, dataRequestMinLength :: Int
announceRequestLength = 24 + 32 + 32 + 32 + 32 + 8 + 16
dataRequestMinLength  = 32 + 24 + 32 + 16


{-------------------------------------------------------------------------------
 -
 - :: State.
 -
 ------------------------------------------------------------------------------}

class (Monad m, Timed m, MonadRandomBytes m, Keyed m, Networked m, MonadLogger m) => OnionNodeMonad m where
  getOnionState :: m OnionState
  putOnionState :: OnionState -> m ()
  getDhtKeyPair :: m KeyPair
  -- | Up to the given number of DHT nodes closest to a key.
  getCloseNodes :: Int -> PublicKey -> m [NodeInfo]
  -- | As 'getCloseNodes', to tell a requester at the address about.
  getCloseNodesFor :: SocketAddress -> Int -> PublicKey -> m [NodeInfo]
  -- | TCP relays we are connected to.
  getTcpRelays :: m [NodeInfo]
  getTcpRelays = return []
  -- | Send an onion request to a relay that is the first node of its path.
  sendOnionViaRelay :: NodeInfo -> BS.ByteString -> m ()
  sendOnionViaRelay _ _ = return ()
  -- | Send an onion response to a client of our TCP relay.
  sendToTcpClient :: Word64 -> BS.ByteString -> m ()
  sendToTcpClient _ _ = return ()
  -- | Send a DHT Request to a node through the nodes close to it: how many
  -- nodes it went through.
  sendDhtRequest :: PublicKey -> BS.ByteString -> m Int
  sendDhtRequest _ _ = return 0

getsOnion :: OnionNodeMonad m => (OnionState -> a) -> m a
getsOnion f = f <$> getOnionState

modifyOnion :: OnionNodeMonad m => (OnionState -> OnionState) -> m ()
modifyOnion f = getOnionState >>= putOnionState . f

instance OnionNodeMonad m => Path.OnionPathMonad (StateT OnionPathState m)

zoomOnionPath :: OnionNodeMonad m => StateT OnionPathState m a -> m a
zoomOnionPath st = do
  s <- getOnionState
  (a, s') <- runStateT st (onionPaths s)
  putOnionState s { onionPaths = s' }
  return a

-- | What an announce node told us about the key we announced or searched for.
data AnnounceStatus
  = NotStored              -- ^ It gave us a ping id to announce with.
  | StoresFriend PublicKey -- ^ It stores the friend, with their data public key.
  | StoresUs               -- ^ It stores our announcement.
  deriving (Eq, Show)

-- | A node we announce ourselves to or ask about a friend.
data OnionNode = OnionNode
  { onNode       :: NodeInfo
  , onStatus     :: AnnounceStatus
  , onPingId     :: Maybe PublicKey
  , onPathNum    :: Maybe Word32
  , onLastSent   :: Maybe Timestamp
  , onUnanswered :: Int
  , onAnswered   :: Bool -- ^ It answered us at least once.
  } deriving (Eq, Show)

-- | Who an announce request is for: us, a friend, or a group we look for
-- under a random search key.
data Target = Self | Friend PublicKey | GroupSearch PublicKey
  deriving (Eq, Ord, Show)

-- | Looking for the peers of a group: the onion nodes near a random search
-- key store and return group announcements by chat id.
data OnionGroup = OnionGroup
  { ogTempKeys :: KeyPair
  , ogNodes    :: Map PublicKey OnionNode
  , ogChatId   :: BS.ByteString
  , ogAnnounce :: Maybe BS.ByteString -- ^ How to reach us in the group, sent with each request.
  } deriving (Eq, Show)

-- | Group announcements we store for others: the newest 16 for each chat id.
data StoredGroup = StoredGroup
  { sgAnnounces :: [(PublicKey, BS.ByteString)]
  , sgLastSeen  :: Timestamp
  } deriving (Eq, Show)

maxOnionPacketSize :: Int
maxOnionPacketSize = 1400

-- | How long the key sealing return data is used: as in toxcore, return data
-- older than that stops working.
returnKeyLifetime :: TimeDiff
returnKeyLifetime = Time.seconds (2 * 60 * 60)

maxStoredAnnounces, maxSentAnnounces, maxStoredGroups, maxExtraDataSize :: Int
maxStoredAnnounces = 16
maxSentAnnounces   = 4
maxStoredGroups    = 512
-- | The most an announce request carries after its own fields: a chat id
-- and an announcement.
maxExtraDataSize   = 136

-- | An announce request's own fields: ping id, search key, data key and
-- sendback data.
announceRequestSize :: Int
announceRequestSize = 32 + 32 + 32 + 8

storedGroupTimeout :: TimeDiff
storedGroupTimeout = Time.seconds 60

data OnionFriend = OnionFriend
  { ofTempKeys         :: KeyPair -- ^ Keeps searches unlinkable to us.
  , ofNodes            :: Map PublicKey OnionNode
  , ofLastDhtPkSent    :: Maybe Timestamp
  , ofLastDhtPkDhtSent :: Maybe Timestamp -- ^ Sent through the DHT.
  , ofDhtPk            :: Maybe PublicKey
  , ofLastNoReplay     :: Word64
  , ofOnline           :: Bool -- ^ Connected, so no need to look for them.
  , ofPings            :: Int  -- ^ Requests since the friend was last online.
  , ofLastPinged       :: Maybe Timestamp
  } deriving (Eq, Show)

-- | How often we ask each node about a friend: often at first, then less and
-- less while they stay away, as toxcore does. After the first rounds the
-- requests are spread out over the nodes too.
friendInterval :: Int -> TimeDiff
friendInterval pings
  | newFriend pings = Time.seconds 3
  | otherwise = Time.seconds . min 3600 $ 2 ^ min 12 (friendRounds pings - 2)

friendRounds :: Int -> Int
friendRounds pings = pings `div` (maxFriendNodes `div` 2)

newFriend :: Int -> Bool
newFriend = (<= 5) . friendRounds

data Sendback = Sendback
  { sbTarget  :: Target
  , sbNode    :: NodeInfo
  , sbPathNum :: Word32
  } deriving (Eq, Show)

-- | An announcement stored by us as an announce node.
data AnnounceEntry = AnnounceEntry
  { aeDataPk  :: PublicKey
  , aeAddress :: SocketAddress     -- ^ Last relay of the announcing path.
  , aeReturn  :: BS.ByteString     -- ^ Return data to reach the announcer.
  , aeTime    :: Timestamp
  } deriving (Eq, Show)

data OnionState = OnionState
  { onionPaths      :: OnionPathState
  , ourRealKeys     :: KeyPair
  , ourDataKeys     :: KeyPair -- ^ Others encrypt onion data to us with this.
  , selfNodes       :: Map PublicKey OnionNode
  , onionFriends    :: Map PublicKey OnionFriend
  , sendbacks       :: PingArray.PingArray Sendback
  , returnKey       :: CombinedKey -- ^ Seals return data when relaying.
  , returnKeySince  :: Maybe Timestamp
  , pingIdSecret    :: BS.ByteString
  , announceEntries :: Map PublicKey AnnounceEntry
  , pathNodes       :: [NodeInfo] -- ^ Nodes from announce responses to build paths from, newest first.
  , bootstrapNodes  :: [NodeInfo] -- ^ As 'pathNodes', from bootstrapping and saves.
  , onionTcpOnly    :: Bool -- ^ Without UDP, paths start at a TCP relay.
  , onionConnected  :: Int  -- ^ Rises while we are announced, falls while not.
  , onionGroups     :: Map PublicKey OnionGroup -- ^ By search key.
  , storedGroups    :: Map BS.ByteString StoredGroup -- ^ By chat id.
  }

-- | What the onion reports to the friend connections.
data OnionEvent
  = FriendDhtPkFound PublicKey PublicKey [NodeInfo] -- ^ Friend, its DHT key and nodes near it.
  | FriendRequestReceived PublicKey BS.ByteString    -- ^ Sender and the request after its packet id.
  | GroupAnnouncesFound BS.ByteString BS.ByteString -- ^ Chat id and packed announcements of its peers.
  deriving (Eq, Show)

initState :: MonadRandomBytes m => KeyPair -> m OnionState
initState realKeys = do
  dataKeys <- newKeyPair
  retKey <- randomKey
  secret <- randomBytes 32
  return OnionState
    { onionPaths      = Path.OnionPathState [] [] 0
    , ourRealKeys     = realKeys
    , ourDataKeys     = dataKeys
    , selfNodes       = Map.empty
    , onionFriends    = Map.empty
    , sendbacks       = PingArray.empty 1024 (Time.seconds 10)
    , returnKey       = retKey
    , returnKeySince  = Nothing
    , pingIdSecret    = secret
    , announceEntries = Map.empty
    , pathNodes       = []
    , bootstrapNodes  = []
    , onionTcpOnly    = False
    , onionConnected  = 0
    , onionGroups     = Map.empty
    , storedGroups    = Map.empty
    }

maxPathNodes :: Int
maxPathNodes = 32

-- | Newer nodes replace the oldest, as in toxcore's ring buffers.
addNewest :: [NodeInfo] -> [NodeInfo] -> [NodeInfo]
addNewest nodes pool = take maxPathNodes $ foldl' add pool nodes
  where
    add acc n
      | NodeInfo.protocol n /= UDP || NodeInfo.publicKey n `elem` map NodeInfo.publicKey acc = acc
      | otherwise = n : acc

addPathNodes, addBootstrapNodes :: OnionNodeMonad m => [NodeInfo] -> m ()
addPathNodes nodes = modifyOnion $ \s -> s { pathNodes = addNewest nodes (pathNodes s) }
addBootstrapNodes nodes = modifyOnion $ \s -> s { bootstrapNodes = addNewest nodes (bootstrapNodes s) }

-- | Nodes to build paths from besides the DHT's.
pathPool :: OnionState -> [NodeInfo]
pathPool s = pathNodes s ++ filter ((`notElem` map NodeInfo.publicKey (pathNodes s)) . NodeInfo.publicKey) (bootstrapNodes s)

-- | DHT nodes have answered us, so path nodes from elsewhere, such as
-- bootstrap nodes we may not reach, only make up for too few of them.
withPool :: Int -> [NodeInfo] -> [NodeInfo] -> [NodeInfo]
withPool n close pool
  | length close >= n = close
  | otherwise         = close ++ pool

zeroKey :: PublicKey
zeroKey = Key . fromJust . Sodium.decode $ BS.replicate 32 0

sendTo :: OnionNodeMonad m => SocketAddress -> Packet RawPayload -> m ()
sendTo addr = sendPacket (NodeInfo UDP addr zeroKey)

encodeStrict :: Binary.Binary a => a -> BS.ByteString
encodeStrict = LBS.toStrict . Binary.encode

decodeStrict :: Binary.Binary a => BS.ByteString -> Maybe a
decodeStrict = Box.decode . PlainText


{-------------------------------------------------------------------------------
 -
 - :: Announcing and searching.
 -
 ------------------------------------------------------------------------------}

-- | Start looking for a friend, if we are not already.
startFriendSearch :: OnionNodeMonad m => PublicKey -> m ()
startFriendSearch friendPk = do
  known <- getsOnion $ Map.member friendPk . onionFriends
  unless known $ do
    tempKeys <- newKeyPair
    modifyOnion $ \s -> s { onionFriends = Map.insert friendPk (OnionFriend tempKeys Map.empty Nothing Nothing Nothing 0 False 0 Nothing) (onionFriends s) }

-- | Periodic work: keep paths, announce ourselves, search for friends and
-- tell found friends our DHT public key.
doOnion :: OnionNodeMonad m => m ()
doOnion = do
  now' <- askTime
  since <- getsOnion returnKeySince
  case since of
    Nothing -> modifyOnion $ \s -> s { returnKeySince = Just now' }
    Just t | Time.addTime t returnKeyLifetime <= now' -> do
      key <- randomKey
      modifyOnion $ \s -> s { returnKey = key, returnKeySince = Just now' }
    _ -> return ()
  ourDhtPk <- KeyPair.publicKey <$> getDhtKeyPair
  close <- getCloseNodes 32 ourDhtPk
  pool <- getsOnion pathPool
  tcpOnly <- getsOnion onionTcpOnly
  -- Without DHT nodes UDP does not work, so the onion goes through relays.
  if tcpOnly || null close
  then getTcpRelays >>= \relays -> zoomOnionPath $ Path.maintainPathsVia relays (withPool maxAnnounceNodes close pool)
  else zoomOnionPath $ Path.maintainPaths (withPool maxAnnounceNodes close pool)
  announce Self
  now <- askTime
  modifyOnion $ \s -> s { onionConnected = if isAnnounced now s then min (2 * connectedRuns) (onionConnected s + 1) else max 0 (onionConnected s - 1) }
  friendPks <- getsOnion $ Map.keys . onionFriends
  forM_ friendPks $ \pk -> do
    online <- getsOnion $ maybe False ofOnline . Map.lookup pk . onionFriends
    unless online $ do
      announce (Friend pk)
      sendDhtPkIfDue pk
  groupKeys <- getsOnion $ Map.keys . onionGroups
  mapM_ (announce . GroupSearch) groupKeys
  modifyOnion $ \s -> s { storedGroups = Map.filter (\g -> Time.addTime (sgLastSeen g) storedGroupTimeout > now') (storedGroups s) }

removeFriend :: OnionNodeMonad m => PublicKey -> m ()
removeFriend pk = modifyOnion $ \s -> s { onionFriends = Map.delete pk (onionFriends s) }

-- | How many 'doOnion' runs we must be announced in to be online.  Toxcore
-- waits 3 seconds and goes offline as slowly.
connectedRuns :: Int
connectedRuns = 2

isConnected :: OnionState -> Bool
isConnected s = onionConnected s >= connectedRuns

-- | Whether we are announced to at least half the nodes that answer us.
isAnnounced :: Timestamp -> OnionState -> Bool
isAnnounced now s = announced > 0 && length live `div` 2 <= announced
  where
    live = filter (\n -> onAnswered n && not (timedOut now n)) (Map.elems $ selfNodes s)
    announced = length $ filter ((== StoresUs) . onStatus) live

-- | Whether we are connected to the friend.
setFriendOnline :: OnionNodeMonad m => PublicKey -> Bool -> m ()
setFriendOnline pk online =
  modifyOnion $ \s -> s { onionFriends = Map.adjust update pk (onionFriends s) }
  where
    update f = f { ofOnline = online, ofPings = if ofOnline f && not online then 0 else ofPings f }

targetKey :: OnionState -> Target -> PublicKey
targetKey s Self             = KeyPair.publicKey $ ourRealKeys s
targetKey _ (Friend pk)      = pk
targetKey _ (GroupSearch pk) = pk

targetNodes :: OnionState -> Target -> Map PublicKey OnionNode
targetNodes s Self        = selfNodes s
targetNodes s (Friend pk) = maybe Map.empty ofNodes $ Map.lookup pk (onionFriends s)
targetNodes s (GroupSearch pk) = maybe Map.empty ogNodes $ Map.lookup pk (onionGroups s)

setTargetNodes :: Target -> Map PublicKey OnionNode -> OnionState -> OnionState
setTargetNodes Self nodes s = s { selfNodes = nodes }
setTargetNodes (Friend pk) nodes s =
  s { onionFriends = Map.adjust (\f -> f { ofNodes = nodes }) pk (onionFriends s) }
setTargetNodes (GroupSearch pk) nodes s =
  s { onionGroups = Map.adjust (\g -> g { ogNodes = nodes }) pk (onionGroups s) }

maxTargetNodes :: Target -> Int
maxTargetNodes Self            = maxAnnounceNodes
maxTargetNodes (Friend _)      = maxFriendNodes
maxTargetNodes (GroupSearch _) = maxFriendNodes

-- | A node stopped answering a while ago.
timedOut :: Timestamp -> OnionNode -> Bool
timedOut now n = onUnanswered n >= maxUnansweredRequests
  && maybe True (\t -> Time.addTime t onionNodeTimeout < now) (onLastSent n)

-- | Keep the closest responsive nodes to the target key, at most @limit@.
closestNodes :: Timestamp -> Int -> PublicKey -> Map PublicKey OnionNode -> Map PublicKey OnionNode
closestNodes now limit key =
  Map.fromList . take limit . sortOn (Distance.xorDistance key . fst) . Map.toList
    . Map.filter (not . timedOut now)

newOnionNode :: NodeInfo -> OnionNode
newOnionNode node = OnionNode node NotStored Nothing Nothing Nothing 0 False

-- | Send announce requests to the nodes of a target that are due one.
announce :: OnionNodeMonad m => Target -> m ()
announce target = do
  s <- getOnionState
  let key = targetKey s target
  close <- getCloseNodes (maxTargetNodes target) key
  now <- askTime
  let pool = take (maxTargetNodes target) . sortOn (Distance.xorDistance key . NodeInfo.publicKey) $ pathPool s
      candidates = withPool (maxTargetNodes target) close pool
      known = targetNodes s target
      fresh = Map.fromList [ (NodeInfo.publicKey n, newOnionNode n) | n <- candidates ]
      nodes = closestNodes now (maxTargetNodes target) key (Map.union known fresh)
  modifyOnion $ setTargetNodes target nodes
  forM_ (filter ((< maxUnansweredRequests) . onUnanswered) $ Map.elems nodes) $ \node -> case target of
    Friend pk -> getsOnion (Map.lookup pk . onionFriends) >>= mapM_ (\f -> do
      let interval = friendInterval (ofPings f)
          spaced = newFriend (ofPings f) || Time.due now (Time.milliseconds $ Time.toMilliseconds interval `div` fromIntegral (maxFriendNodes `div` 2)) (ofLastPinged f)
      when (spaced && Time.due now interval (onLastSent node)) $ do
        sendAnnounceRequest target node
        modifyOnion $ \st -> st { onionFriends = Map.adjust (\f' -> f' { ofPings = ofPings f' + 1, ofLastPinged = Just now }) pk (onionFriends st) })
    _ -> do
      let interval = if onStatus node == NotStored then announceIntervalUnstored else announceIntervalStored
      when (Time.due now interval (onLastSent node)) $ sendAnnounceRequest target node

-- | Send an announce request to a node.  A ping id is only valid through the
-- path it was given on, so that path is reused while it lasts.  Searches send
-- no ping id, so the node does not store their temporary key as an
-- announcement.
sendAnnounceRequest :: OnionNodeMonad m => Target -> OnionNode -> m ()
sendAnnounceRequest target node = do
  s <- getOnionState
  samePath <- case (target, onPathNum node) of
    (Self, Just num) -> zoomOnionPath $ Path.findPath num
    _                -> return Nothing
  mPath <- maybe (zoomOnionPath $ Path.selectPath (target == Self)) (return . Just) samePath
  forM_ mPath $ \path -> do
    now <- askTime
    seed <- randomWord64
    let to = onNode node
        sendback = Sendback target to (Path.pathNum path)
        (sendbackId, sendbacks') = PingArray.addEntry now sendback seed (sendbacks s)
        group = case target of
          GroupSearch pk -> Map.lookup pk (onionGroups s)
          _              -> Nothing
        (keys, dataPk) = case target of
          Self     -> (ourRealKeys s, KeyPair.publicKey $ ourDataKeys s)
          Friend pk -> (maybe (ourRealKeys s) ofTempKeys $ Map.lookup pk (onionFriends s), zeroKey)
          GroupSearch _ -> (maybe (ourRealKeys s) ogTempKeys group, zeroKey)
        -- Group requests carry our announcement once we have one.
        extra = group >>= ogAnnounce
        pingId = if isJust samePath then onPingId node else Nothing
        payload = RPC.AnnounceRequestPayload
          { RPC.announceRequestPingId          = fromMaybe zeroKey pingId
          , RPC.announceRequestSearchKey       = targetKey s target
          , RPC.announceRequestDataSendbackKey = dataPk
          , RPC.announceRequestSendbackData    = sendbackId
          }
    nonce <- randomNonce
    shared <- getCombinedKey (KeyPair.secretKey keys) (NodeInfo.publicKey to)
    let plain = Box.PlainText $ encodeStrict payload <> fromMaybe BS.empty extra
        request = Sodium.encode (unKey nonce) <> keyBytes (KeyPair.publicKey keys) <> Box.unCipherText (Box.encrypt shared nonce plain)
    sendThroughPath path (NodeInfo.address to) $
      BS.cons (PacketKind.kindToByte $ if isJust extra then PacketKind.GroupAnnounceRequest else PacketKind.AnnounceRequest) request
    let node' = node { onLastSent = Just now, onUnanswered = onUnanswered node + 1, onPathNum = Just (Path.pathNum path) }
    modifyOnion $ \st -> setTargetNodes target (Map.insert (NodeInfo.publicKey to) node' $ targetNodes st target)
      st { sendbacks = sendbacks' }

-- | Wrap data for the destination in an onion through the path and send it.
sendThroughPath :: OnionNodeMonad m => OnionPath -> SocketAddress -> BS.ByteString -> m ()
sendThroughPath path dest bytes = case Path.pathNodes path of
  nodeA : _ -> do
    nonce <- randomNonce
    if NodeInfo.protocol nodeA == TCP
    then do
      layers <- Path.wrapLayers path dest nonce bytes
      sendOnionViaRelay nodeA $ Sodium.encode (unKey nonce) <> encodeStrict layers
    else do
      request <- Path.wrapPath path dest nonce bytes
      sendPacket nodeA $ Packet PacketKind.OnionRequest0 request
    zoomOnionPath $ Path.markPathUsed (Path.pathNum path)
  [] -> return ()

-- | Handle a response to one of our announce requests.
handleAnnounceResponse :: OnionNodeMonad m => BS.ByteString -> m ()
handleAnnounceResponse payload = do
  s <- getOnionState
  now <- askTime
  case decodeStrict payload of
    Nothing -> return ()
    Just (res :: RPC.AnnounceResponse) -> do
      let (mSendback, sendbacks') = PingArray.takeEntry now (RPC.announceResponseSendbackData res) (sendbacks s)
      putOnionState s { sendbacks = sendbacks' }
      forM_ mSendback $ \(Sendback target from pathNum) -> do
        let keys = case target of
              Self           -> Just $ ourRealKeys s
              Friend pk      -> ofTempKeys <$> Map.lookup pk (onionFriends s)
              GroupSearch pk -> ogTempKeys <$> Map.lookup pk (onionGroups s)
        forM_ keys $ \kp -> do
          shared <- getCombinedKey (KeyPair.secretKey kp) (NodeInfo.publicKey from)
          let decrypted = Box.decrypt shared (RPC.announceResponseNonce res) (RPC.announceResponseEncryptedPayload res)
          forM_ (decrypted >>= Box.decode) $ \(plain :: RPC.AnnounceResponsePayload) ->
            handleAnnounceResult target from pathNum plain

handleAnnounceResult :: OnionNodeMonad m => Target -> NodeInfo -> Word32 -> RPC.AnnounceResponsePayload -> m ()
handleAnnounceResult target from pathNum plain = do
  zoomOnionPath $ Path.markPathConfirmed pathNum
  let pingIdOrKey = RPC.announceResponsePingId plain
      (status, pingId) = case RPC.announceResponseIsStored plain of
        1 -> (StoresFriend pingIdOrKey, Nothing)
        2 -> (StoresUs, Just pingIdOrKey)
        _ -> (NotStored, Just pingIdOrKey)
  s <- getOnionState
  now <- askTime
  let key = targetKey s target
      old = Map.findWithDefault (newOnionNode from) (NodeInfo.publicKey from) (targetNodes s target)
      node = old { onStatus = status, onPingId = pingId, onPathNum = Just pathNum, onUnanswered = 0, onAnswered = True }
      -- As in toxcore, only a node on a LAN may tell us about LAN nodes.
      offered = [ n | n <- RPC.announceResponseNodes plain
                    , SocketAddress.isLan (NodeInfo.address from) || not (SocketAddress.isLan $ NodeInfo.address n) ]
      found = Map.fromList [ (NodeInfo.publicKey n, newOnionNode n) | n <- offered ]
      nodes = closestNodes now (maxTargetNodes target) key $
        Map.insert (NodeInfo.publicKey from) node (targetNodes s target) `Map.union` found
  -- Paths are built from nodes that worked, not from ones we were told of.
  path <- zoomOnionPath $ Path.findPath pathNum
  addPathNodes $ from : maybe [] Path.pathNodes path
  when (status /= onStatus old) $ logDebugN $ "Onion node " <> pack (show $ NodeInfo.publicKey from)
    <> " for " <> pack (show target) <> ": " <> pack (show status)
  modifyOnion $ setTargetNodes target nodes


{-------------------------------------------------------------------------------
 -
 - :: Onion data.
 -
 ------------------------------------------------------------------------------}

-- | Our DHT public key and nodes near us, so the friend can find our address.
dhtPkData :: OnionNodeMonad m => m BS.ByteString
dhtPkData = do
  ourDhtPk <- KeyPair.publicKey <$> getDhtKeyPair
  relays <- take (maxSentNodes `div` 2) <$> getTcpRelays
  close <- getCloseNodes (maxSentNodes - length relays) ourDhtPk
  let nodes = relays ++ close
  now <- askTime
  return $ LBS.toStrict $ Put.runPut $ do
    Put.putWord8 packetIdDhtPk
    Put.putWord64be $ Time.timestampToMicroseconds now `div` 1000000
    Binary.put ourDhtPk
    mapM_ Binary.put nodes

-- | Send our DHT public key to a friend through every node that stores them.
sendDhtPkIfDue :: OnionNodeMonad m => PublicKey -> m ()
sendDhtPkIfDue friendPk = do
  now <- askTime
  mFriend <- getsOnion $ Map.lookup friendPk . onionFriends
  forM_ mFriend $ \f ->
    when (Time.due now dhtPkSendInterval (ofLastDhtPkSent f)) $ do
      sent <- dhtPkData >>= sendToFriend friendPk
      when (sent > 0) $ do
        logDebugN $ "Sent our DHT public key to " <> pack (show friendPk)
        modifyOnion $ \s -> s { onionFriends = Map.adjust (\f' -> f' { ofLastDhtPkSent = Just now }) friendPk (onionFriends s) }
  forM_ (mFriend >>= \f -> (,) f <$> ofDhtPk f) $ \(f, friendDhtPk) ->
    when (Time.due now dhtPkDhtSendInterval (ofLastDhtPkDhtSent f)) $ do
      sent <- dhtPkData >>= sendDhtPkViaDht friendPk friendDhtPk
      when (sent > 0) $
        modifyOnion $ \s -> s { onionFriends = Map.adjust (\f' -> f' { ofLastDhtPkDhtSent = Just now }) friendPk (onionFriends s) }

-- | Our DHT public key in a DHT Request to the friend's DHT node, encrypted
-- with our real keys.
sendDhtPkViaDht :: OnionNodeMonad m => PublicKey -> PublicKey -> BS.ByteString -> m Int
sendDhtPkViaDht friendPk friendDhtPk dat = do
  realKeys <- getsOnion ourRealKeys
  shared <- getCombinedKey (KeyPair.secretKey realKeys) friendPk
  nonce <- randomNonce
  sendDhtRequest friendDhtPk $ BS.cons packetIdDhtPk (encodeStrict $ KeyPair.publicKey realKeys)
    <> Sodium.encode (unKey nonce) <> Box.unCipherText (Box.encrypt shared nonce (PlainText dat))

-- | Send onion data to a friend through the nodes that store their
-- announcement, if enough of the nodes we know near them do.  Returns the
-- number of nodes it was sent through.
sendToFriend :: OnionNodeMonad m => PublicKey -> BS.ByteString -> m Int
sendToFriend friendPk bytes = do
  nodes <- getsOnion $ maybe [] (Map.elems . ofNodes) . Map.lookup friendPk . onionFriends
  let holders = [ (onNode n, dataPk) | n <- nodes, StoresFriend dataPk <- [onStatus n] ]
  if null holders || length holders < (length nodes - 1) `div` 4 + 1 then return 0 else do
    forM_ holders $ \(node, dataPk) -> sendOnionData friendPk node dataPk bytes
    return $ length holders

-- | Send data to a friend through a node that stores their announcement.
sendOnionData :: OnionNodeMonad m => PublicKey -> NodeInfo -> PublicKey -> BS.ByteString -> m ()
sendOnionData friendPk node dataPk bytes = do
  realKeys <- getsOnion ourRealKeys
  nonce <- randomNonce
  tempKeys <- newKeyPair
  innerKey <- getCombinedKey (KeyPair.secretKey realKeys) friendPk
  outerKey <- getCombinedKey (KeyPair.secretKey tempKeys) dataPk
  let inner = RPC.DataRouteInner (KeyPair.publicKey realKeys) (Box.encrypt innerKey nonce (PlainText bytes))
      request = RPC.DataRouteRequest friendPk nonce (KeyPair.publicKey tempKeys) (Box.encrypt outerKey nonce (Box.encode inner))
  mPath <- zoomOnionPath $ Path.selectPath False
  forM_ mPath $ \path -> sendThroughPath path (NodeInfo.address node) $
    BS.cons (PacketKind.kindToByte PacketKind.OnionDataRequest) (encodeStrict request)

-- | Handle onion data sent to us through an announce node.
handleDataResponse :: OnionNodeMonad m => BS.ByteString -> m [OnionEvent]
handleDataResponse payload = do
  s <- getOnionState
  case decodeStrict payload of
    Nothing -> return []
    Just (res :: RPC.DataRouteResponse) -> do
      let nonce = RPC.dataRouteResponseNonce res
      outerKey <- getCombinedKey (KeyPair.secretKey $ ourDataKeys s) (RPC.dataRouteResponseTemporaryKey res)
      case Box.decrypt outerKey nonce (RPC.dataRouteResponseEncryptedPayload res) >>= Box.decode of
        Nothing -> return []
        Just (inner :: RPC.DataRouteInner) -> do
          let senderPk = RPC.dataRouteInnerSenderPublicKey inner
          innerKey <- getCombinedKey (KeyPair.secretKey $ ourRealKeys s) senderPk
          case Box.decrypt innerKey nonce (RPC.dataRouteInnerEncryptedPayload inner) of
            Just (PlainText bytes) | Just (ident, rest) <- BS.uncons bytes ->
              if | ident == packetIdDhtPk         -> handleDhtPk senderPk rest
                 | ident == packetIdFriendRequest -> return [FriendRequestReceived senderPk rest]
                 | otherwise                      -> return []
            _ -> return []

-- | A DHT request packet addressed to us.  Friends send their DHT public key
-- this way as well once they know ours.
handleDhtRequest :: OnionNodeMonad m => BS.ByteString -> m [OnionEvent]
handleDhtRequest payload = do
  ourDht <- getDhtKeyPair
  realKeys <- getsOnion ourRealKeys
  case decodeStrict payload of
    Just (DhtRequestPacket addressee dhtPkt) | addressee == KeyPair.publicKey ourDht -> do
      plain <- DhtPacket.decryptKeyed ourDht dhtPkt
      case plain of
        Just (PlainText bytes) | Just (ident, rest) <- BS.uncons bytes, ident == packetIdDhtPk
                               , (senderBytes, rest') <- BS.splitAt 32 rest
                               , (nonceBytes, sealed) <- BS.splitAt 24 rest'
                               , Just senderPk <- Key <$> Sodium.decode senderBytes
                               , Just nonce <- Key <$> Sodium.decode nonceBytes
                               , Just ct <- Tunnel.cipherTextMaybe sealed -> do
          shared <- getCombinedKey (KeyPair.secretKey realKeys) senderPk
          case Box.decrypt shared nonce ct of
            Just (PlainText dhtPkBytes) | Just (ident', dhtPkPacket) <- BS.uncons dhtPkBytes, ident' == packetIdDhtPk
                                        , BS.take 32 (BS.drop 8 dhtPkPacket) == Sodium.encode (unKey $ DhtPacket.senderPublicKey dhtPkt) ->
              handleDhtPk senderPk dhtPkPacket
            _ -> return []
        _ -> return []
    _ -> return []

-- | A friend told us their DHT public key.
handleDhtPk :: OnionNodeMonad m => PublicKey -> BS.ByteString -> m [OnionEvent]
handleDhtPk senderPk bytes = do
  mFriend <- getsOnion $ Map.lookup senderPk . onionFriends
  case (mFriend, decodeStrict bytes) of
    (Just f, Just (pkt :: RPC.DHTPublicKeyPacket)) | RPC.dhtPKPacketNoReplay pkt > ofLastNoReplay f -> do
      modifyOnion $ \s -> s { onionFriends = Map.insert senderPk f
        { ofLastNoReplay = RPC.dhtPKPacketNoReplay pkt, ofDhtPk = Just (RPC.dhtPKPacketOurDHTKey pkt) } (onionFriends s) }
      logInfoN $ "Received DHT public key of " <> pack (show senderPk)
      return [FriendDhtPkFound senderPk (RPC.dhtPKPacketOurDHTKey pkt) (RPC.dhtPKPacketNodes pkt)]
    _ -> return []


{-------------------------------------------------------------------------------
 -
 - :: Announce node.
 -
 ------------------------------------------------------------------------------}

-- | A ping id ties an announcement to the announcing key and the path's last
-- relay, and changes every 'announceTimeout'.
pingIdAt :: OnionState -> Integer -> PublicKey -> SocketAddress -> PublicKey
pingIdAt s period pk from =
  Key . fromJust . Sodium.decode . BS.take 32 . Hash.hash $
    pingIdSecret s <> encodeStrict period <> encodeStrict pk <> encodeStrict (Tunnel.OnionIPPort from)

pingIdPeriod :: Timestamp -> Integer
pingIdPeriod now = toInteger (Time.timestampToMicroseconds now) `div` (1000 * Time.toMilliseconds announceTimeout)

-- | Answer an announce request as toxcore does: store the sender if its ping
-- id is valid, and say whether we store it or the key it searches for.
answerAnnounce :: OnionNodeMonad m => SocketAddress -> PublicKey -> BS.ByteString -> RPC.AnnounceRequestPayload -> m (Word8, PublicKey, [NodeInfo])
answerAnnounce from senderPk ret plain = do
  s <- getOnionState
  ourDht <- getDhtKeyPair
  now <- askTime
  let period = pingIdPeriod now
      pingId = pingIdAt s period senderPk from
      valid = RPC.announceRequestPingId plain `elem` [pingId, pingIdAt s (period - 1) senderPk from]
      searchKey = RPC.announceRequestSearchKey plain
      dataPk = RPC.announceRequestDataSendbackKey plain
      live = Map.filter (\e -> Time.addTime (aeTime e) announceTimeout > now) (announceEntries s)
      entries
        | valid = Map.fromList . take maxAnnounceEntries . sortOn (Distance.xorDistance (KeyPair.publicKey ourDht) . fst)
            . Map.toList $ Map.insert senderPk (AnnounceEntry dataPk from ret now) live
        | otherwise = live
      entryKey = if valid then senderPk else searchKey
      (status, pingIdOrKey) = case Map.lookup entryKey entries of
        Just e | entryKey /= senderPk -> (1, aeDataPk e)
               | aeDataPk e == dataPk -> (2, pingId)
        _ -> (0, pingId)
  putOnionState s { announceEntries = entries }
  nodes <- getCloseNodesFor from maxSentNodes searchKey
  return (status, pingIdOrKey, nodes)

-- | Handle an announce request, followed by the return data of its path.
handleAnnounceRequest :: OnionNodeMonad m => SocketAddress -> BS.ByteString -> m ()
handleAnnounceRequest from payload = do
  let (requestBytes, ret) = BS.splitAt announceRequestLength payload
  ourDht <- getDhtKeyPair
  case decodeStrict requestBytes of
    Just (req :: RPC.AnnounceRequest) | BS.length ret == Tunnel.returnSize 3 -> do
      let senderPk = RPC.announceRequestSenderPublicKey req
      shared <- getCombinedKey (KeyPair.secretKey ourDht) senderPk
      let decrypted = Box.decrypt shared (RPC.announceRequestNonce req) (RPC.announceRequestEncryptedPayload req) >>= Box.decode
      forM_ decrypted $ \(plain :: RPC.AnnounceRequestPayload) -> do
        (status, pingIdOrKey, nodes) <- answerAnnounce from senderPk ret plain
        nonce <- randomNonce
        let response = RPC.AnnounceResponse
              { RPC.announceResponseSendbackData = RPC.announceRequestSendbackData plain
              , RPC.announceResponseNonce = nonce
              , RPC.announceResponseEncryptedPayload =
                  Box.encrypt shared nonce . Box.encode $ RPC.AnnounceResponsePayload status pingIdOrKey nodes
              }
        sendTo from $ Tunnel.responseToReturn ret (BS.cons (PacketKind.kindToByte PacketKind.AnnounceResponse) $ encodeStrict response)
    _ -> return ()

-- | Forward onion data to the announcer it is for, along its stored path.
handleDataRequest :: OnionNodeMonad m => BS.ByteString -> m ()
handleDataRequest payload = do
  let (request, ret) = BS.splitAt (BS.length payload - Tunnel.returnSize 3) payload
      (destBytes, forwarded) = BS.splitAt 32 request
  entries <- getsOnion announceEntries
  now <- askTime
  case Key <$> Sodium.decode destBytes of
    Just dest | BS.length request > dataRequestMinLength, BS.length ret == Tunnel.returnSize 3
              , Just e <- Map.lookup dest entries, Time.addTime (aeTime e) announceTimeout > now ->
      sendTo (aeAddress e) $ Tunnel.responseToReturn (aeReturn e) (BS.cons (PacketKind.kindToByte PacketKind.OnionDataResponse) forwarded)
    _ -> return ()


{-------------------------------------------------------------------------------
 -
 - :: Dispatch.
 -
 ------------------------------------------------------------------------------}

-- | Handle an onion packet: relay it, answer it as an announce node or take
-- a response to our own requests.
handleOnionPacket :: OnionNodeMonad m => SocketAddress -> Packet BS.ByteString -> m [OnionEvent]
handleOnionPacket from (Packet kind payload) = case kind of
  PacketKind.OnionRequest0     -> [] <$ relayRequest 1
  PacketKind.OnionRequest1     -> [] <$ relayRequest 2
  PacketKind.OnionRequest2     -> [] <$ relayRequest 3
  PacketKind.OnionResponse3    -> [] <$ relayResponse 3
  PacketKind.OnionResponse2    -> [] <$ relayResponse 2
  PacketKind.OnionResponse1    -> [] <$ relayResponse 1
  PacketKind.AnnounceRequest   -> [] <$ handleAnnounceRequest from payload
  PacketKind.OnionDataRequest  -> [] <$ handleDataRequest payload
  PacketKind.AnnounceResponse  -> [] <$ handleAnnounceResponse payload
  PacketKind.GroupAnnounceRequest  -> [] <$ handleGroupAnnounceRequest from payload
  PacketKind.GroupAnnounceResponse -> handleGroupAnnounceResponse payload
  PacketKind.OnionDataResponse -> handleDataResponse payload
  _                            -> return []
  where
    -- Bigger packets are dropped, as toxcore relays do.
    tooBig = 1 + BS.length payload > maxOnionPacketSize
    relayRequest hop = unless tooBig $ do
      ourDht <- getDhtKeyPair
      retKey <- getsOnion returnKey
      nonce <- randomNonce
      relayed <- Tunnel.relayRequest ourDht retKey nonce hop from payload
      forwardRelayed relayed
    relayResponse hop = unless tooBig $ do
      retKey <- getsOnion returnKey
      forwardRelayed $ Tunnel.relayResponse retKey hop payload

keyBytes :: Sodium.IsEncoding a => Key a -> BS.ByteString
keyBytes = Sodium.encode . unKey

-- | Look for a group's peers under a search key.
addGroupSearch :: OnionNodeMonad m => PublicKey -> BS.ByteString -> m ()
addGroupSearch searchKey chatId = do
  tempKeys <- newKeyPair
  modifyOnion $ \s -> s { onionGroups = Map.insert searchKey (OnionGroup tempKeys Map.empty chatId Nothing) (onionGroups s) }

removeGroupSearch :: OnionNodeMonad m => PublicKey -> m ()
removeGroupSearch searchKey = modifyOnion $ \s -> s { onionGroups = Map.delete searchKey (onionGroups s) }

-- | Our announcement for a group: the chat id and how to reach us.
setGroupAnnounce :: OnionNodeMonad m => PublicKey -> Maybe BS.ByteString -> m ()
setGroupAnnounce searchKey announcement = modifyOnion $ \s ->
  s { onionGroups = Map.adjust (\g -> g { ogAnnounce = announcement }) searchKey (onionGroups s) }

-- | An announce response with group announcements, as toxcore's 0x88.
handleGroupAnnounceResponse :: OnionNodeMonad m => BS.ByteString -> m [OnionEvent]
handleGroupAnnounceResponse payload = do
  s <- getOnionState
  now <- askTime
  case (Get.runGetOrFail Get.getWord64be (LBS.fromStrict payload), Key <$> Sodium.decode (BS.take 24 $ BS.drop 8 payload)) of
    (Right (_, _, sendbackId), Just nonce) -> do
      let (mSendback, sendbacks') = PingArray.takeEntry now sendbackId (sendbacks s)
      putOnionState s { sendbacks = sendbacks' }
      case mSendback of
        Just (Sendback target@(GroupSearch k) from pathNum) | Just group <- Map.lookup k (onionGroups s) -> do
          shared <- getCombinedKey (KeyPair.secretKey $ ogTempKeys group) (NodeInfo.publicKey from)
          case Tunnel.cipherTextMaybe (BS.drop 32 payload) >>= Box.decrypt shared nonce of
            Just (Box.PlainText plain) | Just (isStored, key, nodes, announces) <- parseGroupResponse plain, isStored < 2 -> do
              handleAnnounceResult target from pathNum (RPC.AnnounceResponsePayload isStored key nodes)
              return [ GroupAnnouncesFound (ogChatId group) announces | not (BS.null announces) ]
            _ -> return []
        _ -> return []
    _ -> return []
  where
    parseGroupResponse = fmap (\(rest, _, r) -> let (a, b, c) = r in (a, b, c, LBS.toStrict rest))
      . either (const Nothing) Just . Get.runGetOrFail getHead . LBS.fromStrict
    getHead = do
      isStored <- Get.getWord8
      key <- Binary.get
      count <- Get.getWord8
      when (fromIntegral count > maxSentNodes) $ fail "too many nodes"
      nodes <- replicateM (fromIntegral count) Binary.get
      return (isStored, key, nodes)

-- | An announce request that may carry a group announcement, as toxcore's
-- 0x87: we store the announcement and answer with others of the group.
handleGroupAnnounceRequest :: OnionNodeMonad m => SocketAddress -> BS.ByteString -> m ()
handleGroupAnnounceRequest from payload = do
  let (request, ret) = BS.splitAt (BS.length payload - Tunnel.returnSize 3) payload
      (nonceBytes, rest) = BS.splitAt 24 request
      (senderBytes, sealed) = BS.splitAt 32 rest
  ourDht <- getDhtKeyPair
  case (Key <$> Sodium.decode nonceBytes, Key <$> Sodium.decode senderBytes, Tunnel.cipherTextMaybe sealed) of
    (Just nonce, Just senderPk, Just ct) | BS.length ret == Tunnel.returnSize 3 -> do
      shared <- getCombinedKey (KeyPair.secretKey ourDht) senderPk
      case Box.decrypt shared nonce ct of
        Just (Box.PlainText plain) | (fields, extra) <- BS.splitAt announceRequestSize plain
                                   , Just (req :: RPC.AnnounceRequestPayload) <- decodeStrict fields
                                   , BS.length extra <= maxExtraDataSize -> do
          others <- if BS.null extra then return (Just []) else storeGroupAnnounce extra
          forM_ others $ \announces -> do
            (status, pingIdOrKey, nodes) <- answerAnnounce from senderPk ret req
            responseNonce <- randomNonce
            let body = LBS.toStrict . Put.runPut $ do
                  Put.putWord8 status
                  Binary.put pingIdOrKey
                  Put.putWord8 (fromIntegral $ length nodes)
                  mapM_ Binary.put nodes
                sealedResponse = Box.unCipherText $ Box.encrypt shared responseNonce (Box.PlainText $ body <> BS.concat announces)
            sendTo from $ Tunnel.responseToReturn ret $ BS.concat
              [ BS.singleton (PacketKind.kindToByte PacketKind.GroupAnnounceResponse)
              , encodeStrict (RPC.announceRequestSendbackData req), keyBytes responseNonce, sealedResponse ]
        _ -> return ()
    _ -> return ()

-- | Store a peer's group announcement, a chat id and how to reach the peer.
-- Returns the announcements of others in the group.
storeGroupAnnounce :: OnionNodeMonad m => BS.ByteString -> m (Maybe [BS.ByteString])
storeGroupAnnounce publicAnnounce = do
  now <- askTime
  s <- getOnionState
  let (chatId, announcement) = BS.splitAt 32 publicAnnounce
      peerBytes = BS.take 32 announcement
      existing = Map.lookup chatId (storedGroups s)
      announces = maybe [] sgAnnounces existing
      others = take maxSentAnnounces . nubBy ((==) `on` fst) $ filter ((/= peerBytes) . keyBytes . fst) announces
  case Key <$> Sodium.decode peerBytes of
    Just peerPk | isJust existing || Map.size (storedGroups s) < maxStoredGroups -> do
      putOnionState s { storedGroups = Map.insert chatId
        (StoredGroup (take maxStoredAnnounces $ (peerPk, announcement) : announces) now) (storedGroups s) }
      return . Just $ map snd others
    _ -> return Nothing

-- | An onion request from a client of our TCP relay, which we relay as the
-- first node.
handleTcpOnionRequest :: OnionNodeMonad m => Word64 -> BS.ByteString -> m ()
handleTcpOnionRequest client payload = do
  retKey <- getsOnion returnKey
  nonce <- randomNonce
  forwardRelayed $ Tunnel.relayTcpRequest retKey nonce client payload

forwardRelayed :: OnionNodeMonad m => Maybe Tunnel.Relayed -> m ()
forwardRelayed = mapM_ $ \(Tunnel.Relayed to pkt) -> case to of
  Tunnel.ToUdp addr         -> sendTo addr pkt
  Tunnel.ToTcpClient client -> sendToTcpClient client (encodeStrict pkt)
\end{code}
