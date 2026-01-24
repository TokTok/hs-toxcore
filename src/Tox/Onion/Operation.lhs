\section{Onion Operation}

\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
module Tox.Onion.Operation where

import           Control.Monad                (when, forM_)
import           Control.Monad.State          (MonadState, gets, modify, StateT, runStateT)
import           Data.Binary                  (Binary, encode)
import qualified Data.Binary                  as Binary
import           Data.List                    (sortBy)
import           Data.Ord                     (comparing)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Control.Monad.Validate       (runValidate)
import qualified Data.Maybe                   as Maybe
import           Data.MessagePack             (DecodeError)
import           Data.Word                    (Word32, Word64)

import qualified Crypto.Saltine.Class         as Sodium
import           Tox.Core.Time                (TimeDiff, Timestamp, getTime)
import qualified Tox.Core.Time                as Time
import           Tox.Core.Timed               (Timed)
import qualified Tox.Core.Timed               as Timed
import qualified Tox.Core.PingArray           as PingArray
import           Tox.Crypto.Core.Key               (PublicKey, Nonce, Key(..))
import           Tox.Crypto.Core.Keyed             (Keyed)
import qualified Tox.Crypto.Core.Keyed             as Keyed
import           Tox.Crypto.Core.KeyPair           (KeyPair)
import qualified Tox.Crypto.Core.KeyPair           as KeyPair
import           Tox.Crypto.Core.Box               (CipherText, unCipherText, unPlainText)
import qualified Tox.Crypto.Core.Box               as Box
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)
import qualified Tox.Crypto.Core.MonadRandomBytes as MonadRandomBytes
import qualified Tox.DHT.Distance             as Distance
import           Tox.Network.Core.Networked        (Networked)
import qualified Tox.Network.Core.Networked        as Networked
import           Tox.Network.Core.NodeInfo         (NodeInfo)
import qualified Tox.Network.Core.NodeInfo         as NodeInfo
import           Tox.Network.Core.Packet           (Packet (..))
import qualified Tox.Network.Core.PacketKind       as PacketKind
import           Tox.Network.Core.SocketAddress    (SocketAddress)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (UDP))
import           Tox.Onion.Path                    (OnionPath, OnionPathState)
import qualified Tox.Onion.Path                    as Path
import           Tox.Onion.RPC                     (AnnounceRequest (..), AnnounceResponse (..))
import qualified Tox.Onion.RPC                     as RPC
import qualified Tox.Onion.Tunnel                  as Tunnel
import qualified Tox.DHT.DhtPacket                 as DhtPacket

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

class (Monad m, Timed m, MonadRandomBytes m, Keyed m, Networked m) => OnionNodeMonad m where
  getOnionState :: m OnionState
  putOnionState :: OnionState -> m ()
  getDhtPublicKey :: m PublicKey

getOnion :: OnionNodeMonad m => m OnionState
getOnion = getOnionState

getsOnion :: OnionNodeMonad m => (OnionState -> a) -> m a
getsOnion f = f <$> getOnion

putOnion :: OnionNodeMonad m => OnionState -> m ()
putOnion = putOnionState

modifyOnion :: OnionNodeMonad m => (OnionState -> OnionState) -> m ()
modifyOnion f = getOnion >>= putOnion . f

getDhtPk :: OnionNodeMonad m => m PublicKey
getDhtPk = getDhtPublicKey

instance OnionNodeMonad m => Path.OnionPathMonad (StateT OnionPathState m)

cipherTextMaybe :: BS.ByteString -> Maybe Box.CipherText
cipherTextMaybe bs = case runValidate (Box.cipherText bs) of
  Left _   -> Nothing
  Right ct -> Just ct

data OnionState = OnionState
  { onionPaths      :: OnionPathState
  , ourLongTermKeys :: KeyPair
  , announcedNodes  :: Map PublicKey AnnouncedNode -- ^ Nodes closest to our real PK
  , searchNodes     :: Map PublicKey (Map PublicKey AnnouncedNode) -- ^ Friends we are searching for
  , friendsToSearch :: [PublicKey] -- ^ List of friend long-term PKs to search for
  , requestTracker  :: PingArray.PingArray OnionRequest -- ^ Metadata for outgoing requests
  }

data OnionRequest = OnionRequest
  { orSearchKey :: PublicKey
  , orTargetPk  :: PublicKey
  } deriving (Eq, Show)

data AnnouncedNode = AnnouncedNode
  { nodeInfo      :: NodeInfo
  , pingId        :: Maybe PublicKey -- ^ 0 if unknown
  , lastAnnounced :: Maybe Timestamp
  , pathNum       :: Word32 -- ^ Path used for this node
  } deriving (Eq, Show)

-- | Initial onion state.
initState :: KeyPair -> OnionState
initState keys = OnionState
  { onionPaths = Path.OnionPathState [] [] 0
  , ourLongTermKeys = keys
  , announcedNodes = Map.empty
  , searchNodes = Map.empty
  , friendsToSearch = []
  , requestTracker = PingArray.empty 1024 (Time.seconds 300)
  }

-- | Periodically maintain the onion layer.
doOnion :: OnionNodeMonad m => [NodeInfo] -> m ()
doOnion dhtNodes = do
  -- 1. Maintain paths
  zoomOnionPath $ Path.maintainPaths dhtNodes

  -- 2. Announce ourselves
  announceOurselves dhtNodes

  -- 3. Search for friends
  searchForFriends dhtNodes

zoomOnionPath :: OnionNodeMonad m => StateT OnionPathState m a -> m a
zoomOnionPath st = do
  s <- getOnion
  (a, s') <- runStateT st (onionPaths s)
  putOnion $ s { onionPaths = s' }
  return a


-- | Start searching for a friend.
startFriendSearch :: OnionNodeMonad m => PublicKey -> m ()
startFriendSearch friendPk = modifyOnion $ \s ->
  s { friendsToSearch = if friendPk `elem` friendsToSearch s then friendsToSearch s else friendPk : friendsToSearch s
    , searchNodes = Map.insertWith (\_ old -> old) friendPk Map.empty (searchNodes s)
    }


-- | Periodically search for friends.
searchForFriends :: OnionNodeMonad m => [NodeInfo] -> m ()
searchForFriends dhtNodes = do
  friends <- getsOnion friendsToSearch
  forM_ friends $ \friendPk -> do
    -- 1. Initial search using DHT nodes
    let closestDht = take 8 $ sortBy (comparing (Distance.xorDistance friendPk . NodeInfo.publicKey)) dhtNodes

    -- 2. Continue search using nodes we found via onion
    foundRelays <- getsOnion (Map.findWithDefault Map.empty friendPk . searchNodes)
    let closestOnion = take 8 $ sortBy (comparing (Distance.xorDistance friendPk . NodeInfo.publicKey . nodeInfo)) (Map.elems foundRelays)

    -- Combined list of nodes to ping
    let allNodes = closestDht ++ map nodeInfo closestOnion

    forM_ (take 8 allNodes) $ \node -> do
      mPath <- zoomOnionPath $ Path.selectPath False
      case mPath of
        Nothing -> return ()
        Just path -> sendAnnounceRequest path node friendPk Nothing


-- | Handle an Announce Response.
handleAnnounceResponse :: OnionNodeMonad m => NodeInfo -> RPC.AnnounceResponse -> m ()
handleAnnounceResponse from res = do
  ourLongTerm <- getsOnion ourLongTermKeys
  now <- Timed.askTime

  -- Use sendback data to find the original target node's PK.
  s <- getOnion
  let (mMeta, tracker') = PingArray.takeEntry now (RPC.announceResponseSendbackData res) (requestTracker s)
  putOnion $ s { requestTracker = tracker' }

  case mMeta of
    Nothing -> return () -- No matching request found
    Just (OnionRequest searchKey targetPk) -> do
      -- Decrypt the payload
      combined <- Keyed.getCombinedKey (KeyPair.secretKey ourLongTerm) targetPk
      case Box.decrypt combined (RPC.announceResponseNonce res) (RPC.announceResponseEncryptedPayload res) of
        Nothing -> return () -- Failed to decrypt
        Just plain -> case Box.decode plain of
          Nothing -> return () -- Failed to decode
          Just (payload :: RPC.AnnounceResponsePayload) -> do
            -- Update state with found info (pingId, etc.)
            let newNode = AnnouncedNode
                  { nodeInfo = NodeInfo.NodeInfo UDP (NodeInfo.address from) targetPk
                  , pingId = Just (RPC.announceResponsePingId payload)
                  , lastAnnounced = Just now
                  , pathNum = 0 -- TODO
                  }

            if searchKey == KeyPair.publicKey ourLongTerm
              then modifyOnion $ \s' -> s' { announcedNodes = Map.insert targetPk newNode (announcedNodes s') }
              else modifyOnion $ \s' -> s' { searchNodes = Map.adjust (Map.insert targetPk newNode) searchKey (searchNodes s') }


-- | Send an Announce Request through a chosen path.
sendAnnounceRequest :: OnionNodeMonad m => OnionPath -> NodeInfo -> PublicKey -> Maybe PublicKey -> m ()
sendAnnounceRequest path target searchKey mPingId = do
  ourLongTerm <- getsOnion ourLongTermKeys
  nonce <- MonadRandomBytes.randomNonce
  innerNonce <- MonadRandomBytes.randomNonce

  -- Store request metadata for later response handling
  now <- Timed.askTime
  seed <- MonadRandomBytes.randomWord64
  let meta = OnionRequest searchKey (NodeInfo.publicKey target)
  s <- getOnion
  let (sendback, tracker') = PingArray.addEntry now meta seed (requestTracker s)
  putOnion $ s { requestTracker = tracker' }

  -- We use a temporary public key if we are searching, real if announcing ourselves.
  let senderKeyPair = ourLongTerm -- TODO: use temporary key for searches

      payload = RPC.AnnounceRequestPayload
        { RPC.announceRequestPingId          = maybe (Key . Maybe.fromJust . Sodium.decode $ BS.replicate 32 0) id mPingId
        , RPC.announceRequestSearchKey       = searchKey
        , RPC.announceRequestDataSendbackKey = KeyPair.publicKey ourLongTerm -- TODO: temporary
        , RPC.announceRequestSendbackData    = sendback
        }

      -- Encrypt the payload for the target node
      -- Packet Kind 0x83
  combined <- Keyed.getCombinedKey (KeyPair.secretKey senderKeyPair) (NodeInfo.publicKey target)
  let encryptedPayload = Box.encrypt combined innerNonce (Box.encode payload)

      announceReq = RPC.AnnounceRequest
        { RPC.announceRequestNonce = innerNonce
        , RPC.announceRequestSenderPublicKey = KeyPair.publicKey senderKeyPair
        , RPC.announceRequestEncryptedPayload = encryptedPayload
        }

      -- 0x83 kind + AnnounceRequest
      dataToD = BS.singleton 0x83 <> LBS.toStrict (Binary.encode announceReq)

  -- Wrap the onion request
  case (cipherTextMaybe dataToD, Path.pathNodes path) of
    (Just ct, (nodeA:_)) -> do
      onionPkt0 <- Path.wrapPath ourLongTerm path (NodeInfo.address target) nonce ct
      Networked.sendPacket nodeA $ Packet PacketKind.OnionRequest0 onionPkt0
    _ -> return ()


-- | Decrypt and dispatch a DHT request payload addressed to us.
onDhtRequestPayload :: OnionNodeMonad m => NodeInfo -> DhtPacket.DhtPacket -> m ()
onDhtRequestPayload from dhtPkt = do
  ourKeyPair <- getsOnion ourLongTermKeys
  mPlain <- DhtPacket.decryptKeyed ourKeyPair dhtPkt
  case mPlain of
    Nothing -> return ()
    Just plain -> dispatchOnionData from (unPlainText plain)


-- | Handle an incoming top-level onion packet.
handleOnionPacket :: OnionNodeMonad m => NodeInfo -> Packet BS.ByteString -> m ()
handleOnionPacket from (Packet kind payload) = do
  ourKeyPair <- getsOnion ourLongTermKeys
  case kind of
    PacketKind.OnionRequest0 -> do
      case runBinary (Box.PlainText payload) of
        Nothing -> return ()
        Just (req :: Tunnel.OnionRequest0) -> do
          mInner <- Tunnel.unwrapOnion0 ourKeyPair req
          case mInner of
            Nothing -> return ()
            Just inner -> handleRelayOrDispatch from (Tunnel.onion0Nonce req) inner

    PacketKind.OnionRequest1 -> handleOnionRelay from payload
    PacketKind.OnionRequest2 -> handleOnionRelay from payload

    PacketKind.OnionResponse1 -> handleOnionResponse from payload
    PacketKind.OnionResponse2 -> handleOnionResponse from payload
    PacketKind.OnionResponse3 -> handleOnionResponse from payload

    _ -> return ()
  where
    runBinary (Box.PlainText bs) = case Binary.decodeOrFail (LBS.fromStrict bs) of
      Left _ -> Nothing
      Right (_, _, a) -> Just a


-- | Handle an intermediate relay request (0x81, 0x82).
handleOnionRelay :: OnionNodeMonad m => NodeInfo -> BS.ByteString -> m ()
handleOnionRelay _from _payload = do
  -- TODO: implement intermediate layer unwrapping and relaying
  return ()


-- | Handle an onion response (0x8c, 0x8d, 0x8e).
handleOnionResponse :: OnionNodeMonad m => NodeInfo -> BS.ByteString -> m ()
handleOnionResponse from bs = do
  case runBinary bs of
    Nothing -> return ()
    Just (res :: Tunnel.OnionResponse) -> do
      dispatchOnionData from (Tunnel.onionResponseData res)
  where
    runBinary payload = case Binary.decodeOrFail (LBS.fromStrict payload) of
      Left _ -> Nothing
      Right (_, _, a) -> Just a


-- | Handle the innermost payload of an onion request.
handleRelayOrDispatch :: OnionNodeMonad m => NodeInfo -> Nonce -> Tunnel.OnionRequestPayload -> m ()
handleRelayOrDispatch from _nonce payload = do
  dispatchOnionData from (unCipherText $ Tunnel.onionPayloadEncryptedPayload payload)


-- | Dispatch decrypted onion data to the appropriate service.
dispatchOnionData :: OnionNodeMonad m => NodeInfo -> BS.ByteString -> m ()
dispatchOnionData from bs = case BS.uncons bs of
  Nothing -> return ()
  Just (kind, payload) -> case kind of
    0x83 -> case runBinary payload of
              Nothing -> return ()
              Just req -> handleAnnounceRequest from req
    0x84 -> case runBinary payload of
              Nothing -> return ()
              Just res -> handleAnnounceResponse from res
    0x9c -> case runBinary payload of
              Nothing -> return ()
              Just pkt -> handleDHTPKPacket from pkt
    _    -> return ()
  where
    runBinary payload = case Binary.decodeOrFail (LBS.fromStrict payload) of
      Left _ -> Nothing
      Right (_, _, a) -> Just a


-- | Handle an Announce Request (Server side).
handleAnnounceRequest :: OnionNodeMonad m => NodeInfo -> RPC.AnnounceRequest -> m ()
handleAnnounceRequest _from _req = do
  -- TODO: implement server-side announce handling
  return ()


-- | Handle a received DHT Public Key packet.
handleDHTPKPacket :: OnionNodeMonad m => NodeInfo -> RPC.DHTPublicKeyPacket -> m ()
handleDHTPKPacket _from _pkt = do
  -- TODO: pass DHT key and nodes to DHT/FriendConnection modules
  return ()


-- | Send a DHT Public Key packet to a friend through their discovered relays.
sendDHTPublicKeyPacket :: OnionNodeMonad m => PublicKey -> m ()
sendDHTPublicKeyPacket friendPk = do
  dhtPk <- getDhtPk
  mRelays <- getsOnion (Map.lookup friendPk . searchNodes)
  case mRelays of
    Nothing -> return ()
    Just relays | Map.null relays -> return ()
    Just relays -> do
      let dhtPkPacket = RPC.DHTPublicKeyPacket
            { RPC.dhtPKPacketNoReplay = 0
            , RPC.dhtPKPacketOurDHTKey = dhtPk
            , RPC.dhtPKPacketNodes = []
            }

      forM_ (Map.elems relays) $ \relay -> do
         mPath <- zoomOnionPath $ Path.selectPath False
         case mPath of
           Nothing -> return ()
           Just path -> sendDataRouteRequest path (nodeInfo relay) friendPk (Box.encode dhtPkPacket)


-- | Announce ourselves to nodes closest to our real public key.
announceOurselves :: OnionNodeMonad m => [NodeInfo] -> m ()
announceOurselves dhtNodes = do
  ourLongTerm <- getsOnion ourLongTermKeys
  let ourPk = KeyPair.publicKey ourLongTerm
      closest = take 12 $ sortBy (comparing (Distance.xorDistance ourPk . NodeInfo.publicKey)) dhtNodes

  forM_ closest $ \node -> do
    mPath <- zoomOnionPath $ Path.selectPath True
    case mPath of
      Nothing -> return ()
      Just path -> do
        mAnnounced <- getsOnion (Map.lookup (NodeInfo.publicKey node) . announcedNodes)
        let mPingId = mAnnounced >>= pingId
        sendAnnounceRequest path node ourPk mPingId


-- | Send an Onion Data packet to a destination peer.
sendDataRouteRequest :: OnionNodeMonad m => OnionPath -> NodeInfo -> PublicKey -> Box.PlainText -> m ()
sendDataRouteRequest path relay destPk payload = do
  ourLongTerm <- getsOnion ourLongTermKeys
  nonce <- MonadRandomBytes.randomNonce
  tempKp <- MonadRandomBytes.newKeyPair

  innerCombined <- Keyed.getCombinedKey (KeyPair.secretKey ourLongTerm) destPk
  let innerEnc = Box.encrypt innerCombined nonce payload
      innerPayload = RPC.DataRouteInner (KeyPair.publicKey ourLongTerm) innerEnc

  relayCombined <- Keyed.getCombinedKey (KeyPair.secretKey tempKp) (NodeInfo.publicKey relay)
  let outerEnc = Box.encrypt relayCombined nonce (Box.encode innerPayload)
      routeReq = RPC.DataRouteRequest
        { RPC.dataRouteRequestDestination = destPk
        , RPC.dataRouteRequestNonce = nonce
        , RPC.dataRouteRequestTemporaryKey = KeyPair.publicKey tempKp
        , RPC.dataRouteRequestEncryptedPayload = outerEnc
        }

      dataToD = BS.singleton 0x85 <> LBS.toStrict (Binary.encode routeReq)

  case (cipherTextMaybe dataToD, Path.pathNodes path) of
    (Just ct, (nodeA:_)) -> do
      onionPkt0 <- Path.wrapPath ourLongTerm path (NodeInfo.address relay) nonce ct
      Networked.sendPacket nodeA $ Packet PacketKind.OnionRequest0 onionPkt0
    _ -> return ()

\end{code}
