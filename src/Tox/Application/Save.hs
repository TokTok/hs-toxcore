{-# LANGUAGE StrictData #-}
-- | Our state in toxcore's save format, so profiles move between clients.
module Tox.Application.Save
    ( Profile (..)
    , SavedFriend (..)
    , profileOf
    , savedConference
    , toSaveData
    , fromSaveData
    ) where

import           Control.Monad               (guard)
import           Data.Binary.Get             (getWord32be, runGet)
import           Data.Binary.Put             (putWord32be, runPut)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Map                    as Map
import           Data.Maybe                  (isJust, mapMaybe)
import           Data.Word                   (Word32, Word64, Word8)

import qualified Crypto.Saltine.Class        as Sodium
import qualified Tox.Application.GroupChat   as Group
import qualified Tox.Application.LegacyGroup as Conference
import           Tox.Application.State       (GlobalState (..))
import qualified Tox.Core.Time               as Time
import           Tox.Crypto.Core.Key         (Key (..), PublicKey)
import qualified Tox.Crypto.Core.KeyPair     as KeyPair
import           Tox.Crypto.Core.KeyPair     (KeyPair (..))
import qualified Tox.DHT.DhtState            as DhtState
import qualified Tox.Network.Core.NodeInfo   as NodeInfo
import           Tox.Network.Core.NodeInfo   (NodeInfo)
import qualified Tox.Network.TCP.Client      as Client
import qualified Tox.Network.TCP.Connections as Tcp
import qualified Tox.Onion.Operation         as Onion
import qualified Tox.Persistence             as P
import           Tox.Persistence.Bytes       (Bytes (..))
import qualified Tox.Persistence.Conferences as PC
import           Tox.Persistence.DHT         (DHT (..), DhtSection (..))
import qualified Tox.Persistence.Friend      as PF
import qualified Tox.Persistence.Groups      as PG
import           Tox.Persistence.Nodes       (Nodes (..))
import qualified Tox.Session.Connection      as Connection
import qualified Tox.Session.Friend          as Messenger

data SavedFriend = SavedFriend
  { sfPublicKey     :: PublicKey
  , sfRequest       :: Maybe (BS.ByteString, BS.ByteString) -- ^ Nospam and message, until accepted.
  , sfName          :: BS.ByteString
  , sfStatusMessage :: BS.ByteString
  , sfStatus        :: Messenger.UserStatus
  , sfLastSeen      :: Word64
  } deriving (Show)

-- | What a save file holds.
data Profile = Profile
  { profileKeys          :: KeyPair
  , profileNospam        :: Word32
  , profileName          :: BS.ByteString
  , profileStatusMessage :: BS.ByteString
  , profileStatus        :: Messenger.UserStatus
  , profileFriends       :: [SavedFriend]
  , profileDhtNodes      :: [NodeInfo]
  , profileTcpRelays     :: [NodeInfo]
  , profilePathNodes     :: [NodeInfo]
  , profileConferences   :: [PC.Conference]
  , profileGroups        :: [PG.Group]
  } deriving (Show)

-- | How many DHT nodes and relays we keep, to bootstrap from next time.
savedDhtNodes, savedTcpRelays, savedPathNodes :: Int
savedDhtNodes  = 32
savedTcpRelays = 8
savedPathNodes = 8

-- | Like toxcore, message numbers jump ahead so peers take our new ones.
messageNumberJump, lossyMessageNumberJump :: Integral a => a
messageNumberJump      = 2 ^ (16 :: Int)
lossyMessageNumberJump = 2 ^ (13 :: Int)

-- | Our profile, with a way to turn the times we keep into unix time.
profileOf :: (Time.Timestamp -> Word64) -> GlobalState -> Profile
profileOf unixAt gs = Profile
  { profileKeys          = Connection.ourRealKeys (connManager gs)
  , profileNospam        = Messenger.selfNospam m
  , profileName          = Messenger.selfName m
  , profileStatusMessage = Messenger.selfMsg m
  , profileStatus        = Messenger.selfStatus m
  , profileFriends       = map savedFriend . Map.elems $ Messenger.messengerFriends m
  , profileDhtNodes      = DhtState.takeClosestNodesTo savedDhtNodes dhtPk (dhtState gs)
  -- The relay we host has our DHT key, which is new next time.
  , profileTcpRelays     = take savedTcpRelays [ Client.rcRelay rc | rc <- Map.elems (Tcp.tsRelays $ tcpState gs)
                                                , Client.isConfirmed rc, NodeInfo.publicKey (Client.rcRelay rc) /= dhtPk ]
  , profilePathNodes     = take savedPathNodes . Onion.pathPool $ onionState gs
  , profileGroups        = Group.saveGroups (groupChats gs)
  , profileConferences   = mapMaybe (savedConference unixAt) . filter Conference.cfConnected . Map.elems . Conference.conferences $ conferences gs
  }
  where
    m = messenger gs
    dhtPk = KeyPair.publicKey . DhtState.dhtKeyPair $ dhtState gs
    savedFriend f = SavedFriend
      { sfPublicKey     = Messenger.friendRealPk f
      , sfRequest       = (\r -> (Messenger.orNospam r, Messenger.orMessage r)) <$> Messenger.friendRequest f
      , sfName          = Messenger.friendName f
      , sfStatusMessage = Messenger.friendMsg f
      , sfStatus        = Messenger.friendStatus f
      , sfLastSeen      = Messenger.friendLastSeen f
      }

-- | A conference as a save file holds it.
savedConference :: (Time.Timestamp -> Word64) -> Conference.Conference -> Maybe PC.Conference
savedConference unixAt c = (\gid -> PC.Conference
  { PC.conferenceType     = Conference.cfType c
  , PC.conferenceId       = Key gid
  , PC.messageNumber      = Conference.cfMessageNumber c + messageNumberJump
  , PC.lossyMessageNumber = lossyMessageNumberJump
  , PC.selfPeerNumber     = Conference.cfSelf c
  , PC.title              = Conference.cfTitle c
  , PC.peers              =
      [ PC.Peer (Conference.peerRealPk p) (Conference.peerDhtPk p) n (lastActive p) (Conference.peerName p)
      | (n, p) <- Map.toList (Conference.cfPeers c) ++ Map.toList (Conference.cfFrozen c), n /= Conference.cfSelf c ]
  }) <$> Sodium.decode (Conference.cfId c)
  where
    lastActive = unixAt . Conference.peerLastActive

-- | Friend states in toxcore's save files: added with a request we keep
-- sending, and confirmed.
friendAdded, friendConfirmed :: Word8
friendAdded = 1
friendConfirmed = 3

toSaveData :: Profile -> P.SaveData
toSaveData p = P.SaveData $
  [ P.SectionNospamKeys $ P.NospamKeys (profileNospam p) (KeyPair.publicKey keys) (KeyPair.secretKey keys)
  , P.SectionDHT $ DHT [DhtSectionNodes . Nodes $ profileDhtNodes p]
  , P.SectionFriends . P.Friends $ map savedFriend (profileFriends p)
  , P.SectionName . Bytes . LBS.fromStrict $ profileName p
  , P.SectionStatusMessage . Bytes . LBS.fromStrict $ profileStatusMessage p
  , P.SectionStatus . fromIntegral . fromEnum $ profileStatus p
  ] ++
  [ P.SectionGroups . PG.Groups $ profileGroups p | not . null $ profileGroups p ] ++
  [ P.SectionTcpRelays . Nodes $ profileTcpRelays p
  , P.SectionPathNodes . Nodes $ profilePathNodes p
  , P.SectionConferences . PC.Conferences $ profileConferences p
  , P.SectionEOF
  ]
  where
    keys = profileKeys p
    savedFriend f = PF.Friend
      { PF.status        = if isJust (sfRequest f) then friendAdded else friendConfirmed
      , PF.publicKey     = sfPublicKey f
      , PF.friendRequest = maybe BS.empty snd (sfRequest f)
      , PF.name          = sfName f
      , PF.statusMessage = sfStatusMessage f
      , PF.userStatus    = fromIntegral . fromEnum $ sfStatus f
      , PF.nospam        = maybe 0 (runGet getWord32be . LBS.fromStrict . fst) (sfRequest f)
      , PF.lastSeenTime  = sfLastSeen f
      }

-- | The profile in a save file, or 'Nothing' if it has no keys.
fromSaveData :: P.SaveData -> Maybe Profile
fromSaveData (P.SaveData sections) = do
  P.NospamKeys nospam pk sk <- foldr (\s r -> case s of P.SectionNospamKeys k -> Just k; _ -> r) Nothing sections
  -- As toxcore does, refuse keys that do not belong together.
  guard $ KeyPair.publicKey (KeyPair.fromSecretKey sk) == pk
  return Profile
    { profileKeys          = KeyPair sk pk
    , profileNospam        = nospam
    , profileName          = BS.concat [ LBS.toStrict b | P.SectionName (Bytes b) <- sections ]
    , profileStatusMessage = BS.concat [ LBS.toStrict b | P.SectionStatusMessage (Bytes b) <- sections ]
    , profileStatus        = last (Messenger.Online : [ toStatus s | P.SectionStatus s <- sections ])
    , profileFriends       = concat [ map loadFriend fs | P.SectionFriends (P.Friends fs) <- sections ]
    , profileDhtNodes      = concat [ ns | P.SectionDHT (DHT ds) <- sections, DhtSectionNodes (Nodes ns) <- ds ]
    , profileTcpRelays     = concat [ ns | P.SectionTcpRelays (Nodes ns) <- sections ]
    , profilePathNodes     = concat [ ns | P.SectionPathNodes (Nodes ns) <- sections ]
    , profileConferences   = concat [ cs | P.SectionConferences (PC.Conferences cs) <- sections ]
    , profileGroups        = concat [ gs | P.SectionGroups (PG.Groups gs) <- sections ]
    }
  where
    toStatus s = if fromIntegral s <= fromEnum (maxBound :: Messenger.UserStatus) then toEnum (fromIntegral s) else Messenger.Online
    loadFriend f = SavedFriend
      { sfPublicKey     = PF.publicKey f
      , sfRequest       = if PF.status f >= friendConfirmed then Nothing
                          else Just (LBS.toStrict . runPut . putWord32be $ PF.nospam f, PF.friendRequest f)
      , sfName          = PF.name f
      , sfStatusMessage = PF.statusMessage f
      , sfStatus        = toStatus (PF.userStatus f)
      , sfLastSeen      = PF.lastSeenTime f
      }
