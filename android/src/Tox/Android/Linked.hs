{-# LANGUAGE OverloadedStrings #-}
-- | A profile whose account another device runs on Tox, its home, as a
-- replica of the account kept in sync over Tox. What the screens show of it
-- is derived from its events, and what the user does becomes events.
module Tox.Android.Linked
  ( Linked (..)
  , newLinked
  , homeKey
  , accountKey
  , accountState
  , liveFacts
  , inSession
  , homeOnline
  , homeOffline
  , receivePacket
  , author
  , eventTime
  , fromMillis
  , toMillis
  ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Either            (fromRight)
import           Data.Int               (Int64)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Time              (UTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime,
                                         utcTimeToPOSIXSeconds)

import           Tox.Sync.Account       (AccountState, accountOf)
import qualified Tox.Sync.Event         as Event
import           Tox.Sync.Event         (Content, Event)
import           Tox.Sync.Fact          (Facts)
import qualified Tox.Sync.Frame         as Frame
import           Tox.Sync.Id
import           Tox.Sync.Protocol      (SessionMessage (..), decodeMessage,
                                         encodeMessage, hello)
import qualified Tox.Sync.Replica       as Replica
import           Tox.Sync.Store         (EventStore, allHeads)

data Linked = Linked
  { linkedHome     :: Text
    -- ^ The home's Tox address, in hex.
  , linkedPairing  :: Maybe Text
    -- ^ The friend request that pairs this device, until the home answers.
  , linkedSince    :: UTCTime
    -- ^ When it was linked: messages before aren't new here.
  , linkedReplica  :: Replica.Replica
  , linkedAccount  :: AccountState
    -- ^ What the account graph adds up to, kept as it changes.
  , linkedIncoming :: Frame.Reassembler
  }

-- | Equal where they hold the same: events never change, so the same heads
-- are the same events.
instance Eq Linked where
  a == b = linkedHome a == linkedHome b && linkedPairing a == linkedPairing b && linkedSince a == linkedSince b
    && inSession a == inSession b && liveFacts a == liveFacts b && heads a == heads b
    where heads = allHeads . Replica.replicaStore . linkedReplica

instance Show Linked where
  show l = "Linked " <> show (linkedHome l)

-- | A replica of the home's account, by its address, as this device's key,
-- with the events it holds.
newLinked :: Text -> Maybe Text -> UTCTime -> PublicKey -> EventStore -> Linked
newLinked home pairing since device store = Linked
  { linkedHome = home
  , linkedPairing = pairing
  , linkedSince = since
  , linkedReplica = replica
  , linkedAccount = accountOf (Replica.replicaAccount replica) store
  , linkedIncoming = Frame.reassembler
  }
  where replica = Replica.newReplica device (homeKeyOf home) store

homeKeyOf :: Text -> PublicKey
homeKeyOf = PublicKey . fromRight BS.empty . Base16.decode . Text.encodeUtf8 . Text.take 64

-- | The home's key, which is the account's: the home runs it on Tox.
homeKey :: Linked -> PublicKey
homeKey = homeKeyOf . linkedHome

accountKey :: Linked -> PublicKey
accountKey = homeKey

accountState :: Linked -> AccountState
accountState = linkedAccount

-- | What the home says Tox says now, while in session.
liveFacts :: Linked -> Facts
liveFacts = Replica.replicaLive . linkedReplica

inSession :: Linked -> Bool
inSession = Replica.replicaSession . linkedReplica

-- | Our 'hello' opens each connection to the home.
homeOnline :: Linked -> [BS.ByteString]
homeOnline _ = Frame.frames (encodeMessage hello)

homeOffline :: Linked -> Linked
homeOffline l = l { linkedReplica = Replica.offline (linkedReplica l), linkedIncoming = Frame.reassembler }

-- | A packet from the home: the packets to answer with, and whether the
-- events it brought changed anything.
receivePacket :: BS.ByteString -> Linked -> ([BS.ByteString], Linked)
receivePacket packet l = case Frame.push packet (linkedIncoming l) of
  Left (_, incoming) -> ([], l { linkedIncoming = incoming })
  Right (Nothing, incoming) -> ([], l { linkedIncoming = incoming })
  Right (Just bytes, incoming) -> case decodeMessage bytes of
    Right (Just message) ->
      let (replies, replica) = Replica.received message (linkedReplica l)
          l' = l { linkedIncoming = incoming, linkedReplica = replica }
          -- The home pairs us when its session starts.
          paired = case message of
            Hello {} -> l' { linkedPairing = Nothing }
            _        -> l'
      in (concatMap (Frame.frames . encodeMessage) replies, refreshed paired)
    _ -> ([], l { linkedIncoming = incoming })

-- | An event by this device, and the packets that tell the home.
author :: GraphId -> UTCTime -> Content -> Linked -> (EventId, [BS.ByteString], Linked)
author graph time content l = (i, concatMap (Frame.frames . encodeMessage) pushed, refreshed l { linkedReplica = replica })
  where (i, _, pushed, replica) = Replica.author graph (toMillis time) content (linkedReplica l)

refreshed :: Linked -> Linked
refreshed l = l { linkedAccount = accountOf (Replica.replicaAccount replica) (Replica.replicaStore replica) }
  where replica = linkedReplica l

eventTime :: Event -> UTCTime
eventTime = fromMillis . Event.eventTime

fromMillis :: Int64 -> UTCTime
fromMillis ms = posixSecondsToUTCTime (fromIntegral ms / 1000)

toMillis :: UTCTime -> Int64
toMillis t = round (utcTimeToPOSIXSeconds t * 1000)
