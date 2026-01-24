{-# LANGUAGE OverloadedStrings #-}
-- | The 32-byte names sync uses: keys, events, graphs, blobs and chats.
module Tox.Sync.Id
  ( PublicKey (..)
  , EventId (..)
  , GraphId (..)
  , BlobId (..)
  , ChatId (..)
  , eventIdOf
  , blobIdOf
  , accountGraph
  , friendGraph
  , chatGraph
  , chat
  , getChat
  ) where

import qualified Crypto.Hash.SHA256      as SHA256
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B

import qualified Tox.Sync.MessagePack    as MP

-- | A Tox public key: an account's, a friend's, a device's, or a peer's.
newtype PublicKey = PublicKey BS.ByteString
  deriving (Eq, Ord, Show)

-- | SHA-256 of an event's exact bytes.
newtype EventId = EventId BS.ByteString
  deriving (Eq, Ord, Show)

newtype GraphId = GraphId BS.ByteString
  deriving (Eq, Ord, Show)

-- | A file's bytes, named by their SHA-256.
newtype BlobId = BlobId BS.ByteString
  deriving (Eq, Ord, Show)

-- | A group by its chat ID, or a conference by its ID.
data ChatId = GroupChat BS.ByteString | ConferenceChat BS.ByteString
  deriving (Eq, Ord, Show)

eventIdOf :: BS.ByteString -> EventId
eventIdOf = EventId . SHA256.hash

blobIdOf :: BS.ByteString -> BlobId
blobIdOf = BlobId . SHA256.hash

graphOf :: [BS.ByteString] -> GraphId
graphOf = GraphId . SHA256.hash . BS.concat

accountGraph :: PublicKey -> GraphId
accountGraph (PublicKey account) = graphOf ["account", account]

friendGraph :: PublicKey -> PublicKey -> GraphId
friendGraph (PublicKey account) (PublicKey friend) = graphOf ["friend", account, friend]

-- | The graph of a chat's conversation.
chatGraph :: ChatId -> GraphId
chatGraph (GroupChat i)      = graphOf ["group", i]
chatGraph (ConferenceChat i) = graphOf ["conference", i]

-- | @[kind, id]@: 0 a group, 1 a conference.
chat :: ChatId -> B.Builder
chat (GroupChat i)      = MP.array [MP.uint 0, MP.bin i]
chat (ConferenceChat i) = MP.array [MP.uint 1, MP.bin i]

getChat :: MP.Get ChatId
getChat = MP.variant known (const (fail "Unknown chat kind"))
  where
    known 0 = Just . GroupChat <$> MP.field MP.getKey
    known 1 = Just . ConferenceChat <$> MP.field MP.getKey
    known _ = pure Nothing
