{-# LANGUAGE LambdaCase #-}
-- | The messages two devices exchange in a sync session.
module Tox.Sync.Protocol
  ( SessionMessage (..)
  , CallAction (..)
  , HeadsDigest (..)
  , protocolVersion
  , accountSync
  , syncsAccounts
  , hello
  , headsDigest
  , encodeMessage
  , decodeMessage
  ) where

import qualified Crypto.Hash.SHA256      as SHA256
import           Data.Bits               ((.&.))
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import           Data.List               (sort)
import           Data.Word               (Word64)

import           Tox.Sync.Fact
import           Tox.Sync.Id
import qualified Tox.Sync.MessagePack    as MP

protocolVersion :: Word64
protocolVersion = 1

-- | The feature bit of account sync in 'Hello'. Unknown bits are kept, so a
-- newer device's features pass through unharmed.
accountSync :: Word64
accountSync = 1

-- | Equal on two devices exactly when they have the same heads.
newtype HeadsDigest = HeadsDigest BS.ByteString
  deriving (Eq, Ord, Show)

headsDigest :: [EventId] -> HeadsDigest
headsDigest heads = HeadsDigest (SHA256.hash (BS.concat [i | EventId i <- sort heads]))

-- | What a device asks of a friend's call.
data CallAction
  = Connect Bool
    -- ^ Answers it, or places it, with video or not, and has the home device
    -- call this device once it's connected.
  | HangUp
    -- ^ Ends it, or declines it while it rings.
  deriving (Eq, Show)

data SessionMessage
  = Hello Word64 Word64
    -- ^ Version and features.
  | Summary [(GraphId, HeadsDigest)]
  | Heads GraphId [EventId]
  | Want [EventId]
  | Events [BS.ByteString]
    -- ^ Encoded events, as stored.
  | History GraphId [EventId] Word64
    -- ^ Asks for up to the limit of ancestors of the events, newest first.
  | WantBlob BlobId
  | NoBlob BlobId
  | Facts [(Subject, Value)] Bool
    -- ^ Live facts, which aren't stored: with the flag, every one there is.
  | Call PublicKey CallAction
    -- ^ From a device, about the account's call with a friend.
  deriving (Eq, Show)

-- | Whether features of a 'Hello' offer account sync.
syncsAccounts :: Word64 -> Bool
syncsAccounts features = features .&. accountSync /= 0

hello :: SessionMessage
hello = Hello protocolVersion accountSync

ids :: [EventId] -> B.Builder
ids = MP.array . map (\(EventId i) -> MP.bin i)

graph :: GraphId -> B.Builder
graph (GraphId g) = MP.bin g

-- | @[tag, [fields]]@, or @[tag, field]@ for one with a single field.
encodeMessage :: SessionMessage -> BS.ByteString
encodeMessage m = MP.encode $ MP.array $ case m of
  Hello version features -> [MP.uint 0, MP.array [MP.uint version, MP.uint features]]
  Summary graphs ->
    [MP.uint 1, MP.array [MP.array [graph g, MP.bin d] | (g, HeadsDigest d) <- graphs]]
  Heads g heads -> [MP.uint 2, MP.array [graph g, ids heads]]
  Want wanted -> [MP.uint 3, ids wanted]
  Events events -> [MP.uint 4, MP.array (map MP.bin events)]
  History g before limit -> [MP.uint 5, MP.array [graph g, ids before, MP.uint limit]]
  WantBlob (BlobId b) -> [MP.uint 6, MP.bin b]
  NoBlob (BlobId b) -> [MP.uint 7, MP.bin b]
  Facts facts allOf -> [MP.uint 8, MP.array [MP.array [MP.array [subject s, value v] | (s, v) <- facts], MP.bool allOf]]
  Call (PublicKey friend) action ->
    [ MP.uint 9
    , MP.array [ MP.bin friend
               , case action of
                   Connect video -> MP.array [MP.uint 0, MP.bool video]
                   HangUp        -> MP.uint 1 ] ]

-- | Nothing for a message this version doesn't know.
decodeMessage :: BS.ByteString -> Either String (Maybe SessionMessage)
decodeMessage = MP.decodeExact (MP.variant known (const (pure Nothing)))
  where
    f = MP.field
    struct = fmap Just . f . MP.fields
    key = MP.getKey
    eventIds = MP.list (EventId <$> key)
    known :: Word64 -> MP.Fields (Maybe (Maybe SessionMessage))
    known = \case
      0 -> struct $ Just <$> (Hello <$> f MP.getUint <*> f MP.getUint)
      1 -> Just . Just . Summary <$> f (MP.list (MP.tuple ((,) <$> f (GraphId <$> key) <*> f (HeadsDigest <$> key))))
      2 -> struct $ Just <$> (Heads <$> f (GraphId <$> key) <*> f eventIds)
      3 -> Just . Just . Want <$> f eventIds
      4 -> Just . Just . Events <$> f (MP.list MP.getBin)
      5 -> struct $ Just <$> (History <$> f (GraphId <$> key) <*> f eventIds <*> f MP.getUint)
      6 -> Just . Just . WantBlob . BlobId <$> f key
      7 -> Just . Just . NoBlob . BlobId <$> f key
      8 -> struct $ Just <$> (Facts <$> f (MP.list (MP.tuple ((,) <$> f getSubject <*> f getValue))) <*> f MP.getBool)
      9 -> struct $ do
        friend <- f (PublicKey <$> key)
        fmap (Call friend) <$> f callAction
      _ -> pure Nothing
    callAction = MP.variant
      (\case
        0 -> Just . Just . Connect <$> f MP.getBool
        _ -> pure Nothing)
      (\case
        1 -> pure (Just HangUp)
        _ -> pure Nothing)

