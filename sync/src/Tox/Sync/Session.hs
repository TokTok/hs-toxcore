-- | The exchange both ends of a session run: each sends a 'Summary', answers
-- the other's with 'Heads' where they differ, asks with 'Want' for heads it
-- lacks, and answers 'Want' and 'History' from its own store.
module Tox.Sync.Session
  ( summary
  , headsFor
  , want
  , batches
  , answer
  , maxHistory
  ) where

import qualified Data.ByteString   as BS
import qualified Data.Map.Strict   as Map
import           Data.Maybe        (mapMaybe)
import           Data.Word         (Word64)

import           Tox.Sync.Event
import           Tox.Sync.Id
import           Tox.Sync.Protocol
import           Tox.Sync.Store

-- | Keeps replies small, so live events aren't stuck behind them.
maxEventsBytes :: Int
maxEventsBytes = 64 * 1024

maxWant :: Int
maxWant = 64

-- | The most a 'History' answer holds.
maxHistory :: Word64
maxHistory = 500

summary :: EventStore -> SessionMessage
summary store = Summary [(g, headsDigest hs) | (g, hs) <- allHeads store]

-- | Our heads of each graph the other side holds differently or not at all.
-- Graphs only they have come back in their 'Heads' for our summary.
headsFor :: EventStore -> [(GraphId, HeadsDigest)] -> [SessionMessage]
headsFor store theirs =
  [Heads g hs | (g, hs) <- allHeads store, Map.lookup g theirs' /= Just (headsDigest hs)]
  where theirs' = Map.fromList theirs

want :: [EventId] -> [SessionMessage]
want [] = []
want ids = Want chunk : want rest
  where (chunk, rest) = splitAt maxWant ids

-- | Encoded events, in as many 'Events' messages as they need.
batches :: [BS.ByteString] -> [SessionMessage]
batches = go [] 0
  where
    go [] _ [] = []
    go batch _ [] = [Events (reverse batch)]
    go batch size (e : es)
      | size + BS.length e > maxEventsBytes && not (null batch) = Events (reverse batch) : go [e] (BS.length e) es
      | otherwise = go (e : batch) (size + BS.length e) es

-- | The replies to a message, as far as they only need the store. 'Events'
-- and 'Facts' are for the caller, which knows what to accept.
answer :: EventStore -> SessionMessage -> [SessionMessage]
answer store message = case message of
  Summary theirs -> headsFor store theirs
  Heads _ hs -> want (filter (not . (`member` store)) hs)
  Want ids -> batches (mapMaybe (`eventBytes` store) (take maxWant ids))
  History g before limit ->
    batches [ b | i <- ancestors before (fromIntegral (min limit maxHistory)) store
                , Just e <- [lookupEvent i store], eventGraph e == g
                , Just b <- [eventBytes i store] ]
  _ -> []
