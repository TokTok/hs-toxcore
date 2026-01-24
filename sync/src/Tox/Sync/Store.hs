-- | Every event a device knows, with each graph's heads.
module Tox.Sync.Store
  ( EventStore
  , emptyStore
  , maxEvent
  , maxRank
  , insert
  , create
  , lookupEvent
  , member
  , eventBytes
  , storeGraphs
  , storedIds
  , headsOf
  , allHeads
  , countIn
  , missing
  , frontier
  , ancestors
  , byRank
  , displayOrder
  ) where

import qualified Data.ByteString as BS
import           Data.Int        (Int64)
import           Data.List       (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Word       (Word64)

import           Tox.Sync.Event
import           Tox.Sync.Id

-- | Forwarded as received, so every device computes the same IDs.
data Stored = Stored
  { storedEvent :: Event
  , storedBytes :: BS.ByteString
  }

data EventStore = EventStore
  { events     :: Map EventId Stored
  , graphs     :: Map GraphId [EventId]
  , heads      :: Map GraphId [EventId]
  , referenced :: Map GraphId (Set EventId)
    -- ^ Per graph, the events its events have as parents.
  }

emptyStore :: EventStore
emptyStore = EventStore Map.empty Map.empty Map.empty Map.empty

maxEvent :: Int
maxEvent = 64 * 1024

-- | Ranks stay below this, so one more always fits, also where numbers are
-- doubles.
maxRank :: Word64
maxRank = 2 ^ (53 :: Int)

-- | Adds an event by its bytes, unless it's there already.
insert :: BS.ByteString -> EventStore -> Either String (EventId, EventStore)
insert bytes store
  | BS.length bytes > maxEvent = Left "Event too long"
  | member i store = Right (i, store)
  | otherwise = do
      event <- decodeEvent bytes
      checkRank event store
      pure (i, add i event bytes store)
  where
    i = eventIdOf bytes

-- | An event's rank is one more than its highest parent's. Only the parents
-- here can be checked: the others may come later, as history is paged in
-- newest first.
checkRank :: Event -> EventStore -> Either String ()
checkRank event store
  | fits = Right ()
  | otherwise = Left "Rank doesn't follow its parents'"
  where
    known = map eventRank (mapMaybe (`lookupEvent` store) (eventParents event))
    fits = eventRank event < maxRank && case known of
      [] -> True
      _ | length known == length (eventParents event) -> eventRank event == maximum known + 1
        | otherwise -> eventRank event > maximum known

-- | An event by an author, after every head of its graph.
create :: GraphId -> PublicKey -> Int64 -> Content -> EventStore -> (EventId, BS.ByteString, EventStore)
create graph author time c store = (i, bytes, add i event bytes store)
  where
    parents = Set.toAscList (Set.fromList (headsOf graph store))
    rank = maybe 0 (+ 1) (maximumMay (map eventRank (mapMaybe (`lookupEvent` store) parents)))
    event = Event graph author parents rank time c
    bytes = encodeEvent event
    i = eventIdOf bytes
    maximumMay [] = Nothing
    maximumMay xs = Just (maximum xs)

add :: EventId -> Event -> BS.ByteString -> EventStore -> EventStore
add i event bytes store = store
  { events = Map.insert i (Stored event bytes) (events store)
  , graphs = Map.insertWith (++) graph [i] (graphs store)
  , heads = Map.insert graph heads' (heads store)
  , referenced = Map.insert graph (Set.union refs (Set.fromList (eventParents event))) (referenced store)
  }
  where
    graph = eventGraph event
    refs = Map.findWithDefault Set.empty graph (referenced store)
    kept = filter (`notElem` eventParents event) (headsOf graph store)
    heads' = if Set.member i refs then kept else kept ++ [i]

lookupEvent :: EventId -> EventStore -> Maybe Event
lookupEvent i = fmap storedEvent . Map.lookup i . events

member :: EventId -> EventStore -> Bool
member i = Map.member i . events

eventBytes :: EventId -> EventStore -> Maybe BS.ByteString
eventBytes i = fmap storedBytes . Map.lookup i . events

-- | Every event held.
storedIds :: EventStore -> Set EventId
storedIds = Map.keysSet . events

storeGraphs :: EventStore -> [GraphId]
storeGraphs = Map.keys . graphs

headsOf :: GraphId -> EventStore -> [EventId]
headsOf g = Map.findWithDefault [] g . heads

allHeads :: EventStore -> [(GraphId, [EventId])]
allHeads = Map.toList . heads

countIn :: GraphId -> EventStore -> Int
countIn g = length . idsIn g

idsIn :: GraphId -> EventStore -> [EventId]
idsIn g = Map.findWithDefault [] g . graphs

eventsIn :: GraphId -> EventStore -> [(EventId, Event)]
eventsIn g store = [(i, storedEvent (events store Map.! i)) | i <- idsIn g store]

-- | Parents of the graph's events that aren't here.
missing :: GraphId -> EventStore -> Set EventId
missing g store =
  Set.fromList [p | (_, e) <- eventsIn g store, p <- eventParents e, not (member p store)]

-- | The graph's events whose parents aren't all here: where its history
-- stops here, or has a gap.
frontier :: GraphId -> EventStore -> [(EventId, Event)]
frontier g store = [(i, e) | (i, e) <- eventsIn g store, not (all (`member` store) (eventParents e))]

-- | Up to a number of ancestors of events, newest (highest rank) first.
ancestors :: [EventId] -> Int -> EventStore -> [EventId]
ancestors start limit store = go initial seen0 []
  where
    rankOf i = eventRank <$> lookupEvent i store
    push (queue, seen) i = case rankOf i of
      Just r | not (Set.member i seen) -> (Set.insert (r, i) queue, Set.insert i seen)
      _ -> (queue, seen)
    (initial, seen0) =
      foldl push (Set.empty, Set.empty) [p | i <- start, e <- maybe [] pure (lookupEvent i store), p <- eventParents e]
    go queue seen found
      | length found >= limit = reverse found
      | otherwise = case Set.maxView queue of
          Nothing -> reverse found
          Just ((_, i), rest) ->
            let parents = maybe [] eventParents (lookupEvent i store)
                (queue', seen') = foldl push (rest, seen) parents
            in go queue' seen' (i : found)

-- | In the order last-writer-wins rules apply them: by rank, then ID.
byRank :: GraphId -> EventStore -> [(EventId, Event)]
byRank g = sortOn (\(i, e) -> (eventRank e, i)) . eventsIn g

-- | By rank, then effective time, then ID. An event's effective time is
-- never before its parents', so a fast clock can't move it back in history.
displayOrder :: GraphId -> EventStore -> [(EventId, Event)]
displayOrder g store = sortOn (\(i, e) -> (eventRank e, effective Map.! i, i)) ranked
  where
    ranked = byRank g store
    effective = foldl step Map.empty ranked
    step m (i, e) =
      Map.insert i (maximum (eventTime e : mapMaybe (`Map.lookup` m) (eventParents e))) m

