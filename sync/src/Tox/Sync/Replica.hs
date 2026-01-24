-- | Keeping a copy of an account, for a device whose home device runs it on
-- Tox.
module Tox.Sync.Replica
  ( Replica (..)
  , newReplica
  , historyPage
  , offline
  , author
  , received
  ) where

import qualified Data.ByteString   as BS
import           Data.Int          (Int64)
import           Data.List         (nub)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Word         (Word64)

import           Tox.Sync.Event
import           Tox.Sync.Fact
import           Tox.Sync.Id
import           Tox.Sync.Protocol
import           Tox.Sync.Session
import           Tox.Sync.Store

-- | Conversations start from their latest events; older ones are asked for
-- when wanted.
historyPage :: Word64
historyPage = 50

data Replica = Replica
  { replicaDevice  :: PublicKey
  , replicaAccount :: GraphId
  , replicaStore   :: EventStore
  , replicaSession :: Bool
  , replicaPaged   :: Set GraphId
    -- ^ Conversations whose latest page was asked for in this session.
  , replicaLive    :: Facts
  }

-- | A device's replica of the account of a home device, by their keys.
newReplica :: PublicKey -> PublicKey -> EventStore -> Replica
newReplica device home store = Replica device (accountGraph home) store False Set.empty noFacts

offline :: Replica -> Replica
offline r = r { replicaSession = False, replicaPaged = Set.empty }

-- | Adds an event by this device, and what to send the home device for it:
-- nothing outside a session, where the next one's summaries catch up.
author :: GraphId -> Int64 -> Content -> Replica -> (EventId, BS.ByteString, [SessionMessage], Replica)
author g time c r = (i, bytes, [Events [bytes] | replicaSession r], r { replicaStore = store })
  where (i, bytes, store) = create g (replicaDevice r) time c (replicaStore r)

-- | The replies to a message from the home device.
received :: SessionMessage -> Replica -> ([SessionMessage], Replica)
received message r = case message of
  Hello _ features ->
    let session = syncsAccounts features
    in ([summary (replicaStore r) | session], r { replicaSession = session, replicaPaged = Set.empty })
  _ | not (replicaSession r) -> ([], r)
  Events events -> addEvents events r
  Facts facts everything ->
    ([], r { replicaLive = updateFacts facts (if everything then noFacts else replicaLive r) })
  _ -> (answer (replicaStore r) message, r)

addEvents :: [BS.ByteString] -> Replica -> ([SessionMessage], Replica)
addEvents events r0 = (concat replies, r2)
  where
    -- The graphs of what came, also of events held already.
    (came, grew, store) = foldl take' ([], Set.empty, replicaStore r0) events
    take' (gs, grown, s) bytes = case lookupEvent (eventIdOf bytes) s of
      Just held -> (eventGraph held : gs, grown, s)
      Nothing -> case insert bytes s of
        Right (i, s') | Just e <- lookupEvent i s' -> (eventGraph e : gs, Set.insert (eventGraph e) grown, s')
        _ -> (gs, grown, s)
    r1 = r0 { replicaStore = store }
    (replies, r2) = foldl step ([], r1) (nub came)
    step (acc, r) g = let (out, r') = forGraph g (Set.member g grew) r in (acc ++ [out], r')

forGraph :: GraphId -> Bool -> Replica -> ([SessionMessage], Replica)
forGraph g grown r = case gap g r of
  Just ids
    | grown -> ([History g ids maxHistory], r)
    -- History brought nothing new: the missing parents are further back
    -- than it goes.
    | otherwise -> (want (Set.toAscList (Set.fromList
        [p | i <- ids, Just e <- [lookupEvent i store], p <- eventParents e, not (member p store)])), r)
  Nothing
    | grown && g /= replicaAccount r && not (Set.member g (replicaPaged r)) ->
        ([History g (headsOf g store) historyPage], r { replicaPaged = Set.insert g (replicaPaged r) })
    | otherwise -> ([], r)
  where store = replicaStore r

-- | The events to ask for the ancestors of, while a graph has a gap: all
-- the account's, and of a conversation the part between its latest events
-- and the older ones held here.
gap :: GraphId -> Replica -> Maybe [EventId]
gap g r = if null ids then Nothing else Just ids
  where
    store = replicaStore r
    oldest
      | g == replicaAccount r = Nothing
      | otherwise = case map (eventRank . snd) (byRank g store) of
          []    -> Nothing
          ranks -> Just (minimum ranks)
    ids = [i | (i, e) <- frontier g store, maybe True (eventRank e >) oldest]
