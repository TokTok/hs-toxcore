{-# LANGUAGE LambdaCase #-}
-- | Our messages on their way: the parts Tox holds for the friend and the
-- parts the friend confirmed, so that a message may be sent again at any
-- time and only what is missing goes.
module Tox.Android.Outgoing
  ( Outgoing
  , newOutgoing
  , send
  , receipt
  , friendGone
  , flush
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Text               (Text)
import           Data.Time               (getCurrentTime)
import           Data.Word               (Word32)
import qualified Tox.Application.Tox     as Tox
import           Tox.Crypto.Core.Key     (PublicKey)

import           Tox.Android.Messages    (parts)
import           Tox.Android.Model

data Progress = Progress
  { prContact   :: ContactId
  , prText      :: Text
  , prConfirmed :: Int
  , prHeld      :: Int
    -- ^ Parts Tox took and will report the receipts of.
  , prUnsent    :: Bool
    -- ^ Whether Tox's queue had no room for the next part.
  }

data State = State
  { stMessages :: Map (PublicKey, MessageId) Progress
  , stReceipts :: Map (PublicKey, Word32) MessageId
  }

newtype Outgoing = Outgoing (MVar State)

newOutgoing :: IO Outgoing
newOutgoing = Outgoing <$> newMVar (State Map.empty Map.empty)

-- | Give Tox the parts of a message it doesn't have, after those the friend
-- confirmed, as far as we know or the model says.
send :: Tox.Tox -> Outgoing -> ContactId -> PublicKey -> MessageId -> Text -> Int -> IO [Event]
send tox (Outgoing v) cid pk i text confirmed = modifyMVar v $ \st -> do
  let known = Map.lookup (pk, i) (stMessages st)
      p = maybe (Progress cid text confirmed 0 False)
            (\q -> q { prConfirmed = max confirmed (prConfirmed q) }) known
      all' = parts text
  (p', receipts) <- go p [] (drop (prConfirmed p + prHeld p) all')
  let taken = prConfirmed p' + prHeld p' == length all'
  pure ( st { stMessages = Map.insert (pk, i) p' (stMessages st)
            , stReceipts = foldr (\r -> Map.insert (pk, r) i) (stReceipts st) receipts }
       , [Delivering cid i Sent | taken, not (null receipts), prConfirmed p' < length all'] )
  where
    go p receipts [] = pure (p { prUnsent = False }, receipts)
    go p receipts (part : rest) = Tox.friendSendMessage tox pk Tox.MessageNormal part >>= \case
      Right r -> go p { prHeld = prHeld p + 1 } (r : receipts) rest
      Left Tox.SendQueueFull -> pure (p { prUnsent = True }, receipts)
      Left _ -> pure (p { prUnsent = False }, receipts)

-- | The friend received a part: how many of its message it has now.
receipt :: Outgoing -> PublicKey -> Word32 -> IO [Event]
receipt (Outgoing v) pk r = modifyMVar v $ \st -> case Map.lookup (pk, r) (stReceipts st) of
  Nothing -> pure (st, [])
  Just i -> case Map.lookup (pk, i) (stMessages st) of
    Nothing -> pure (st { stReceipts = Map.delete (pk, r) (stReceipts st) }, [])
    Just p -> do
      now <- getCurrentTime
      let p' = p { prConfirmed = prConfirmed p + 1, prHeld = prHeld p - 1 }
          finished = prConfirmed p' >= length (parts (prText p'))
      pure ( st { stReceipts = Map.delete (pk, r) (stReceipts st)
                , stMessages = (if finished then Map.delete else (`Map.insert` p')) (pk, i) (stMessages st) }
           , [Confirmed (prContact p) i (prConfirmed p') now] )

-- | Tox forgets the parts it held for a friend that goes offline.
friendGone :: Outgoing -> PublicKey -> IO ()
friendGone (Outgoing v) pk = modifyMVar_ v $ \st -> pure st
  { stMessages = Map.mapWithKey (\(p, _) m -> if p == pk then m { prHeld = 0, prUnsent = False } else m) (stMessages st)
  , stReceipts = Map.filterWithKey (\(p, _) _ -> p /= pk) (stReceipts st)
  }

-- | Offer Tox again the parts its queue had no room for.
flush :: Tox.Tox -> Outgoing -> IO [Event]
flush tox o@(Outgoing v) = do
  unsent <- modifyMVar v $ \st -> pure (st, [ (key, p) | (key, p) <- Map.toList (stMessages st), prUnsent p ])
  concat <$> mapM (\((pk, i), p) -> send tox o (prContact p) pk i (prText p) 0) unsent
