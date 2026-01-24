{-# LANGUAGE LambdaCase #-}
-- | Avatars going to and coming from friends, as Tox clients send them:
-- files of the avatar kind whose ID is the picture's SHA-256, of at most
-- 64 KiB, with an empty one meaning none. They are kept in memory.
module Tox.Android.Avatars
  ( Avatars
  , newAvatars
  , maxSize
  , send
  , offered
  , has
  , requested
  , received
  , cancelled
  , friendGone
  , flush
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar, readMVar)
import           Control.Monad           (void)
import qualified Crypto.Hash.SHA256      as SHA256
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Word               (Word64, Word8)
import qualified Tox.Application.Tox     as Tox
import           Tox.Crypto.Core.Key     (PublicKey)

import           Tox.Android.Model

data Avatar
  = Ours BS.ByteString [(Word64, Int)]
    -- ^ The picture, and what Tox asked for of it that its queue had no
    -- room for yet, in order.
  | Theirs ContactId BS.ByteString Word64 BS.ByteString
    -- ^ The hash and size it should have, and what came so far.

newtype Avatars = Avatars (MVar (Map (PublicKey, Tox.FileRef) Avatar))

newAvatars :: IO Avatars
newAvatars = Avatars <$> newMVar Map.empty

-- | The largest avatar sent or taken.
maxSize :: Int
maxSize = 64 * 1024

-- | Offer a friend our avatar, or say we have none.
send :: Tox.Tox -> Avatars -> PublicKey -> Maybe BS.ByteString -> IO ()
send tox (Avatars v) pk = \case
  Nothing -> void $ Tox.fileSend tox pk Tox.fileKindAvatar 0 Nothing BS.empty
  Just png -> do
    let hash = SHA256.hash png
        -- Some clients take an avatar with no name for none.
        name = Base16.encode hash <> BS.pack [0x2e, 0x70, 0x6e, 0x67]
    Tox.fileSend tox pk Tox.fileKindAvatar (fromIntegral (BS.length png)) (Just hash) name >>= \case
      Right n -> modifyMVar_ v (pure . Map.insert (pk, Tox.Outgoing n) (Ours png []))
      Left _  -> pure ()

-- | A friend offers its avatar: take it if it may be one, or learn it has
-- none.
offered :: Tox.Tox -> Avatars -> ContactId -> PublicKey -> Word8 -> Word64 -> BS.ByteString -> IO [Event]
offered tox (Avatars v) cid pk n size hash
  | size == 0 = [FriendAvatar cid Nothing] <$ refuse
  | size > fromIntegral maxSize = [] <$ refuse
  | otherwise = do
      modifyMVar_ v (pure . Map.insert (pk, Tox.Incoming n) (Theirs cid hash size BS.empty))
      [] <$ Tox.fileControl tox pk (Tox.Incoming n) Tox.FileResume
  where refuse = Tox.fileControl tox pk (Tox.Incoming n) Tox.FileCancel

-- | Whether a transfer is one of an avatar.
has :: Avatars -> PublicKey -> Tox.FileRef -> IO Bool
has (Avatars v) pk ref = Map.member (pk, ref) <$> readMVar v

-- | Tox asks for data of our avatar, or says the friend has all of it.
requested :: Tox.Tox -> Avatars -> PublicKey -> Word8 -> Word64 -> Int -> IO ()
requested tox av@(Avatars v) pk n position len
  | len == 0 = modifyMVar_ v (pure . Map.delete key)
  | otherwise = do
      modifyMVar_ v $ pure . Map.adjust (\case
        Ours png unsent -> Ours png (unsent ++ [(position, len)])
        other -> other) key
      flushOne tox av key
  where key = (pk, Tox.Outgoing n)

-- | Data of a friend's avatar, in order, or that it is all there: the
-- avatar, if it is what was offered.
received :: Tox.Tox -> Avatars -> PublicKey -> Word8 -> Word64 -> BS.ByteString -> IO [Event]
received tox (Avatars v) pk n position bytes = modifyMVar v $ \known -> case Map.lookup key known of
  Just (Theirs cid hash size got)
    | BS.null bytes ->
        pure ( Map.delete key known
             , [ FriendAvatar cid (Just got)
               | fromIntegral (BS.length got) == size, SHA256.hash got == hash ] )
    | position == fromIntegral (BS.length got) && BS.length got + BS.length bytes <= maxSize ->
        pure (Map.insert key (Theirs cid hash size (got <> bytes)) known, [])
    | otherwise -> do
        void $ Tox.fileControl tox pk (Tox.Incoming n) Tox.FileCancel
        pure (Map.delete key known, [])
  _ -> pure (known, [])
  where key = (pk, Tox.Incoming n)

-- | The friend cancelled a transfer.
cancelled :: Avatars -> PublicKey -> Tox.FileRef -> IO ()
cancelled (Avatars v) pk ref = modifyMVar_ v (pure . Map.delete (pk, ref))

-- | Tox ends a friend's transfers when it goes offline.
friendGone :: Avatars -> PublicKey -> IO ()
friendGone (Avatars v) pk = modifyMVar_ v (pure . Map.filterWithKey (\(p, _) _ -> p /= pk))

-- | Offer Tox again the data its queue had no room for.
flush :: Tox.Tox -> Avatars -> IO ()
flush tox av@(Avatars v) = do
  waiting <- Map.keys . Map.filter (\case Ours _ (_ : _) -> True; _ -> False) <$> readMVar v
  mapM_ (flushOne tox av) waiting

flushOne :: Tox.Tox -> Avatars -> (PublicKey, Tox.FileRef) -> IO ()
flushOne tox (Avatars v) key@(pk, ref) = modifyMVar_ v $ \known -> case (Map.lookup key known, ref) of
  (Just (Ours png unsent), Tox.Outgoing n) -> do
    left <- go n png unsent
    pure (Map.insert key (Ours png left) known)
  _ -> pure known
  where
    go n png = \case
      [] -> pure []
      unsent@((position, len) : rest) ->
        Tox.fileSendChunk tox pk n position (BS.take len (BS.drop (fromIntegral position) png)) >>= \case
          Left Tox.FileSendQueueFull -> pure unsent
          _ -> go n png rest
