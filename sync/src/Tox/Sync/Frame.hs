-- | Sync messages split into Tox custom lossless packets, and joined again.
module Tox.Sync.Frame
  ( packetId
  , maxMessage
  , frames
  , Reassembler
  , reassembler
  , push
  ) where

import           Data.Bits       ((.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Word       (Word8)

packetId :: Word8
packetId = 0xb0

maxMessage :: Int
maxMessage = 1024 * 1024

flagFinal, flagFirst :: Word8
flagFinal = 1
flagFirst = 2

-- | What fits in one of Tox's custom packets after the header.
maxPayload :: Int
maxPayload = 1373 - 2

-- | A message as packets: a first frame starts it, a final one ends it.
frames :: BS.ByteString -> [BS.ByteString]
frames message = zipWith frame [0 ..] chunks
  where
    chunks = takeWhile (not . BS.null) (map (\i -> BS.take maxPayload (BS.drop (i * maxPayload) message)) [0 ..])
    lastChunk = length chunks - 1
    frame i chunk = BS.cons packetId (BS.cons (flags i) chunk)
    flags i = (if i == (0 :: Int) then flagFirst else 0) .|. (if i == lastChunk then flagFinal else 0)

-- | Frames of a message so far, or whether the rest of one is dropped.
data Reassembler = Reassembler [BS.ByteString] Int Bool

reassembler :: Reassembler
reassembler = Reassembler [] 0 False

-- | The message, once its final frame has arrived.
push :: BS.ByteString -> Reassembler -> Either (String, Reassembler) (Maybe BS.ByteString, Reassembler)
push packet r@(Reassembler parts size discarding)
  | BS.length packet < 2 || BS.head packet /= packetId = Left ("Not a sync packet", r)
  | otherwise = step (if flags .&. flagFirst /= 0 then reassembler else Reassembler parts size discarding)
  where
    flags = BS.index packet 1
    payload = BS.drop 2 packet
    final = flags .&. flagFinal /= 0
    step (Reassembler ps n dropping)
      -- Left from a connection that ended mid-message.
      | dropping = Right (Nothing, Reassembler [] 0 (not final))
      | n + BS.length payload > maxMessage = Left ("Sync message too long", Reassembler [] 0 (not final))
      | final = Right (Just (BS.concat (reverse (payload : ps))), reassembler)
      | otherwise = Right (Nothing, Reassembler (payload : ps) (n + BS.length payload) False)
