{-# LANGUAGE Safe       #-}
{-# LANGUAGE StrictData #-}
module Network.Tox.DHT.Stamped where

import qualified Data.Foldable    as F
import           Data.List        ((\\))
import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Network.Tox.Time (Timestamp)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

-- | a collection of objects associated with a timestamp.
type Stamped a = Map Timestamp [a]

empty :: Stamped a
empty = Map.empty

-- | add a timestamped object. There is no requirement that the stamp be later
-- than that of previously added objects.
add :: Timestamp -> a -> Stamped a -> Stamped a
add time x = Map.insertWith (++) time [x]

delete :: Eq a => Timestamp -> a -> Stamped a -> Stamped a
delete time x = Map.adjust (\\ [x]) time

findStamps :: (a -> Bool) -> Stamped a -> [Timestamp]
findStamps p = Map.keys . Map.filter (any p)

dropOlder :: Timestamp -> Stamped a -> Stamped a
dropOlder time = Map.mapMaybeWithKey $
  \t x -> if t < time then Nothing else Just x

getList :: Stamped a -> [a]
getList = F.concat

popFirst :: Stamped a -> (Maybe (Timestamp, a), Stamped a)
popFirst stamped =
  case Map.toAscList stamped of
    [] -> (Nothing, stamped)
    assoc:assocs -> case assoc of
      (_, [])       -> popFirst $ Map.fromAscList assocs
      (stamp, [a])  -> (Just (stamp, a), Map.fromAscList assocs)
      (stamp, a:as) -> (Just (stamp, a), Map.fromAscList $ (stamp, as):assocs)

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}
