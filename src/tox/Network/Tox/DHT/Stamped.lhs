\begin{code}
{-# LANGUAGE Safe #-}
module Network.Tox.DHT.Stamped where

import qualified Data.Foldable    as F
import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Network.Tox.Time (Timestamp)

-- Stub module, awaiting a proper event loop architecture

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

dropOlder :: Timestamp -> Stamped a -> Stamped a
dropOlder time = Map.mapMaybeWithKey $
  \t x -> if t < time then Nothing else Just x

getList :: Stamped a -> [a]
getList = F.concat

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

\end{code}
