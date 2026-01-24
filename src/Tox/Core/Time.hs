{-# LANGUAGE StrictData #-}
module Tox.Core.Time where

import           Data.Word                 (Word64)
import qualified System.Clock              as Clock
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

newtype Timestamp = Timestamp Clock.TimeSpec
  deriving (Eq, Ord, Show, Read)

timestampToMicroseconds :: Timestamp -> Word64
timestampToMicroseconds (Timestamp ts) =
  (fromIntegral (Clock.sec ts) * 1000000) + (fromIntegral (Clock.nsec ts) `div` 1000)

fromSeconds :: Integer -> Timestamp
fromSeconds s = Timestamp $ Clock.TimeSpec (fromIntegral s) 0

fromMillis :: Integer -> Timestamp
fromMillis ms =
  let (s, ns) = ms `divMod` 1000
  in Timestamp $ Clock.TimeSpec (fromIntegral s) (fromIntegral ns * 1000000)

newtype TimeDiff = TimeDiff Clock.TimeSpec
  deriving (Eq, Ord, Show, Read)

instance Num TimeDiff where
  TimeDiff t + TimeDiff t' = TimeDiff $ t + t'
  TimeDiff t - TimeDiff t' = TimeDiff $ t - t'
  TimeDiff t * TimeDiff t' = TimeDiff $ t * t'
  negate (TimeDiff t) = TimeDiff $ negate t
  abs (TimeDiff t) = TimeDiff $ abs t
  signum (TimeDiff t) = TimeDiff $ signum t
  fromInteger = TimeDiff . fromInteger

seconds :: Integer -> TimeDiff
seconds s = TimeDiff $ Clock.TimeSpec (fromIntegral s) 0

milliseconds :: Integer -> TimeDiff
milliseconds = TimeDiff . Clock.TimeSpec 0 . (*10^(6::Integer)) . fromIntegral

toMilliseconds :: TimeDiff -> Integer
toMilliseconds (TimeDiff t) = Clock.toNanoSecs t `div` 1000000

getTime :: IO Timestamp
getTime = Timestamp <$> Clock.getTime Clock.Monotonic

diffTime :: Timestamp -> Timestamp -> TimeDiff
diffTime (Timestamp t) (Timestamp t') = TimeDiff $ t - t'

addTime :: Timestamp -> TimeDiff -> Timestamp
addTime (Timestamp t) (TimeDiff t') = Timestamp $ t + t'

-- | Whether the interval has passed since the time, or there was none.
due :: Timestamp -> TimeDiff -> Maybe Timestamp -> Bool
due now interval = maybe True (\t -> addTime t interval <= now)

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

instance Arbitrary Timestamp
  where arbitrary = (Timestamp <$>) $ Clock.TimeSpec <$> arbitrary <*> arbitrary

instance Arbitrary TimeDiff
  where arbitrary = (TimeDiff <$>) $ Clock.TimeSpec <$> arbitrary <*> arbitrary
