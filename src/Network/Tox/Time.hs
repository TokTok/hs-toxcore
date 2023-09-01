{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Time where

import qualified System.Clock              as Clock
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

newtype Timestamp = Timestamp Clock.TimeSpec
  deriving (Eq, Ord, Show, Read)

newtype TimeDiff = TimeDiff Clock.TimeSpec
  deriving (Eq, Ord, Show, Read)

instance Num TimeDiff where
  TimeDiff t + TimeDiff t' = TimeDiff $ t Prelude.+ t'
  TimeDiff t - TimeDiff t' = TimeDiff $ t Prelude.- t'
  TimeDiff t * TimeDiff t' = TimeDiff $ t * t'
  negate (TimeDiff t) = TimeDiff $ negate t
  abs (TimeDiff t) = TimeDiff $ abs t
  signum (TimeDiff t) = TimeDiff $ signum t
  fromInteger = TimeDiff . fromInteger

seconds :: Integer -> TimeDiff
seconds s = TimeDiff $ Clock.TimeSpec (fromIntegral s) 0

milliseconds :: Integer -> TimeDiff
milliseconds = TimeDiff . Clock.TimeSpec 0 . (*10^(6::Integer)) . fromIntegral

getTime :: IO Timestamp
getTime = Timestamp <$> Clock.getTime Clock.Monotonic

(-) :: Timestamp -> Timestamp -> TimeDiff
Timestamp t - Timestamp t' = TimeDiff $ t Prelude.- t'

(+) :: Timestamp -> TimeDiff -> Timestamp
Timestamp t + TimeDiff t' = Timestamp $ t Prelude.+ t'

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

instance Arbitrary Timestamp
  where arbitrary = (Timestamp <$>) $ Clock.TimeSpec <$> arbitrary <*> arbitrary

instance Arbitrary TimeDiff
  where arbitrary = (TimeDiff <$>) $ Clock.TimeSpec <$> arbitrary <*> arbitrary
