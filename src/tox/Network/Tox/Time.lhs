\begin{code}
{-# LANGUAGE Safe #-}
module Network.Tox.Time where

import qualified System.Clock as Clock
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary)
import Data.Monoid (Monoid, mappend, mempty)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

newtype TimeStamp = TimeStamp Clock.TimeSpec
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

getTime :: IO TimeStamp
getTime = TimeStamp <$> Clock.getTime Clock.Monotonic

(-) :: TimeStamp -> TimeStamp -> TimeDiff
TimeStamp t - TimeStamp t' = TimeDiff $ t Prelude.- t'

(+) :: TimeStamp -> TimeDiff -> TimeStamp
TimeStamp t + TimeDiff t' = TimeStamp $ t Prelude.+ t'

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

instance Arbitrary TimeStamp
  where arbitrary = (TimeStamp <$>) $ Clock.TimeSpec <$> arbitrary <*> arbitrary

instance Arbitrary TimeDiff
  where arbitrary = (TimeDiff <$>) $ Clock.TimeSpec <$> arbitrary <*> arbitrary
\end{code}
