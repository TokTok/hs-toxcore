{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Core.TimeSpec (spec) where

import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import qualified Tox.Core.Time   as Time

spec :: Spec
spec = do
  describe "TimeDiff" $ do
    it "can be added and subtracted" $ property $
      \(s1 :: Integer) (s2 :: Integer) ->
        let t1 = Time.seconds s1
            t2 = Time.seconds s2
            t_sum = t1 + t2
            t_diff = t_sum - t2
        in t_diff == t1

    it "honors fromInteger as nanoseconds" $
      let d = 5 :: Time.TimeDiff
      in d `shouldBe` Time.milliseconds 0 Prelude.+ 5

  describe "Timestamp" $ do
    it "can add a TimeDiff" $ property $
      \(s1 :: Integer) (s2 :: Integer) ->
        let ts = Time.Timestamp (fromInteger s1) -- normalized via Num
            td = Time.seconds s2
            ts_plus = ts `Time.addTime` td
            td_recovered = ts_plus `Time.diffTime` ts
        in td_recovered == td
