{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DistanceExtSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.DHT.Distance      as Distance
import qualified Network.Tox.ExternalTest      as Test
import qualified Network.Tox.ExternalTest.Test as Test (Result (..), Test (..))


spec :: Spec
spec =
  it "computes distances correctly (compare distance result)" $
    property $ \origin alice bob ->
      Test.run Test.Distance (origin, alice, bob) $
        Test.Success (compare (Distance.xorDistance origin alice)
                              (Distance.xorDistance origin bob))
