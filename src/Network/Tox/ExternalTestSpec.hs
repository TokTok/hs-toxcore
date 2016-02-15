{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.ExternalTestSpec where

import           Test.Hspec                    (Spec, it)

import qualified Network.Tox.ExternalTest      as Test
import qualified Network.Tox.ExternalTest.Test as Test (Result (..), Test (..))


spec :: Spec
spec = do
  it "should handle Success and Failure tests as Success/Failure respectively" $ do
    Test.run Test.FailureTest () $ Test.Failure "Failure"
    Test.run Test.SuccessTest () $ Test.Success ()

  it "should handle Skipped tests as both Success and Failure" $ do
    Test.run Test.SkippedTest () $ Test.Failure ""
    Test.run Test.SkippedTest () $ Test.Success ()
    Test.run Test.SkippedTest () Test.Skipped
