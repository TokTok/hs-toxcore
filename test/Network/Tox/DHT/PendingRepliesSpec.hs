{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.PendingRepliesSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Network.Tox.DHT.PendingReplies as PendingReplies
import qualified Network.Tox.DHT.Stamped        as Stamped

spec :: Spec
spec = do
  it "Accepts a response with the same RequestID if sent since the cutoff" $
    property $ \time time' node requestID ->
      let
        expecting = PendingReplies.expectReply time node requestID Stamped.empty
      in
        fst (PendingReplies.checkExpectedReply time' node requestID expecting)
        `shouldBe` time' <= time

  it "Rejects a response with a different requestID" $
    property $ \time node requestID requestID' ->
      let
        expecting = PendingReplies.expectReply time node requestID Stamped.empty
      in
        fst (PendingReplies.checkExpectedReply time node requestID' expecting)
        `shouldBe` requestID == requestID'

  it "Doesn't accept the same response twice" $
    property $ \time node requestID ->
      let
        expecting = PendingReplies.expectReply time node requestID Stamped.empty
        accepted  = snd $ PendingReplies.checkExpectedReply time node requestID expecting
      in
        fst (PendingReplies.checkExpectedReply time node requestID accepted)
        `shouldBe` False
