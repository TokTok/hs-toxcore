{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.RelaysSpec (spec) where

import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Test.Hspec

import           Tox.Android.Relays

spec :: Spec
spec = do
  it "reads who a bot relayed, by the longest known name" $ do
    relayed [] "<Bob> a> b" `shouldBe` Just ("Bob", "a> b")
    relayed ["Bo", "Bob> a"] "<Bob> a> b" `shouldBe` Just ("Bob> a", "b")
    mapM_ (\t -> relayed [] t `shouldBe` Nothing) ["<> x", "<Bob>x", "Bob> x", "not relayed"]

  it "lists the names relayed, last seen first" $ do
    let at = posixSecondsToUTCTime
    relayedNames [] [(at 1, "<Bob> hi"), (at 2, "<Carol> yo"), (at 3, "<Bob> again"), (at 4, "not relayed")]
      `shouldBe` [("Bob", at 3), ("Carol", at 2)]
