{-# LANGUAGE OverloadedStrings #-}
-- | A model with friends and chats, made by the events that would make it.
module Tox.Android.Sample
  ( alice
  , bob
  , carol
  , team
  , at
  , applying
  , history
  , sample
  ) where

import qualified Data.Text           as Text
import           Data.Time           (UTCTime (..), fromGregorian,
                                      secondsToDiffTime)

import qualified Tox.Application.Tox as Tox

import           Tox.Android.Model

alice, bob, carol :: ContactId
alice = ContactId (Text.replicate 8 "3B8EEA1F")
bob = ContactId (Text.replicate 8 "E8743B05")
carol = ContactId (Text.replicate 8 "19A974C2")

team :: GroupId
team = GroupId (Text.replicate 8 "C0FFEE00")

-- | A time in November 2026.
at :: Int -> Integer -> Integer -> UTCTime
at day hour minute = UTCTime (fromGregorian 2026 11 day)
  (secondsToDiffTime (hour * 3600 + minute * 60))

applying :: [Event] -> Model -> Model
applying events model = foldl (\m e -> fst (update e m)) model events

-- | Alice online with a chat, Bob offline, and a request from Carol.
sample :: Model
sample = applying history initial

-- | What made the sample.
history :: [Event]
history =
  [ Started (toxId (Text.replicate 8 "5A17ED00") "0BADF00D")
  , Requested alice "Hi, it's Alice"
  , Accept alice
  , FriendNamed alice "Alice"
  , FriendOnline alice True
  , Received alice (at 1 15 14) Tox.MessageNormal "Hi! Is this the Haskell client?"
  , Open (Chat alice)
  , DraftChanged "It is, and it's on the network now."
  , Send (at 1 15 15)
  , Confirmed alice 1 1 (at 1 15 16)
  , Back
  , Received alice (at 2 9 2) Tox.MessageNormal "Looks good already."
  , Requested bob "Hi, it's Bob"
  , Accept bob
  , FriendNamed bob "Bob"
  , Requested carol "Hey, I'm Carol, come chat with me on Tox!"
  ]
