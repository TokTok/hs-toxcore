{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.EmojiSpec (spec) where

import           Test.Hspec

import           Tox.Android.Emoji

spec :: Spec
spec = do
  it "takes one to three emoji, of every kind keyboards send" $
    mapM_ (\t -> (t, emojiOnly t) `shouldBe` (t, True))
      [ "\x1F600", "\x1F44D\x1F3FD", "\x2764\xFE0F", "\x1F1F3\x1F1F1", "1\xFE0F\x20E3", "\x26A1"
      , "\x1F468\x200D\x1F469\x200D\x1F467", "\x1F3F4\xE0067\xE0062\xE0065\xE006E\xE0067\xE007F"
      , "\x1F600\x1F600\x1F600" ]

  it "leaves text, symbols shown as text and many emoji as they are" $
    mapM_ (\t -> (t, emojiOnly t) `shouldBe` (t, False))
      ["", "hi", "\x1F600 ok", "\xA9", "\x2764", "1", "\x1F600\x1F600\x1F600\x1F600", "\x1F3FD", "\x200D"]
