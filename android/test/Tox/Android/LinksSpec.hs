{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.LinksSpec (spec) where

import           Test.Hspec

import           Tox.Android.Links

spec :: Spec
spec = do
  it "finds web and e-mail addresses" $
    findLinks "see https://example.com/a?b=c#d, www.example.org or me@x.co" `shouldBe`
      [ ("https://example.com/a?b=c#d", "https://example.com/a?b=c#d")
      , ("www.example.org", "https://www.example.org")
      , ("me@x.co", "mailto:me@x.co") ]

  it "leaves out punctuation at the end and unmatched brackets" $ do
    findLinks "(at https://en.wikipedia.org/wiki/Tox_(protocol))." `shouldBe`
      [("https://en.wikipedia.org/wiki/Tox_(protocol)", "https://en.wikipedia.org/wiki/Tox_(protocol)")]
    findLinks "'www.example.com/x'!" `shouldBe` [("www.example.com/x", "https://www.example.com/x")]

  it "links only web and e-mail addresses that start a word" $
    findLinks "awww.example.com xhttps://example.com file:///etc/passwd \
              \smb://host/share javascript:alert(1) https://localhost www. \
              \https://a_b.example_c.com www.example.com's me@x.co_" `shouldBe` []
