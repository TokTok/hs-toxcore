module Tox.Android.FilesSpec (spec) where

import           Test.Hspec

import           Tox.Android.Files

spec :: Spec
spec =
  it "cuts a long name to what a file system takes, keeping its extension" $ do
    let name = replicate 251 'a' ++ ".pdf"
        path = fitted "1793830661369-0-" name
    length path `shouldBe` 255
    drop 251 path `shouldBe` ".pdf"
    fitted "1-0-" "short.txt" `shouldBe` "1-0-short.txt"
