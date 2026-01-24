module Tox.Android.MessagesSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as Char8
import           Data.Either           (isRight)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Test.Hspec
import           Test.QuickCheck

import           Tox.Android.Messages

spec :: Spec
spec = do
  it "splits into parts that make the message again, none too long" $
    property $ \(Positive n) s -> let limit = 4 + n `mod` 64; bytes = Text.encodeUtf8 (Text.pack s) in
      mconcat (split limit bytes) == bytes && all ((<= limit) . BS.length) (split limit bytes)

  it "never splits a character" $
    property $ \(Positive n) s -> let limit = 4 + n `mod` 64 in
      all (isRight . Text.decodeUtf8') (split limit (Text.encodeUtf8 (Text.pack s)))

  it "splits after a space in a part's second half" $
    split 10 (Char8.pack "aaaaaa bbbbbbb") `shouldBe` map Char8.pack ["aaaaaa ", "bbbbbbb"]
