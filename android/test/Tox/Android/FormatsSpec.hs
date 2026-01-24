module Tox.Android.FormatsSpec (spec) where

import qualified Data.ByteString     as BS
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Test.Hspec

import           Tox.Android.Formats

-- | A message as TRIfA writes it, with an ID.
trifa :: BS.ByteString -> BS.ByteString
trifa id' = Text.encodeUtf8 (Text.pack "Gr\252\223e \10003") <> BS.pack [0, 0] <> id' <> BS.pack [0x63, 0xb0, 0xcd, 0x00]

spec :: Spec
spec = do
  it "reads what TRIfA writes, keyed by its ID" $ do
    let id' = BS.cons 1 (BS.replicate 31 0)
    readWith formats (trifa id') `shouldBe` Reading (Text.pack "Gr\252\223e \10003") (Just id')

  it "leaves other messages as their text" $ do
    let plain = Text.encodeUtf8 (Text.pack "just text, long enough to have room")
    readWith formats plain `shouldBe` Reading (Text.pack "just text, long enough to have room") Nothing

  it "reads no format with none on" $
    readRepeat (readWith [] (trifa (BS.replicate 32 0))) `shouldBe` Nothing
