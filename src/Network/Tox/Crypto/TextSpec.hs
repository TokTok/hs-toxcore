{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.Crypto.TextSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString               as ByteString
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.Crypto.Text       (CipherText, PlainText (..))
import qualified Network.Tox.Crypto.Text       as Text
import           Network.Tox.EncodingSpec
import qualified Network.Tox.ExternalTest.Test as Test


spec :: Spec
spec = do
  jsonSpec (Proxy :: Proxy (CipherText Int))
  jsonSpec (Proxy :: Proxy (PlainText Int))
  binarySpec (Proxy :: Proxy (CipherText Int))
  binarySpec (Proxy :: Proxy (PlainText Int))
  readShowSpec (Proxy :: Proxy (CipherText Int))
  readShowSpec (Proxy :: Proxy (PlainText Int))

  it "encodes/decodes arbitrary texts" $
    property $ \(bytes :: String) ->
      Text.decode (Text.encode bytes) `shouldBe` Just bytes

  it "should return an error message in a monad that supports fail" $
    case Text.decode (PlainText (ByteString.pack [0x00])) of
      Test.Skipped         -> expectationFailure   "Expected failure, but got Skipped"
      Test.Success success -> expectationFailure $ "Expected failure, but got Success: " ++ success
      Test.Failure failure -> failure `shouldContain` "not enough bytes"
