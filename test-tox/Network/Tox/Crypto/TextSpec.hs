{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.Crypto.TextSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString          as ByteString
import           Data.Proxy               (Proxy (..))
import           Network.Tox.Crypto.Text  (CipherText, PlainText (..))
import qualified Network.Tox.Crypto.Text  as Text
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy CipherText)
  rpcSpec (Proxy :: Proxy PlainText)
  binarySpec (Proxy :: Proxy CipherText)
  binarySpec (Proxy :: Proxy PlainText)
  readShowSpec (Proxy :: Proxy CipherText)
  readShowSpec (Proxy :: Proxy PlainText)

  it "encodes/decodes arbitrary texts" $
    property $ \(bytes :: String) ->
      Text.decode (Text.encode bytes) `shouldBe` Just bytes

  {-
  it "should return an error message in a monad that supports fail" $
    case Text.decode (PlainText (ByteString.pack [0x00])) of
      Test.Skipped         -> expectationFailure   "Expected failure, but got Skipped"
      Test.Success success -> expectationFailure $ "Expected failure, but got Success: " ++ success
      Test.Failure failure -> failure `shouldContain` "not enough bytes"
  -}
