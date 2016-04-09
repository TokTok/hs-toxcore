{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy      #-}
module Network.Tox.ExternalTest.ExternalEncodingSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative           ((<$>))
import qualified Network.Tox.Crypto.Text       as Text (decode, encode)
import qualified Network.Tox.ExternalTest      as Test (run)
import qualified Network.Tox.ExternalTest.Test as Test (DataFormat (..),
                                                        Result (..), Test (..),
                                                        TestInput, construct,
                                                        deconstruct,
                                                        eraseFailure)


externalEncodingSpec :: Test.TestInput fmt => Test.DataFormat fmt -> Spec
externalEncodingSpec fmt = do
  it "should handle invalid encodings as failures" $
    property $ \plainText ->
      Test.run (Test.BinaryDecode fmt) plainText $ Test.eraseFailure $ Test.deconstruct <$> Text.decode plainText

  it "re-encodes values correctly" $
    property $ \expected ->
      Test.run (Test.BinaryDecode fmt) (Text.encode $ Test.construct expected) $ Test.Success expected

  it "encodes values correctly" $
    property $ \expected ->
      Test.run (Test.BinaryEncode fmt) expected $ Test.Success (Test.construct expected)


spec :: Spec
spec = do
  externalEncodingSpec Test.Word32
  externalEncodingSpec Test.String
  externalEncodingSpec Test.ByteString
