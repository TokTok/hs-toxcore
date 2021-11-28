{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.SaveDataSpec where

import           Test.Hspec

import qualified Data.Binary              as Binary (get)
import qualified Data.Binary.Get          as Binary (Get)
import           Data.Proxy               (Proxy (..))
import           Network.Tox.EncodingSpec (binarySpec)
import qualified Network.Tox.EncodingSpec as EncodingSpec (expectDecoderFail)
import           Network.Tox.SaveData     (SaveData)


spec :: Spec
spec = do
  binarySpec (Proxy :: Proxy SaveData)

  it "should handle invalid magic numbers" $ do
    expectDecoderFail [0x00, 0x00, 0x00, 0x01]
      "savedata should start with 32 zero-bits"
    expectDecoderFail [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      "wrong magic number"

  where
    expectDecoderFail =
      EncodingSpec.expectDecoderFail (Binary.get :: Binary.Get SaveData)
