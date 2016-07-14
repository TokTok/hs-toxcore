{-# LANGUAGE Trustworthy #-}
module Network.Tox.Protocol.PacketKindSpec where

import           Test.Hspec

import qualified Data.Binary                     as Binary (get)
import qualified Data.Binary.Get                 as Binary (Get)
import           Data.Proxy                      (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.Protocol.PacketKind (PacketKind)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy PacketKind)
  binarySpec (Proxy :: Proxy PacketKind)
  readShowSpec (Proxy :: Proxy PacketKind)

  it "should handle invalid packet kinds as failures" $ do
    expectDecoderFailure [0xfe] "No binary mapping for packet kind 254"
    expectDecoderFailure [0xff] "No binary mapping for packet kind 255"

  where
    expectDecoderFailure =
      expectDecoderFail (Binary.get :: Binary.Get PacketKind)
