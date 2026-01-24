{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Onion.TunnelSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.Binary                    (decode, encode)
import qualified Data.ByteString.Lazy           as LBS

import           Tox.Network.Core.SocketAddress (SocketAddress (..))
import           Tox.Onion.Tunnel

spec :: Spec
spec = do
  describe "OnionIPPort" $ do
    prop "roundtrips Binary" $ \(x :: OnionIPPort) ->
      decode (encode x) == x

    it "has fixed size of 19 bytes" $ property $ \(x :: OnionIPPort) ->
      LBS.length (encode x) == 19

    it "puts IPv4 octets in order" $
      encode (OnionIPPort $ SocketAddress (read "\"127.0.0.1\"") 33445)
        `shouldBe` LBS.pack ([2, 127, 0, 0, 1] ++ replicate 12 0 ++ [0x82, 0xa5])

  describe "OnionRequest0" $
    prop "roundtrips Binary" $ \(x :: OnionRequest0) ->
      decode (encode x) == x

  describe "OnionRequestPayload" $
    prop "roundtrips Binary" $ \(x :: OnionRequestPayload) ->
      decode (encode x) == x
