{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Onion.RPCSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Data.Binary           (decode, encode)

import           Tox.Onion.RPC

spec :: Spec
spec = do
  describe "AnnounceRequestPayload" $
    prop "roundtrips Binary" $ \(x :: AnnounceRequestPayload) ->
      decode (encode x) == x

  describe "AnnounceRequest" $
    prop "roundtrips Binary" $ \(x :: AnnounceRequest) ->
      decode (encode x) == x

  describe "AnnounceResponsePayload" $
    prop "roundtrips Binary" $ \(x :: AnnounceResponsePayload) ->
      -- We only take up to 4 nodes in serialization
      let x' = x { announceResponseNodes = take 4 (announceResponseNodes x) }
      in decode (encode x') == x'

  describe "AnnounceResponse" $
    prop "roundtrips Binary" $ \(x :: AnnounceResponse) ->
      decode (encode x) == x

  describe "DataRouteRequest" $
    prop "roundtrips Binary" $ \(x :: DataRouteRequest) ->
      decode (encode x) == x

  describe "DataRouteResponse" $
    prop "roundtrips Binary" $ \(x :: DataRouteResponse) ->
      decode (encode x) == x

  describe "DataRouteInner" $
    prop "roundtrips Binary" $ \(x :: DataRouteInner) ->
      decode (encode x) == x

  describe "DHTPublicKeyPacket" $
    prop "roundtrips Binary" $ \(x :: DHTPublicKeyPacket) ->
      let x' = x { dhtPKPacketNodes = take 4 (dhtPKPacketNodes x) }
      in decode (encode x') == x'
