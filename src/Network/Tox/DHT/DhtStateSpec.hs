{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DhtStateSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                    (Proxy (..))
import           Network.Tox.DHT.DhtState      (DhtState)
import qualified Network.Tox.DHT.DhtState      as DhtState
import           Network.Tox.EncodingSpec
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy DhtState)

  it "adding and removing a node yields the same state" $
    property $ \dhtState nodeInfo ->
      let
        afterAdd    = DhtState.addNode dhtState nodeInfo
        afterRemove = DhtState.removeNode afterAdd $ NodeInfo.publicKey nodeInfo
      in
      afterRemove `shouldBe` dhtState
