{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.NodesResponseSpec where

import           Test.Hspec

import           Data.Proxy                    (Proxy (..))
import           Network.Tox.DHT.NodesResponse (NodesResponse)
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy NodesResponse)
  binarySpec (Proxy :: Proxy NodesResponse)
  readShowSpec (Proxy :: Proxy NodesResponse)
