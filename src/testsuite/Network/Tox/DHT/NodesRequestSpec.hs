{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.NodesRequestSpec where

import           Test.Hspec

import           Data.Proxy                   (Proxy (..))
import           Network.Tox.DHT.NodesRequest (NodesRequest)
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy NodesRequest)
  binarySpec (Proxy :: Proxy NodesRequest)
  readShowSpec (Proxy :: Proxy NodesRequest)
