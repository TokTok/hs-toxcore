{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.NodesResponseSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                    (Proxy (..))
import           Network.Tox.DHT.NodesResponse (NodesResponse)
import qualified Network.Tox.DHT.NodesResponse as NodesResponse
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy NodesResponse)
  binarySpec (Proxy :: Proxy NodesResponse)
  readShowSpec (Proxy :: Proxy NodesResponse)
