{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.NodesRequestSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                   (Proxy (..))
import           Network.Tox.DHT.NodesRequest (NodesRequest)
import qualified Network.Tox.DHT.NodesRequest as NodesRequest
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  jsonSpec (Proxy :: Proxy NodesRequest)
  binarySpec (Proxy :: Proxy NodesRequest)
  readShowSpec (Proxy :: Proxy NodesRequest)
