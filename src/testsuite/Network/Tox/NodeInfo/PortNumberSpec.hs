{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.PortNumberSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                      (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.PortNumber (PortNumber)
import qualified Network.Tox.NodeInfo.PortNumber as PortNumber


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy PortNumber)
  binarySpec (Proxy :: Proxy PortNumber)
  readShowSpec (Proxy :: Proxy PortNumber)
