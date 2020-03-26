{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.HostAddressSpec where

import           Test.Hspec

import           Data.Proxy                       (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.HostAddress (HostAddress)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy HostAddress)
  binarySpec (Proxy :: Proxy HostAddress)
  readShowSpec (Proxy :: Proxy HostAddress)
