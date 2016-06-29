{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.HostAddressSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Proxy                       (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.HostAddress (HostAddress)
import qualified Network.Tox.NodeInfo.HostAddress as HostAddress


spec :: Spec
spec = do
  jsonSpec (Proxy :: Proxy HostAddress)
  binarySpec (Proxy :: Proxy HostAddress)
  readShowSpec (Proxy :: Proxy HostAddress)
