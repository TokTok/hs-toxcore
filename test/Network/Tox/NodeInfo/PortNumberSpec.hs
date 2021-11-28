{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.PortNumberSpec where

import           Test.Hspec

import           Data.Proxy                      (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.PortNumber (PortNumber)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy PortNumber)
  binarySpec (Proxy :: Proxy PortNumber)
  readShowSpec (Proxy :: Proxy PortNumber)
