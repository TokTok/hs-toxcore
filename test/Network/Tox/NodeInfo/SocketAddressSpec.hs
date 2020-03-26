{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.SocketAddressSpec where

import           Test.Hspec

import           Data.Proxy                         (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.SocketAddress (SocketAddress)
import qualified Network.Tox.NodeInfo.SocketAddress as SocketAddress


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy SocketAddress)
  binarySpec (Proxy :: Proxy SocketAddress)
  readShowSpec (Proxy :: Proxy SocketAddress)

  binaryGetPutSpec "{get,put}SocketAddress"
    SocketAddress.getSocketAddress
    (uncurry SocketAddress.putSocketAddress)
