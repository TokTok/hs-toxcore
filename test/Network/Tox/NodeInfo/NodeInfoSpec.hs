{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.NodeInfo.NodeInfoSpec where

import           Test.Hspec

import qualified Data.Binary                   as Binary (get)
import qualified Data.Binary.Get               as Binary (Get)
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.EncodingSpec
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy NodeInfo)
  binarySpec (Proxy :: Proxy NodeInfo)
  readShowSpec (Proxy :: Proxy NodeInfo)

  it "should handle invalid packets as failures" $ do
    expectDecoderFailure [0x20] "Invalid address family: 32"
    expectDecoderFailure [0xa0] "Invalid address family: 32"
    expectDecoderFailure [0x00] "Invalid address family: 0"
    expectDecoderFailure [0x01] "Invalid address family: 1"

  where
    expectDecoderFailure =
      expectDecoderFail (Binary.get :: Binary.Get NodeInfo)
