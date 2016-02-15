module Network.Tox.NodeInfo.NodeInfoExtSpec where

import           Test.Hspec

import           Network.Tox.ExternalTest.ExternalEncodingSpec (externalEncodingSpec)
import qualified Network.Tox.ExternalTest.Test                 as Test (DataFormat (..))


spec :: Spec
spec = externalEncodingSpec Test.NodeInfo
