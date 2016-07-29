{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.NodesRequestSpec where

import           Test.Hspec

import           Data.Proxy                   (Proxy (..))
import qualified Network.Tox.Crypto.KeyPair   as KeyPair
import           Network.Tox.DHT.NodesRequest (NodesRequest (..))
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy NodesRequest)
  binarySpec (Proxy :: Proxy NodesRequest)
  readShowSpec (Proxy :: Proxy NodesRequest)

  it "has a public key" $ do
    kp <- KeyPair.newKeyPair
    let req = NodesRequest (KeyPair.publicKey kp)
    requestedKey req `shouldBe` KeyPair.publicKey kp
