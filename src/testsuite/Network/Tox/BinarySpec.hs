{-# LANGUAGE Trustworthy #-}
module Network.Tox.BinarySpec where

import           Test.Hspec

import           Data.Proxy         (Proxy (..))

import qualified Network.Tox.Binary as Binary
import qualified Network.Tox.RPC    as RPC


spec :: Spec
spec =
  it "should decode encoded data" $ do
    encoded <- Binary.encodeM (Proxy :: Proxy String) (RPC.toObject "hello")
    decoded <- Binary.decodeM (Proxy :: Proxy String) encoded
    (decoded >>= RPC.fromObject) `shouldBe` Just "hello"
