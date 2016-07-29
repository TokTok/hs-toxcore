{-# LANGUAGE Trustworthy #-}
module Data.MessagePack.AssocSpec where

import           Test.Hspec

import           Data.MessagePack         (Assoc)
import           Data.Proxy               (Proxy (..))
import           Network.Tox.EncodingSpec


spec :: Spec
spec =
  readShowSpec (Proxy :: Proxy (Assoc Int))
