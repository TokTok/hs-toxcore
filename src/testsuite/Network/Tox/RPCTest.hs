{-# LANGUAGE LambdaCase #-}
module Network.Tox.RPCTest where

import           Control.Exception          (catch)
import qualified Data.Text                  as Text
import           Test.Hspec

import           Data.MessagePack           (Object (..))
import           Network.MessagePack.Client (Client, RpcError (..))
import           Network.Tox.RPC            (runClient)


runTest :: Client a -> IO ()
runTest c =
  runClient c `catch` \case
    ServerError (ObjectStr msg) | msg == Text.pack "Pending" -> pending
    e -> expectationFailure $ show e
