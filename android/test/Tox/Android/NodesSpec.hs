{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.NodesSpec (spec) where

import qualified Data.ByteString.Char8 as Char8
import           Test.Hspec

import           Tox.Android.Nodes

key :: String
key = concat (replicate 8 "3B8EEA1F")

spec :: Spec
spec = do
  it "reads the nodes nodes.tox.chat lists, those relaying over TCP first" $ do
    let list = Char8.pack $ concat
          [ "{\"last_scan\": 1, \"nodes\": ["
          , "{\"ipv4\": \"1.2.3.4\", \"ipv6\": \"-\", \"port\": 33445, \"tcp_ports\": [], \"public_key\": \"", key
          , "\", \"motd\": \"caf\\u00e9 \\\"x\\\"\", \"status_tcp\": false},"
          , "{\"ipv4\": \"-\", \"port\": 1, \"tcp_ports\": [1], \"public_key\": \"", key, "\"},"
          , "{\"ipv4\": \"5.6.7.8\", \"port\": 443, \"tcp_ports\": [3389, 443], \"public_key\": \"", key
          , "\", \"status_tcp\": true, \"last_ping\": 1.5e9}"
          , "]}" ]
    fmap (map nodeHost) (readNodes list) `shouldBe` Just ["1.2.3.4", "5.6.7.8"]
    fmap (map nodeTcpPort) (readNodes list) `shouldBe` Just [Nothing, Just 3389]
    fmap (map nodeHost . usable) (readNodes list) `shouldBe` Just ["5.6.7.8"]

  it "keeps the list it has over one without nodes or that doesn't read" $ do
    readNodes "{\"nodes\": []}" `shouldBe` Nothing
    readNodes "{\"nodes\": [" `shouldBe` Nothing
    readNodes "<html>" `shouldBe` Nothing
    length (usable builtIn) `shouldBe` length builtIn
