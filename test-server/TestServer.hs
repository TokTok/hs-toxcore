{-# LANGUAGE Safe #-}
module Main (main) where

import qualified Network.Tox.RPC                as RPC

import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey


main :: IO ()
main = RPC.runServer
  [ Box.encryptS
  , Box.decryptS
  , CombinedKey.precomputeS
  ]
