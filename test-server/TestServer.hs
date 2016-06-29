{-# LANGUAGE Safe #-}
module Main (main) where

import qualified Network.Tox.RPC                as RPC

import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import qualified Network.Tox.Crypto.Nonce       as Nonce


main :: IO ()
main = RPC.runServer
  [ Box.encryptS
  , Box.decryptS
  , CombinedKey.precomputeS
  , KeyPair.newKeyPairS
  , KeyPair.fromSecretKeyS
  , Nonce.newNonceS
  , Nonce.incrementS
  ]
