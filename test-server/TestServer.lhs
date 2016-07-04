\chapter{Testing}

The final part of the architecture is the test protocol. We use a
[MessagePack](http://msgpack.org) based RPC protocol to expose language agnostic
interfaces to internal functions. Using property based testing with random
inputs as well as specific edge case tests help ensure that an implementation of
the Tox protocol following the architecture specified in this document is
correct.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}
module Main (main) where

import           Control.Applicative            ((<$>))
import           System.Environment             (getArgs)
import           Text.Read                      (readMaybe)

import qualified Network.Tox.RPC                as RPC

import qualified Network.Tox.Binary             as Binary
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import qualified Network.Tox.Crypto.Nonce       as Nonce


services :: [RPC.Method IO]
services =
  [ Binary.decodeS
  , Binary.encodeS
  , Box.encryptS
  , Box.decryptS
  , CombinedKey.precomputeS
  , KeyPair.newKeyPairS
  , KeyPair.fromSecretKeyS
  , Nonce.newNonceS
  , Nonce.incrementS
  ]


main :: IO ()
main = map readMaybe <$> getArgs >>= \case
    [Just port] -> RPC.runServer port            services
    _           -> RPC.runServer RPC.defaultPort services
\end{code}
