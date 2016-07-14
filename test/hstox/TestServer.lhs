\chapter{Testing}

The final part of the architecture is the test protocol. We use a
[MessagePack](http://msgpack.org) based RPC protocol to expose language agnostic
interfaces to internal functions. Using property based testing with random
inputs as well as specific edge case tests help ensure that an implementation of
the Tox protocol following the architecture specified in this document is
correct.

\begin{code}
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
  , Box.decryptS
  , Box.encryptS
  , CombinedKey.precomputeS
  , KeyPair.fromSecretKeyS
  , KeyPair.newKeyPairS
  , Nonce.incrementS
  , Nonce.newNonceS
  ]


main :: IO ()
main = map readMaybe <$> getArgs >>= \case
    [Just port] -> RPC.runServer port            services
    _           -> RPC.runServer RPC.defaultPort services
\end{code}

See the \href{https://github.com/msgpack/msgpack/blob/master/spec.md}{spec} of
msgpack for information on the binary representation.
