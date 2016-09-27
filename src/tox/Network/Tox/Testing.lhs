\chapter{Testing}

The final part of the architecture is the test protocol. We use a
\href{http://msgpack.org}{MessagePack} based RPC protocol to expose language
agnostic interfaces to internal functions. Using property based testing with
random inputs as well as specific edge case tests help ensure that an
implementation of the Tox protocol following the architecture specified in this
document is correct.

See the \href{https://github.com/msgpack/msgpack/blob/master/spec.md}{spec} of
msgpack for information on the binary representation.

\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}
module Network.Tox.Testing (serve, defaultPort) where

import           Control.Applicative            ((<$>))
import qualified Network.MessagePack.Rpc        as Rpc
import qualified Network.MessagePack.Server     as Server
import           System.Environment             (getArgs)
import           Text.Read                      (readMaybe)

import qualified Network.Tox.Binary             as Binary
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import qualified Network.Tox.Crypto.KeyPair     as KeyPair
import qualified Network.Tox.Crypto.Nonce       as Nonce


defaultPort :: Int
defaultPort = 1234


services :: [Server.Method IO]
services =
  [ Binary.decodeS
  , Binary.encodeS
  , Rpc.method Box.decryptR
  , Rpc.method Box.encryptR
  , Rpc.method CombinedKey.precomputeR
  , Rpc.method KeyPair.fromSecretKeyR
  , Rpc.method KeyPair.newKeyPairR
  , Rpc.method Nonce.incrementR
  , Rpc.method Nonce.newNonceR
  ]


serve :: IO ()
serve = map readMaybe <$> getArgs >>= \case
    [Just port] -> Server.runServer port        services
    _           -> Server.runServer defaultPort services
\end{code}

TODO(iphydf): Generate and add specifications of each test method here.
