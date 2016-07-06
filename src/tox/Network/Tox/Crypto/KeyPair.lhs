\subsection{Key Pair}

A Key Pair is a pair of Secret Key and Public Key.  A new key pair is generated
using the \texttt{crypto_box_keypair} function of the NaCl crypto library.  Two
separate calls to the key pair generation function must return distinct key
pairs.  See the \href{https://nacl.cr.yp.to/box.html}{NaCl documentation} for
details.

A Public Key can be computed from a Secret Key using the NaCl function
\texttt{crypto_scalarmult_base}, which computes the scalar product of a
standard group element and the Secret Key.  See the
\href{https://nacl.cr.yp.to/scalarmult.html}{NaCl documentation} for details.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE Trustworthy        #-}
module Network.Tox.Crypto.KeyPair where

import           Control.Applicative            ((<$>))
import qualified Crypto.Saltine.Class           as Sodium (decode, encode)
import qualified Crypto.Saltine.Core.Box        as Sodium (newKeypair)
import qualified Crypto.Saltine.Core.ScalarMult as Sodium (multBase)
import           Data.Binary                    (Binary)
import           Data.MessagePack               (MessagePack (..))
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import           Test.QuickCheck.Arbitrary      (Arbitrary, arbitrary)

import           Network.Tox.Crypto.Key         (Key (..))
import qualified Network.Tox.Crypto.Key         as Key
import qualified Network.Tox.RPC                as RPC


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data KeyPair = KeyPair
  { secretKey :: Key.SecretKey
  , publicKey :: Key.PublicKey
  }
  deriving (Eq, Show, Read, Generic, Typeable)

instance Binary KeyPair
instance MessagePack KeyPair


newKeyPair :: IO KeyPair
newKeyPair = do
  (sk, pk) <- Sodium.newKeypair
  return $ KeyPair (Key sk) (Key pk)

newKeyPairC :: RPC.Client KeyPair
newKeyPairS :: RPC.Method IO
(newKeyPairC, newKeyPairS) = RPC.stubs "KeyPair.newKeyPair" RPC.ioFun0 newKeyPair


fromSecretKey :: Key.SecretKey -> KeyPair
fromSecretKey sk =
  let
    skBytes = Sodium.encode sk
    Just skScalar = Sodium.decode skBytes
    pkGroupElement = Sodium.multBase skScalar
    pkBytes = Sodium.encode pkGroupElement
    Just pk = Sodium.decode pkBytes
  in
  KeyPair sk pk

fromSecretKeyC :: Key.SecretKey -> RPC.Client KeyPair
fromSecretKeyS :: RPC.Method IO
(fromSecretKeyC, fromSecretKeyS) = RPC.stubs "KeyPair.fromSecretKey" RPC.fun1 fromSecretKey


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary KeyPair where
  arbitrary =
    fromSecretKey <$> arbitrary
\end{code}
