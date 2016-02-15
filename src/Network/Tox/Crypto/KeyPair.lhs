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
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.KeyPair where

import           Control.Applicative            ((<$>))
import qualified Crypto.Saltine.Class           as Sodium (decode, encode)
import qualified Crypto.Saltine.Core.Box        as Sodium (newKeypair)
import qualified Crypto.Saltine.Core.ScalarMult as Sodium (multBase)
import           Network.Tox.Crypto.Key         (Key (..))
import qualified Network.Tox.Crypto.Key         as Key
import           Test.QuickCheck.Arbitrary      (Arbitrary, arbitrary)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype KeyPair = KeyPair (Key.SecretKey, Key.PublicKey)
  deriving (Eq, Show, Read)


newKeyPair :: IO KeyPair
newKeyPair = do
  (secretKey, publicKey) <- Sodium.newKeypair
  return $ KeyPair (Key secretKey, Key publicKey)


fromSecretKey :: Key.SecretKey -> KeyPair
fromSecretKey secretKey =
  let
    Key sk = secretKey
    skBytes = Sodium.encode sk
    Just skScalar = Sodium.decode skBytes
    pkGroupElement = Sodium.multBase skScalar
    pkBytes = Sodium.encode pkGroupElement
    Just pk = Sodium.decode pkBytes
  in
  KeyPair (secretKey, Key pk)


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary KeyPair where
  arbitrary =
    fromSecretKey <$> arbitrary
\end{code}
