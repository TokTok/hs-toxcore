\subsection{Combined Key}

\begin{code}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.CombinedKey where

import qualified Crypto.Saltine.Core.Box as Sodium (beforeNM)
import           Network.MessagePack.Rpc (Doc (..))
import qualified Network.MessagePack.Rpc as Rpc

import           Network.Tox.Crypto.Key  (CombinedKey, Key (..), PublicKey,
                                          SecretKey)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A Combined Key is computed from a Secret Key and a Public Key using the NaCl
function \texttt{crypto_box_beforenm}.  Given two Key Pairs KP1 (SK1, PK1) and
KP2 (SK2, PK2), the Combined Key computed from (SK1, PK2) equals the one
computed from (SK2, PK1).  This allows for symmetric encryption, as peers can
derive the same shared key from their own secret key and their peer's public
key.

\begin{code}

precompute :: SecretKey -> PublicKey -> CombinedKey
precompute (Key sk) (Key pk) =
  Key $ Sodium.beforeNM sk pk


precomputeR :: Rpc.Rpc (SecretKey -> PublicKey -> Rpc.Returns CombinedKey)
precomputeR =
  Rpc.stubs "CombinedKey.precompute"
    (Arg "sk" $ Arg "pk" $ Ret "key")
    precompute


\end{code}

In the Tox protocol, packets are encrypted using the public key of the receiver
and the secret key of the sender.  The receiver decrypts the packets using the
receiver's secret key and the sender's public key.

The fact that the same key is used to encrypt and decrypt packets on both sides
means that packets being sent could be replayed back to the sender if there is
nothing to prevent it.

The shared key generation is the most resource intensive part of the
encryption/decryption which means that resource usage can be reduced
considerably by saving the shared keys and reusing them later as much as
possible.
