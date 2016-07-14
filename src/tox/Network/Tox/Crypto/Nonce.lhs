\subsection{Nonce}

A random nonce is generated using the cryptographically secure random number
generator from the NaCl library \texttt{randombytes}.

A nonce is incremented by interpreting it as a Big Endian number and adding 1.
If the nonce has the maximum value, the value after the increment is 0.

Most parts of the protocol use random nonces.  This prevents new nonces from
being associated with previous nonces.  If many different packets could be tied
together due to how the nonces were generated, it might for example lead to
tying DHT and onion announce packets together.  This would introduce a flaw in
the system as non friends could tie some people's DHT keys and long term keys
together.

\begin{code}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.Nonce where

import           Control.Applicative     ((<$>))
import qualified Crypto.Saltine.Class    as Sodium (decode, encode, nudge)
import qualified Crypto.Saltine.Core.Box as Sodium (newNonce)
import qualified Data.ByteString         as ByteString
import           Network.Tox.Crypto.Key
import qualified Network.Tox.RPC         as RPC


newNonce :: IO Nonce
newNonce = Key <$> Sodium.newNonce

newNonceC :: RPC.Client Nonce
newNonceS :: RPC.Method IO
(newNonceC, newNonceS) = RPC.stubs "Nonce.newNonce" RPC.ioFun0 newNonce


reverseNonce :: Nonce -> Nonce
reverseNonce (Key nonce) =
  let Just reversed = Sodium.decode $ ByteString.reverse $ Sodium.encode nonce in
  Key reversed


nudge :: Nonce -> Nonce
nudge =
  Key . Sodium.nudge . unKey


increment :: Nonce -> Nonce
increment =
  reverseNonce . nudge . reverseNonce

incrementC :: Nonce -> RPC.Client Nonce
incrementS :: RPC.Method IO
(incrementC, incrementS) = RPC.stubs "Nonce.increment" RPC.fun1 increment

\end{code}
