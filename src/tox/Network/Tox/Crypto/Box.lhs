\section{Box}

\begin{code}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.Box where

import           Control.Applicative     ((<$>))
import qualified Crypto.Saltine.Core.Box as Sodium (boxAfterNM, boxOpenAfterNM)

import           Network.Tox.Crypto.Key  (CombinedKey, Key (..), Nonce)
import           Network.Tox.Crypto.Text (CipherText (..), PlainText (..))
import qualified Network.Tox.RPC         as RPC


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

The encryption function takes a Combined Key, a Nonce, and a Plain Text, and
returns a Cipher Text.  It uses \texttt{crypto_box_afternm} to perform the
encryption.  The meaning of the sentence "encrypting with a secret key, a
public key, and a nonce" is: compute a combined key from the secret key and the
public key and then use the encryption function for the transformation.

\begin{code}

encrypt :: CombinedKey -> Nonce -> PlainText -> CipherText
encrypt (Key ck) (Key nonce) (PlainText bytes) =
  CipherText $ Sodium.boxAfterNM ck nonce bytes

encryptC :: CombinedKey -> Nonce -> PlainText -> RPC.Client CipherText
encryptS :: RPC.Method IO
(encryptC, encryptS) = RPC.stubs "Box.encrypt" RPC.fun3 encrypt

\end{code}

The decryption function takes a Combined Key, a Nonce, and a Cipher Text, and
returns either a Plain Text or an error.  It uses
\texttt{crypto_box_open_afternm} from the NaCl library.  Since the cipher is
symmetric, the encryption function can also perform decryption, but will not
perform message authentication, so the implementation must be careful to use
the correct functions.

\begin{code}

decrypt :: CombinedKey -> Nonce -> CipherText -> Maybe PlainText
decrypt (Key ck) (Key nonce) (CipherText bytes) =
  PlainText <$> Sodium.boxOpenAfterNM ck nonce bytes

decryptC :: CombinedKey -> Nonce -> CipherText -> RPC.Client (Maybe PlainText)
decryptS :: RPC.Method IO
(decryptC, decryptS) = RPC.stubs "Box.decrypt" RPC.fun3 decrypt

\end{code}

\texttt{crypto_box} uses xsalsa20 symmetric encryption and poly1305
authentication.

The create and handle request functions are the encrypt and decrypt functions
for a type of DHT packets used to send data directly to other DHT nodes.  To be
honest they should probably be in the DHT module but they seem to fit better
here.  TODO: What exactly are these functions?
