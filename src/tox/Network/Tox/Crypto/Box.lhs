\section{Box}

\begin{code}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.Crypto.Box
  ( PlainText (..)
  , CipherText
  , cipherText
  , unCipherText
  , decode
  , encode
  , decrypt, decryptR
  , encrypt, encryptR
  ) where

import           Control.Applicative               ((<$>), (<*>))
import qualified Crypto.Saltine.Core.Box           as Sodium (boxAfterNM,
                                                              boxOpenAfterNM)
import qualified Crypto.Saltine.Internal.ByteSizes as ByteSizes
import           Data.Binary                       (Binary, get, put)
import           Data.Binary.Get                   (Decoder (..), pushChunk,
                                                    runGetIncremental)
import           Data.Binary.Put                   (runPut)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as ByteString
import qualified Data.ByteString.Base16            as Base16
import qualified Data.ByteString.Lazy              as LazyByteString
import           Data.MessagePack                  (MessagePack (..))
import           Data.Typeable                     (Typeable)
import           GHC.Generics                      (Generic)
import           Network.MessagePack.Rpc           (Doc (..))
import qualified Network.MessagePack.Rpc           as Rpc
import           Test.QuickCheck.Arbitrary         (Arbitrary, arbitrary)
import           Text.Read                         (readPrec)

import           Network.Tox.Crypto.Key            (CombinedKey, Key (..),
                                                    Nonce)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


\end{code}

The Tox protocol differentiates between two types of text: Plain Text and
Cipher Text.  Cipher Text may be transmitted over untrusted data channels.
Plain Text can be Sensitive or Non Sensitive.  Sensitive Plain Text must be
transformed into Cipher Text using the encryption function before it can be
transmitted over untrusted data channels.

\begin{code}


newtype PlainText = PlainText { unPlainText :: ByteString }
  deriving (Eq, Binary, Generic, Typeable)

instance MessagePack PlainText

instance Show PlainText where
  show = show . Base16.encode . unPlainText

instance Read PlainText where
  readPrec = PlainText . fst . Base16.decode <$> readPrec


newtype CipherText = CipherText { unCipherText :: ByteString }
  deriving (Eq, Typeable)

cipherText :: Monad m => ByteString -> m CipherText
cipherText bs
  | ByteString.length bs >= ByteSizes.boxMac = return $ CipherText bs
  | otherwise                                = fail "ciphertext is too short"

instance Binary CipherText where
  put = put . unCipherText
  get = get >>= cipherText

instance MessagePack CipherText where
  toObject = toObject . unCipherText
  fromObject x = do
    bs <- fromObject x
    cipherText bs

instance Show CipherText where
  show = show . Base16.encode . unCipherText

instance Read CipherText where
  readPrec = fst . Base16.decode <$> readPrec >>= cipherText


encode :: Binary a => a -> PlainText
encode =
  PlainText . LazyByteString.toStrict . runPut . put


decode :: (Monad m, Binary a) => PlainText -> m a
decode (PlainText bytes) =
  finish $ pushChunk (runGetIncremental get) bytes
  where
    finish = \case
      Done _ _ output -> return output
      Fail _ _ msg    -> fail msg
      Partial f       -> finish $ f Nothing


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

encryptR :: Rpc.Rpc (CombinedKey -> Nonce -> PlainText -> Rpc.Returns CipherText)
encryptR =
  Rpc.stubs "Box.encrypt"
    (Arg "key" $ Arg "nonce" $ Arg "plain" $ Ret "encrypted")
    encrypt

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

decryptR :: Rpc.Rpc (CombinedKey -> Nonce -> CipherText -> Rpc.Returns (Maybe PlainText))
decryptR =
  Rpc.stubs "Box.decrypt"
    (Arg "key" $ Arg "nonce" $ Arg "encrypted" $ Ret "plain")
    decrypt

\end{code}

\texttt{crypto_box} uses xsalsa20 symmetric encryption and poly1305
authentication.

The create and handle request functions are the encrypt and decrypt functions
for a type of DHT packets used to send data directly to other DHT nodes.  To be
honest they should probably be in the DHT module but they seem to fit better
here.  TODO: What exactly are these functions?


\begin{code}


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary PlainText where
  arbitrary = PlainText . ByteString.pack <$> arbitrary


instance Arbitrary CipherText where
  arbitrary = encrypt <$> arbitrary <*> arbitrary <*> arbitrary

\end{code}
