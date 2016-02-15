\section{Key}

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.Crypto.Key where

import           Control.Applicative               ((<$>))
import qualified Crypto.Saltine.Class              as Sodium (IsEncoding,
                                                              decode, encode)
import qualified Crypto.Saltine.Core.Box           as Sodium (CombinedKey,
                                                              Nonce, PublicKey,
                                                              SecretKey)
import qualified Crypto.Saltine.Internal.ByteSizes as Sodium (boxBeforeNM,
                                                              boxNonce, boxPK,
                                                              boxSK)
import           Data.Aeson                        (FromJSON, ToJSON)
import qualified Data.Aeson                        as Aeson (parseJSON, toJSON)
import           Data.Binary                       (Binary)
import qualified Data.Binary                       as Binary (get, put)
import qualified Data.Binary.Get                   as Binary (getByteString,
                                                              runGet)
import qualified Data.Binary.Put                   as Binary (putByteString)
import qualified Data.ByteString                   as ByteString
import qualified Data.ByteString.Base16            as Base16
import qualified Data.ByteString.Char8             as Char8
import qualified Data.ByteString.Lazy              as LazyByteString
import           Data.Proxy                        (Proxy (..))
import           Test.QuickCheck.Arbitrary         (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Arbitrary         as Arbitrary
import           Text.Read                         (readPrec)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A Crypto Number is a large fixed size unsigned (positive) integer.  Its binary
encoding is as a Big Endian integer in exactly the encoded byte size.  Its
human-readable encoding is as a base-16 number encoded as String.  The NaCl
implementation \href{https://github.com/jedisct1/libsodium}{libsodium} supplies
the functions \texttt{sodium_bin2hex} and \texttt{sodium_hex2bin} to aid in
implementing the human-readable encoding.  The in-memory encoding of these
crypto numbers in NaCl already satisfies the binary encoding, so for
applications directly using those APIs, binary encoding and decoding is the
\href{https://en.wikipedia.org/wiki/Identity_function}{identity function}.

\begin{code}

class Sodium.IsEncoding a => CryptoNumber a where
  encodedByteSize :: Proxy a -> Int

\end{code}

Tox uses four kinds of Crypto Numbers:

\begin{tabular}{l|l|l}
  Type         & Bits & Encoded byte size \\
  \hline
  Public Key   & 256  & 32 \\
  Secret Key   & 256  & 32 \\
  Combined Key & 256  & 32 \\
  Nonce        & 192  & 24 \\
\end{tabular}

\begin{code}

instance CryptoNumber Sodium.PublicKey   where { encodedByteSize Proxy = Sodium.boxPK       }
instance CryptoNumber Sodium.SecretKey   where { encodedByteSize Proxy = Sodium.boxSK       }
instance CryptoNumber Sodium.CombinedKey where { encodedByteSize Proxy = Sodium.boxBeforeNM }
instance CryptoNumber Sodium.Nonce       where { encodedByteSize Proxy = Sodium.boxNonce    }


newtype Key a = Key a
  deriving (Eq)

type PublicKey   = Key Sodium.PublicKey
type SecretKey   = Key Sodium.SecretKey
type CombinedKey = Key Sodium.CombinedKey
type Nonce       = Key Sodium.Nonce

instance Sodium.IsEncoding a => Sodium.IsEncoding (Key a) where
  encode = Sodium.encode . unwrap
  decode = fmap Key . Sodium.decode


unwrap :: Key a -> a
unwrap (Key key) = key


keyToInteger :: Sodium.IsEncoding a => Key a -> Integer
keyToInteger =
  Binary.runGet Binary.get . encode
  where
    prefix = LazyByteString.pack
      [ 0x01 -- Tag: big integer
      , 0x01 -- Sign: positive
      , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20 -- Length: 32 bytes
      ]
    encode =
      LazyByteString.append prefix
        . LazyByteString.reverse
        . LazyByteString.fromStrict
        . Sodium.encode


decode :: (CryptoNumber a, Monad m) => ByteString.ByteString -> m (Key a)
decode bytes =
  case Sodium.decode bytes of
    Nothing  -> fail $ "Unable to decode ByteString: " ++ show bytes
    Just key -> return $ Key key


instance CryptoNumber a => Binary (Key a) where
  put (Key key) =
    Binary.putByteString $ Sodium.encode key

  get = do
    bytes <- Binary.getByteString $ encodedByteSize (Proxy :: Proxy a)
    decode bytes


instance CryptoNumber a => Show (Key a) where
  show (Key key) = show $ Base16.encode $ Sodium.encode key

instance CryptoNumber a => Read (Key a) where
  readPrec = fst . Base16.decode <$> readPrec >>= decode


instance CryptoNumber a => ToJSON (Key a) where
  toJSON = Aeson.toJSON . Char8.unpack . Base16.encode . Sodium.encode

instance CryptoNumber a => FromJSON (Key a) where
  parseJSON value = fst . Base16.decode . Char8.pack <$> Aeson.parseJSON value >>= decode


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance CryptoNumber a => Arbitrary (Key a) where
  arbitrary = do
    bytes <- fmap ByteString.pack $ Arbitrary.vector $ encodedByteSize (Proxy :: Proxy a)
    decode bytes
\end{code}
