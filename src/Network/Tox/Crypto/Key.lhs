\section{Key}

\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
module Network.Tox.Crypto.Key where

import           Control.Monad                ((>=>))
import           Control.Monad.Validate       (MonadValidate, refute)
import qualified Crypto.Saltine.Class         as Sodium (IsEncoding, decode,
                                                         encode)
import qualified Crypto.Saltine.Core.Box      as Sodium (CombinedKey, Nonce,
                                                         PublicKey, SecretKey)
import qualified Crypto.Saltine.Core.Sign     as Sodium (Signature)
import qualified Crypto.Saltine.Internal.Box  as Sodium (box_beforenmbytes,
                                                         box_noncebytes,
                                                         box_publickeybytes,
                                                         box_secretkeybytes)
import qualified Crypto.Saltine.Internal.Sign as Sodium (sign_bytes)
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Binary (get, put)
import qualified Data.Binary.Get              as Binary (getByteString, runGet)
import qualified Data.Binary.Put              as Binary (putByteString)
import qualified Data.ByteString              as ByteString
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Lazy         as LazyByteString
import           Data.MessagePack             (DecodeError, MessagePack (..))
import           Data.Proxy                   (Proxy (..))
import           Data.String                  (fromString)
import           Data.Typeable                (Typeable)
import qualified Test.QuickCheck.Arbitrary    as Arbitrary
import           Test.QuickCheck.Arbitrary    (Arbitrary, arbitrary)
import           Text.Read                    (readPrec)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A Crypto Number is a large fixed size unsigned (non-negative) integer.  Its binary
encoding is as a Big Endian integer in exactly the encoded byte size.  Its
human-readable encoding is as a base-16 number encoded as String.  The NaCl
implementation \href{https://github.com/jedisct1/libsodium}{libsodium} supplies
the functions \texttt{sodium\_bin2hex} and \texttt{sodium\_hex2bin} to aid in
implementing the human-readable encoding.  The in-memory encoding of these
crypto numbers in NaCl already satisfies the binary encoding, so for
applications directly using those APIs, binary encoding and decoding is the
\href{https://en.wikipedia.org/wiki/Identity_function}{identity function}.

\begin{code}

class Sodium.IsEncoding a => CryptoNumber a where
  encodedByteSize :: proxy a -> Int

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

instance CryptoNumber Sodium.PublicKey   where { encodedByteSize _ = Sodium.box_publickeybytes }
instance CryptoNumber Sodium.SecretKey   where { encodedByteSize _ = Sodium.box_secretkeybytes }
instance CryptoNumber Sodium.CombinedKey where { encodedByteSize _ = Sodium.box_beforenmbytes  }
instance CryptoNumber Sodium.Nonce       where { encodedByteSize _ = Sodium.box_noncebytes     }
instance CryptoNumber Sodium.Signature   where { encodedByteSize _ = Sodium.sign_bytes         }

deriving instance Typeable Sodium.PublicKey
deriving instance Typeable Sodium.SecretKey
deriving instance Typeable Sodium.CombinedKey
deriving instance Typeable Sodium.Nonce
deriving instance Typeable Sodium.Signature

newtype Key a = Key { unKey :: a }
  deriving (Eq, Ord, Typeable)

type PublicKey   = Key Sodium.PublicKey
type SecretKey   = Key Sodium.SecretKey
type CombinedKey = Key Sodium.CombinedKey
type Nonce       = Key Sodium.Nonce
type Signature   = Key Sodium.Signature

instance Sodium.IsEncoding a => Sodium.IsEncoding (Key a) where
  encode = Sodium.encode . unKey
  decode = fmap Key . Sodium.decode


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


decode :: (CryptoNumber a, MonadValidate DecodeError m) => ByteString.ByteString -> m (Key a)
decode bytes =
  case Sodium.decode bytes of
    Just key -> return $ Key key
    Nothing  -> refute $ fromString $ "unable to decode ByteString to Key: " <> show (ByteString.length bytes)


instance CryptoNumber a => Binary (Key a) where
  put (Key key) =
    Binary.putByteString $ Sodium.encode key

  get = do
    bytes <- Binary.getByteString $ encodedByteSize (Proxy :: Proxy a)
    decode bytes


instance CryptoNumber a => Show (Key a) where
  show = show . Base16.encode . Sodium.encode . unKey

instance CryptoNumber a => Read (Key a) where
  readPrec = do
    text <- readPrec
    case Base16.decode text of
      Left err -> fail err
      Right ok -> decode ok

instance CryptoNumber a => MessagePack (Key a) where
  toObject cfg = toObject cfg . Sodium.encode
  fromObjectWith cfg = fromObjectWith cfg >=> decode


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance CryptoNumber a => Arbitrary (Key a) where
  arbitrary = do
    bytes <- fmap ByteString.pack $ Arbitrary.vector $ encodedByteSize (Proxy :: Proxy a)
    case Sodium.decode bytes of
      Just key -> return $ Key key
      Nothing  -> error "unable to decode ByteString to Key"
\end{code}
