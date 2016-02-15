\section{Text}

The Tox protocol differentiates between two types of text: Plain Text and
Cipher Text.  Cipher Text may be transmitted over untrusted data channels.
Plain Text can be Sensitive or Non Sensitive.  Sensitive Plain Text must be
transformed into Cipher Text using the encryption function before it can be
transmitted over untrusted data channels.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.Crypto.Text where

import           Control.Applicative       ((<$>))
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Aeson                as Aeson (parseJSON, toJSON)
import           Data.Binary               (Binary, get, put)
import           Data.Binary.Get           (Decoder (..), pushChunk,
                                            runGetIncremental)
import           Data.Binary.Put           (runPut)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Char8     as Char8
import qualified Data.ByteString.Lazy      as LazyByteString
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Text.Read                 (readPrec)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype PlainText a = PlainText ByteString
  deriving (Eq, Binary)

instance Show (PlainText a) where
  show (PlainText bytes) = show $ Base16.encode bytes

instance Read (PlainText a) where
  readPrec = PlainText . fst . Base16.decode <$> readPrec

instance ToJSON (PlainText a) where
  toJSON (PlainText bytes) = Aeson.toJSON $ Char8.unpack $ Base16.encode bytes

instance FromJSON (PlainText a) where
  parseJSON value = PlainText . fst . Base16.decode . Char8.pack <$> Aeson.parseJSON value


newtype CipherText a = CipherText ByteString
  deriving (Eq, Binary)

instance Show (CipherText a) where
  show (CipherText bytes) = show $ Base16.encode bytes

instance Read (CipherText a) where
  readPrec = CipherText . fst . Base16.decode <$> readPrec

instance ToJSON (CipherText a) where
  toJSON (CipherText bytes) = Aeson.toJSON $ Char8.unpack $ Base16.encode bytes

instance FromJSON (CipherText a) where
  parseJSON value = CipherText . fst . Base16.decode . Char8.pack <$> Aeson.parseJSON value


encode :: Binary a => a -> PlainText a
encode =
  PlainText . LazyByteString.toStrict . runPut . put


decode :: (Monad m, Binary a) => PlainText a -> m a
decode (PlainText bytes) =
  finish $ pushChunk (runGetIncremental get) bytes
  where
    finish = \case
      Done _ _ output -> return output
      Fail _ _ msg    -> fail msg
      Partial f       -> finish $ f Nothing


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary (PlainText a) where
  arbitrary = fmap (PlainText . ByteString.pack) arbitrary


instance Arbitrary (CipherText a) where
  arbitrary = fmap (CipherText . ByteString.pack) arbitrary
\end{code}
