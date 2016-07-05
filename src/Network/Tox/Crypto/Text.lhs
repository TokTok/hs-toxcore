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
import           Data.Binary               (Binary, get, put)
import           Data.Binary.Get           (Decoder (..), pushChunk,
                                            runGetIncremental)
import           Data.Binary.Put           (runPut)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Char8     as Char8
import qualified Data.ByteString.Lazy      as LazyByteString
import           Data.MessagePack.Class    (MessagePack (..))
import           Data.String               (IsString (..))
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Text.Read                 (readPrec)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype PlainText = PlainText { unPlainText :: ByteString }
  deriving (Eq, Binary)

instance Show PlainText where
  show (PlainText bytes) = show $ Base16.encode bytes

instance Read PlainText where
  readPrec = PlainText . fst . Base16.decode <$> readPrec

instance MessagePack PlainText where
  toObject = toObject . unPlainText
  fromObject x = PlainText <$> fromObject x

instance IsString PlainText where
  fromString = PlainText . fromString


newtype CipherText = CipherText { unCipherText :: ByteString }
  deriving (Eq, Binary)

instance Show CipherText where
  show (CipherText bytes) = show $ Base16.encode bytes

instance Read CipherText where
  readPrec = CipherText . fst . Base16.decode <$> readPrec

instance MessagePack CipherText where
  toObject = toObject . unCipherText
  fromObject x = CipherText <$> fromObject x


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


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary PlainText where
  arbitrary = fmap (PlainText . ByteString.pack) arbitrary


instance Arbitrary CipherText where
  arbitrary = fmap (CipherText . ByteString.pack) arbitrary
\end{code}
