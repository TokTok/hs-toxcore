\section{Text}

The Tox protocol differentiates between two types of text: Plain Text and
Cipher Text.  Cipher Text may be transmitted over untrusted data channels.
Plain Text can be Sensitive or Non Sensitive.  Sensitive Plain Text must be
transformed into Cipher Text using the encryption function before it can be
transmitted over untrusted data channels.

\begin{code}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
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
import qualified Data.ByteString.Lazy      as LazyByteString
import           Data.MessagePack          (MessagePack (..))
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Text.Read                 (readPrec)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype PlainText = PlainText { unPlainText :: ByteString }
  deriving (Eq, Binary, Generic, Typeable)

instance MessagePack PlainText

instance Show PlainText where
  show = show . Base16.encode . unPlainText

instance Read PlainText where
  readPrec = PlainText . fst . Base16.decode <$> readPrec


newtype CipherText = CipherText { unCipherText :: ByteString }
  deriving (Eq, Binary, Generic, Typeable)

instance MessagePack CipherText

instance Show CipherText where
  show = show . Base16.encode . unCipherText

instance Read CipherText where
  readPrec = CipherText . fst . Base16.decode <$> readPrec


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
