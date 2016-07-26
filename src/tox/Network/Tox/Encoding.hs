{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.Encoding where

import           Data.Binary             (Binary, get, put)
import           Data.Binary.Bits.Get    (BitGet)
import           Data.Binary.Bits.Put    (BitPut)
import           Data.Binary.Get         (Decoder (..), Get, getWord8,
                                          pushChunk, runGetIncremental)
import           Data.Binary.Put         (Put, putWord8, runPut)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as ByteString
import qualified Data.ByteString.Lazy    as LazyByteString
import           Network.Tox.Crypto.Text (PlainText (..))


class BitEncoding a where
  bitGet :: BitGet a
  bitPut :: a -> BitPut ()


encode :: Binary a => a -> ByteString
encode =
  LazyByteString.toStrict . runPut . put


decode :: (Monad m, Binary a) => ByteString -> m a
decode bytes =
  finish $ pushChunk (runGetIncremental get) bytes
  where
    finish = \case
      Done unconsumed _ output ->
        if ByteString.null unconsumed
          then return output
          else fail $ "unconsumed input: " ++ show (PlainText unconsumed)
      Fail _ _ msg    -> fail msg
      Partial f       -> finish $ f Nothing
