{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.Tox.EncodingSpec where

import           Control.Monad.IO.Class (liftIO)
import qualified Network.Tox.RPC        as RPC
import qualified Network.Tox.RPCTest    as RPC
import           Test.Hspec
import           Test.QuickCheck        (Arbitrary, property)

import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary (get, put)
import qualified Data.Binary.Bits.Get   as Bits (BitGet, runBitGet)
import qualified Data.Binary.Bits.Put   as Bits (BitPut, runBitPut)
import qualified Data.Binary.Get        as Binary (Decoder (..), Get, pushChunk,
                                                   runGet, runGetIncremental)
import qualified Data.Binary.Put        as Binary (Put, runPut)
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as LazyByteString
import           Data.Proxy             (Proxy (..))
import           Data.Typeable          (Typeable)
import           Data.Word              (Word8)

import qualified Network.Tox.Binary     as Binary
import           Network.Tox.Encoding   (BitEncoding, bitGet, bitPut, getBool,
                                         putBool)


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy Int)
  --binarySpec (Proxy :: Proxy Bool)
  binaryGetPutSpec "{get,put}Bool" getBool putBool


expectDecoded :: (Binary a, Eq a, Show a) => [Word8] -> a -> Expectation
expectDecoded bytes expected =
  Binary.runGet Binary.get (LazyByteString.pack bytes) `shouldBe` expected


expectDecoderFail :: Binary.Get a -> [Word8] -> String -> Expectation
expectDecoderFail getA bytes expectedMessage =
  let decoder = Binary.runGetIncremental getA in
  case Binary.pushChunk decoder $ ByteString.pack bytes of
    Binary.Fail _ _ msg -> msg `shouldContain` expectedMessage
    Binary.Partial _    -> expectationFailure "Not enough input to reach failure"
    Binary.Done {}      -> expectationFailure "Input unexpectedly yielded a valid value"


binaryEncodeAndDecode :: (Eq a, Show a) => Binary.Get a -> (a -> Binary.Put) -> a -> Expectation
binaryEncodeAndDecode getA putA expected =
  let bytes = LazyByteString.toStrict $ Binary.runPut $ putA expected in
  finish $ Binary.pushChunk (Binary.runGetIncremental getA) bytes

  where
    finish = \case
      Binary.Fail _ _ msg            -> expectationFailure msg
      Binary.Partial next            -> finish $ next Nothing
      Binary.Done remaining _ output -> do
        remaining `shouldBe` ByteString.empty
        output `shouldBe` expected


binaryGetPutSpec :: (Arbitrary a, Eq a, Show a) => String -> Binary.Get a -> (a -> Binary.Put) -> Spec
binaryGetPutSpec name getA putA =
  describe name $ do
    it "decodes encoded protocols correctly" $
      property $ binaryEncodeAndDecode getA putA

    it "handles arbitrary input" $
      property $ \bytes ->
        let
          finish = \case
            Binary.Fail {}         -> return ()
            Binary.Partial f       -> finish $ f Nothing
            Binary.Done _ _ output -> binaryEncodeAndDecode getA putA output
        in
        finish $ Binary.pushChunk (Binary.runGetIncremental getA) $ ByteString.pack bytes

    it "handles empty input" $
      let
        bytes = []
        decoder = Binary.runGetIncremental getA
      in
      case Binary.pushChunk decoder $ ByteString.pack bytes of
        Binary.Fail _ _ msg -> expectationFailure msg
        Binary.Partial _    -> return ()
        Binary.Done {}      -> expectationFailure "Done with empty input; packet grammar appears to be nullable"


binarySpec :: (Arbitrary a, Eq a, Show a, Binary a) => Proxy a -> Spec
binarySpec (Proxy :: Proxy a) =
  binaryGetPutSpec "Binary.{get,put}" (Binary.get :: Binary.Get a) (Binary.put :: a -> Binary.Put)


bitEncodingSpec :: (Arbitrary a, Eq a, Show a, BitEncoding a) => Proxy a -> Spec
bitEncodingSpec (Proxy :: Proxy a) =
  let
    bitGetA = (bitGet :: Bits.BitGet a)
    bitPutA = (bitPut :: a -> Bits.BitPut ())
  in
  binaryGetPutSpec "BitEncoding.bit{Get,Put}" (Bits.runBitGet bitGetA) (Bits.runBitPut . bitPutA)


readShowSpec :: (Arbitrary a, Eq a, Show a, Read a) => Proxy a -> Spec
readShowSpec (Proxy :: Proxy a) =
  let
    showA = show :: a -> String
    readA = read :: String -> a
  in
  describe "Read/Show" $
    it "encodes and decodes correctly" $
      property $ \expected ->
        let output = readA $ showA expected in
        output `shouldBe` expected


rpcSpec :: (Arbitrary a, Eq a, Show a, Typeable a, Binary a, RPC.MessagePack a) => Proxy a -> Spec
rpcSpec (Proxy :: Proxy a) =
  let
    encodeAC = Binary.encodeC :: a -> RPC.Client ByteString.ByteString
    decodeAC = Binary.decodeC :: ByteString.ByteString -> RPC.Client (Maybe a)
    encodeA  = Binary.encode  :: a -> ByteString.ByteString
    decodeA  = Binary.decode  :: ByteString.ByteString -> Maybe a
  in

  describe "MessagePack" $ do
    it "encodes and decodes correctly" $
      property $ \expected -> RPC.runTest $ do
        encoded <- encodeAC expected
        decoded <- decodeAC encoded
        liftIO $ decoded `shouldBe` Just expected

    it "encodes arbitrary input correctly" $
      property $ \expected -> RPC.runTest $ do
        encoded <- encodeAC expected
        liftIO $ encoded `shouldBe` encodeA expected

    it "decodes arbitrary input correctly" $
      property $ \bytes -> RPC.runTest $ do
        let bs = ByteString.pack bytes
        decoded <- decodeAC bs
        liftIO $ decoded `shouldBe` decodeA bs
