{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE Safe             #-}
module Main (main) where

import           Control.Applicative             ((<$>))
import           Data.Binary                     (Binary, get, put)
import           Data.Binary.Get                 (Get, runGetOrFail)
import           Data.Binary.Put                 (runPut)
import qualified Data.ByteString.Lazy            as LazyByteString
import           Data.List.Split                 (splitOn)
import qualified Network.Tox.Crypto.Text         as Text (decode)
import qualified Network.Tox.DHT.Distance        as Distance
import qualified Network.Tox.DHT.KBuckets        as KBuckets
import qualified Network.Tox.DHT.KBucketsExtSpec as KBucketsExtSpec
import qualified Network.Tox.ExternalTest.Test   as Test (DataFormat (..),
                                                          Result (..),
                                                          Test (..), TestInput,
                                                          TestInput, construct,
                                                          deconstruct)


runTest :: (Binary input, Binary output)
        => Test.Test input output -> (input -> Test.Result output) -> Get LazyByteString.ByteString
runTest _ f =
  fmap (runPut . put . f) get


encodeFailure :: String -> LazyByteString.ByteString
encodeFailure message =
  runPut $ put (Test.Failure message :: Test.Result ())


runBinaryDecode :: Test.TestInput input
                => Test.DataFormat input -> Get LazyByteString.ByteString
runBinaryDecode fmt =
  runTest (Test.BinaryDecode fmt) $ \x ->
    Test.deconstruct <$> Text.decode x


runBinaryEncode :: Test.TestInput input
                => Test.DataFormat input -> Get LazyByteString.ByteString
runBinaryEncode fmt =
  runTest (Test.BinaryEncode fmt) $ Test.Success . Test.construct


getResult :: Get LazyByteString.ByteString
getResult = do
  command <- get
  case splitOn " " command of
    ["FailureTest"] -> runTest Test.FailureTest $ \() -> Test.Failure "Failure"
    ["SuccessTest"] -> runTest Test.SuccessTest $ \() -> Test.Success ()
    ["SkippedTest"] -> runTest Test.SkippedTest $ \() -> Test.Skipped

    ["BinaryDecode", dataFormat] ->
      case dataFormat of
        "Word32"     -> runBinaryDecode Test.Word32
        "String"     -> runBinaryDecode Test.String
        "NodeInfo"   -> runBinaryDecode Test.NodeInfo
        _ -> return $ encodeFailure $ "Unsupported data format: " ++ command

    ["BinaryEncode", dataFormat] ->
      case dataFormat of
        "Word32"     -> runBinaryEncode Test.Word32
        "String"     -> runBinaryEncode Test.String
        "NodeInfo"   -> runBinaryEncode Test.NodeInfo
        _ -> return $ encodeFailure $ "Unsupported data format: " ++ command

    ["Distance"] ->
      runTest Test.Distance $ \(origin, alice, bob) ->
        Test.Success $
          compare (Distance.xorDistance origin alice)
                  (Distance.xorDistance origin bob)

    ["KBucketIndex"] ->
      runTest Test.KBucketIndex $ \(pk1, pk2) ->
        Test.Success $ KBuckets.bucketIndex pk1 pk2

    ["KBucketNodes"] ->
      runTest Test.KBucketNodes $ \(bucketSize, baseKey, nodes, removedNodes) ->
        KBucketsExtSpec.testKBucketNodes bucketSize baseKey nodes removedNodes

    _ ->
      return $ encodeFailure $ "Unsupported test: " ++ command


main :: IO ()
main =
  LazyByteString.getContents >>= writeResult . runGetOrFail getResult
  where
    writeResult = \case
      (Left  (_, _, msg)) ->
        LazyByteString.putStr $ encodeFailure $ "Error decoding test frame: " ++ msg
      (Right (_, _, result)) ->
        LazyByteString.putStr result
