{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.ExternalTest where

import           Test.Hspec                    (Expectation, expectationFailure,
                                                shouldBe, shouldContain)

import           Control.Applicative           ((<$>))
import           Control.Exception             (bracket)
import qualified Control.Exception             as Exception
import           Control.Monad                 (filterM, foldM)
import           Data.Binary                   (Binary, get, put)
import qualified Data.Binary.Get               as Binary (Decoder (..),
                                                          pushChunk,
                                                          runGetIncremental)
import qualified Data.Binary.Put               as Binary (runPut)
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Lazy          as LazyByteString
import           Network.Tox.ExternalTest.Test (Test)
import qualified Network.Tox.ExternalTest.Test as Test (Result (..))
import qualified System.Directory              as Directory (executable, getDirectoryContents,
                                                             getPermissions)
import qualified System.Exit                   as Exit
import qualified System.IO                     as IO (Handle, hClose, hFlush,
                                                      hSetBinaryMode)
import           System.Process                (ProcessHandle, StdStream (..),
                                                createProcess, proc, std_err,
                                                std_in, std_out, waitForProcess)
import           Text.Printf                   (printf)


testDirectory :: String
testDirectory = "test"


getResult :: (Show a, Binary a) => ByteString.ByteString -> Either String a
getResult bytes =
  finish $ Binary.pushChunk (Binary.runGetIncremental get) bytes
  where
    finish = \case
      Binary.Done rest offset result ->
        if ByteString.null rest
        then Right result
        else Left $
          printf "Unexpected extra data after result payload (%d bytes, parsed as `%s´): %s"
            offset
            (show result)
            (show $ Base16.encode rest)
      Binary.Partial next ->
        finish $ next Nothing
      Binary.Fail _ _ msg ->
        Left $ msg ++ " while reading input: " ++ show (Base16.encode bytes)


makeProc :: FilePath -> IO (IO.Handle, IO.Handle, ProcessHandle)
makeProc exe = do
  (Just procInWrite, Just procOutRead, _, process) <- createProcess (proc exe [])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = Inherit
    }

  IO.hSetBinaryMode procInWrite True
  IO.hSetBinaryMode procOutRead True

  return (procInWrite, procOutRead, process)


closePipes :: (IO.Handle, IO.Handle, ProcessHandle) -> IO Exit.ExitCode
closePipes (procInWrite, procOutRead, process) = do
  IO.hClose procInWrite
  IO.hClose procOutRead
  waitForProcess process


withProc :: (IO.Handle -> IO.Handle -> Expectation) -> FilePath -> Expectation
withProc f exe =
  bracket (makeProc exe) closePipes $ \(procInWrite, procOutRead, process) -> do
    f procInWrite procOutRead

    exitCode <- waitForProcess process

    case exitCode of
      Exit.ExitSuccess ->
        return ()
      Exit.ExitFailure code ->
        expectationFailure $ "Process terminated with exit code " ++ show code


run :: ( Eq input , Show input , Binary input
       , Eq output, Show output, Binary output)
    => Test input output -> input -> Test.Result output -> Expectation
run test input expected = do
  files <- map ((++) $ testDirectory ++ "/") <$> Directory.getDirectoryContents testDirectory
  exes <- filterM isExecutable files
  results <- mapM (withProc runTest) exes
  foldM (\() -> return) () results

  where
    isExecutable file = Directory.executable <$> Directory.getPermissions file

    inputData = Binary.runPut $ put (show test, input)

    runTest procInWrite procOutRead = do
      LazyByteString.hPut procInWrite inputData
      IO.hFlush procInWrite

      actualOrError <- getResult <$> ByteString.hGetContents procOutRead

      case (actualOrError, expected) of
        (Right Test.Skipped, _) ->
          -- Test skipped = success.
          return ()
        (Right (Test.Failure actualMsg), Test.Failure expectedMsg) ->
          actualMsg `shouldContain` expectedMsg
        (Left failureMsg, _) ->
          expectationFailure failureMsg
        (Right actual, _) ->
          actual `shouldBe` expected
