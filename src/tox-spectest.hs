module Main (main) where

import           Control.Concurrent (threadDelay)
import           System.Environment (getArgs, withArgs)
import           System.Process     (createProcess, proc, terminateProcess)

import qualified ToxTestSuite


getSutAndArgs :: IO (String, [String])
getSutAndArgs = do
  args <- getArgs
  case args of
    []         -> fail "Usage: tox-spectest <sut> [args...]"
    sut : rest -> return (sut, rest)


withSut :: String -> IO () -> IO ()
withSut "" action = action
withSut sut action = do
  -- Start a SUT (System Under Test) process that will listen on port 1234.
  (_, _, _, sutProc) <- createProcess $ proc sut []
  -- 2 seconds delay to give the SUT time to set up its socket before we try
  -- to build connections in the test runner. This is high, because on Travis,
  -- the SUT can take quite some time to become ready to accept connections.
  -- Ideally, we would probe the SUT every 100ms or so until it starts
  -- accepting RPCs, but that's something we should consider putting into the
  -- RPC library itself.
  threadDelay $ 2 * 1000 * 1000
  action
  terminateProcess sutProc


main :: IO ()
main = do
  (sut, args) <- getSutAndArgs
  withSut sut $
    -- TestSuite (the test runner) makes connections to port 1234 to communicate
    -- with the SUT.
    withArgs (["--print-cpu-time", "--color"] ++ args) ToxTestSuite.main
