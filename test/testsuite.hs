module Main (main) where

import           Control.Concurrent.Async (cancel, wait, withAsync)
import           System.Environment       (getArgs, withArgs)

import           Network.Tox.Testing      (serve)
import qualified ToxTestSuite


main :: IO ()
main = do
  args <- getArgs
  let client = withArgs (args ++ ["--print-cpu-time", "--color"]) ToxTestSuite.main
  withAsync serve $ \server ->
    withAsync client $ \res -> do
      wait res
      cancel server
