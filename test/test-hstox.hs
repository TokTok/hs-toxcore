module Main (main) where

import           Control.Concurrent.Async (cancel, wait, withAsync)
import           System.Environment       (getArgs, withArgs)

import           Network.Tox.Testing      (serve)
import qualified TestSuite


main :: IO ()
main = do
  args <- getArgs
  withAsync serve $ \server ->
    withAsync (client args) $ \res -> do
      wait res
      cancel server
  where
    client args = withArgs (args ++ ["--print-cpu-time", "--color"]) TestSuite.main
