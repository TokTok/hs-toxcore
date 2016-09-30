module Main (main) where

import           Control.Concurrent.Async (cancel, wait, withAsync)
import           System.Environment       (withArgs)

import           Network.Tox.Testing      (serve)
import qualified ToxTestSuite


main :: IO ()
main =
  withAsync serve $ \server ->
    withAsync client $ \res -> do
      wait res
      cancel server
  where
    client = withArgs ["--print-cpu-time", "--color"] ToxTestSuite.main
