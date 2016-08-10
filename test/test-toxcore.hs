module Main (main) where

import           System.Environment  (getArgs, withArgs)
import           System.Process      (spawnProcess, terminateProcess)

import           Network.Tox.Testing (serve)
import qualified TestSuite


main :: IO ()
main = do
  args <- getArgs
  sut <- spawnProcess "dist/build/sut-toxcore/sut-toxcore" []
  withArgs (["--print-cpu-time", "--color"] ++ args) TestSuite.main
  terminateProcess sut
