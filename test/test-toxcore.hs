module Main (main) where

import           System.Environment  (getArgs, withArgs)
import           System.Process      (createProcess, proc, terminateProcess)

import           Network.Tox.Testing (serve)
import qualified TestSuite


main :: IO ()
main = do
  args <- getArgs
  (_, _, _, sut) <- createProcess $ proc "dist/build/sut-toxcore/sut-toxcore" []
  withArgs (["--print-cpu-time", "--color"] ++ args) TestSuite.main
  terminateProcess sut
