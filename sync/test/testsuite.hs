module Main (main) where

import           Test.Hspec

import qualified Tox.Sync.ConformanceSpec

main :: IO ()
main = hspec $ describe "Conformance" Tox.Sync.ConformanceSpec.spec
