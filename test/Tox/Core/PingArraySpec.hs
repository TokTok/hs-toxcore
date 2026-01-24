{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Core.PingArraySpec (spec) where

import           Data.List          (foldl')
import qualified Data.Map           as Map
import           Data.Word          (Word64)
import           Test.Hspec
import           Test.QuickCheck

import qualified Tox.Core.PingArray as PingArray
import qualified Tox.Core.Time      as Time

spec :: Spec
spec =
  describe "PingArray" $
    it "keeps only the newest entries, each found once by its ID" $ property $
      \(seeds :: [Word64]) ->
        let now = Time.Timestamp 0
            size = 4
            add (acc, arr) (n, seed) = let (pingId, arr') = PingArray.addEntry now n seed arr in (pingId : acc, arr')
            (ids, pa) = foldl' add ([], PingArray.empty size (Time.seconds 10)) (zip [0 :: Int ..] seeds)
            newest = take size ids
            taken = [ fst (PingArray.takeEntry now pingId pa) | pingId <- newest ]
            twice = [ fst . PingArray.takeEntry now pingId . snd $ PingArray.takeEntry now pingId pa | pingId <- newest ]
        in Map.size (PingArray.paEntries pa) <= size
             .&&. taken === [ Just (length seeds - 1 - i) | i <- [0 .. length newest - 1] ]
             .&&. all (== Nothing) twice
