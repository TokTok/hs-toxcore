{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.LocationsSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Tox.Android.Locations

spec :: Spec
spec = do
  it "reads places as clients write them, and nothing else" $ do
    location "geo:52.370216,4.895168" `shouldBe` Just (52.370216, 4.895168)
    location "geo:-33.9,18" `shouldBe` Just (-33.9, 18)
    mapM_ (\t -> location t `shouldBe` Nothing)
      ["geo:91,0", "geo:0,181", "geo:1e-7,0", "geo:52.1,4.3;u=20", "geo: 1,2", "geo:.5,1", "geo:1.,2", "see geo:1,2"]

  it "writes places so that they read back" $
    property $ \(Positive a) (Positive b) ->
      let latitude = fromIntegral (a `mod` 180000000 :: Int) / 1000000 - 90
          longitude = fromIntegral (b `mod` 360000000 :: Int) / 1000000 - 180
      in fmap (\(x, y) -> (round (x * 1000000), round (y * 1000000)))
           (location (locationText latitude longitude))
           == Just (round (latitude * 1000000) :: Integer, round (longitude * 1000000) :: Integer)
