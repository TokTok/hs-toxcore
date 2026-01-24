module Tox.Transport.StreamSpec (spec) where

import           Data.List            (foldl')
import           Test.Hspec

import qualified Tox.Core.Time        as Time
import           Tox.Transport.Stream

-- | Take samples 50 ms apart, sending the same number of new packets before
-- each, with the send queue at the same length.
steady :: Int -> Int -> Congestion -> Congestion
steady perSample queue cc0 = foldl' step cc0 [1 .. 13]
  where
    step cc i = sample 0 queue False (Time.fromMillis (50 * i)) (iterate sentNew cc !! perSample)

spec :: Spec
spec = describe "congestion control" $ do
  it "sends a fifth faster than it measured" $
    ccSendRate (steady 3 0 initState) `shouldBe` 72

  it "sends a tenth slower within a second of a congestion event" $
    ccSendRate (steady 3 0 initState { ccLastCongestion = Just (Time.fromMillis 600) }) `shouldBe` 54

  it "slows down while the send queue holds more than two seconds" $
    ccSendRate (steady 3 300 initState) `shouldBe` 24

  it "refills its budget at the send rate, at most four refills ahead" $ do
    let start = refill (Time.fromMillis 0) initState { ccSendRate = 72 }
        later = Time.fromMillis 1000
    ccPacketsLeft start `shouldBe` minQueue
    ccPacketsLeft (refill later start) `shouldBe` minQueue + 72
    ccPacketsLeft (refill later start { ccPacketsLeft = 1000 }) `shouldBe` 4 * 72 + minQueue

  it "counts resending its whole budget as a congestion event" $ do
    let cc = refill (Time.fromMillis 0) initState
        now = Time.fromMillis 10
    ccLastCongestion (resent now 10 cc) `shouldBe` Nothing
    ccPacketsLeft (resent now 10 cc) `shouldBe` minQueue - 10
    ccLastCongestion (resent now minQueue cc) `shouldBe` Just now
