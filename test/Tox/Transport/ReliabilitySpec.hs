{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tox.Transport.ReliabilitySpec where

import qualified Data.ByteString           as BS
import           Data.List                 (foldl')
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as Set
import           Test.Hspec
import           Test.QuickCheck

import qualified Tox.Core.Time             as Time
import           Tox.Transport.Reliability

-- | A sender that sent @n@ packets at time 0.
sender :: Int -> ReliabilityState
sender n = foldl' send initState [1 .. n]
  where
    send rs i =
      let (num, rs') = fromJust $ addSent (BS.singleton $ fromIntegral i) rs
      in markSent (Time.fromSeconds 0) num rs'

spec :: Spec
spec = do
  describe "receiving" $ do
    it "delivers packets in order once the gap is filled" $ do
      let s1 = fromJust $ addReceived 1 "b" initState
          (d1, s1') = takeReceived s1
          s2 = fromJust $ addReceived 0 "a" s1'
          (d2, _) = takeReceived s2
      d1 `shouldBe` []
      d2 `shouldBe` ["a", "b"]

    it "rejects duplicates" $ do
      let s1 = fromJust $ addReceived 3 "x" initState
      addReceived 3 "x" s1 `shouldBe` Nothing

  describe "request packets" $
    it "make the sender resend exactly the missing packets" $
      property $ \(Positive n') (received :: [Int]) ->
        let n = n' `mod` 1000 + 1
            got = Set.fromList $ map (`mod` n) received
            recv = fromJust $ setReceivedEnd (fromIntegral n) $
              foldl' (\rs i -> fromJust $ addReceived (fromIntegral i) "x" rs) initState (Set.toList got)
            request = requestPacket maxCryptoDataSize recv
            sent' = fst . fromJust $ handleRequest (Time.fromSeconds 10) (Time.seconds 1) request (sender n)
            missing = [fromIntegral i | i <- [0 .. n - 1], Set.notMember i got]
        in map fst (unsentPackets sent') `shouldBe` missing

  describe "data packet contents" $
    it "round-trip with padding" $
      property $ \start num (NonEmpty bytes) ->
        let payload = BS.pack $ 16 : bytes
        in decodeData (encodeData start num payload) `shouldBe` Just (start, num, payload)
