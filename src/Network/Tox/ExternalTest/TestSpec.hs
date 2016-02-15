{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.ExternalTest.TestSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative           (pure, (<*>))
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.EncodingSpec
import qualified Network.Tox.ExternalTest.Test as Test


resultLaws :: Spec
resultLaws = do
  describe "Monad" $ do
    it "satisfies left identity" $
      property $ \a -> (return' a `bind'` f) `shouldBe` f a

    it "satisfies right identity" $
      property $ \m -> (m `bind'` return') `shouldBe` m

    it "satisfies associativity" $
      property $ \m -> ((m `bind'` f) `bind'` g) `shouldBe` (m `bind'` (\x -> f x `bind'` g))

  describe "Applicative" $ do
    it "satisfies identity" $
      property identity

    it "satisfies composition" $
      property $ \x y -> composition (pure (x *)) (pure (y *))

    it "satisfies homomorphism" $
      property $ \x -> homomorphism (x *)

    it "satisfies interchange" $
      property $ \x -> interchange (pure (x *))

  where
    --
    -- Aliases constrained to the Result monad. These also help avoid lint
    -- warnings about using monad laws.
    --

    return' :: Int -> Test.Result Int
    return' = return

    bind' :: Test.Result Int -> (Int -> Test.Result Int) -> Test.Result Int
    bind' = (>>=)

    pure' :: a -> Test.Result a
    pure' = pure

    --
    -- Applicative laws.
    --

    identity :: Test.Result Int -> Expectation
    identity v =
      (pure' id <*> v) `shouldBe` v

    composition :: Test.Result (Int -> Int) -> Test.Result (Int -> Int) -> Test.Result Int -> Expectation
    composition u v w =
      (pure' (.) <*> u <*> v <*> w) `shouldBe` (u <*> (v <*> w))

    homomorphism :: (Int -> Int) -> Int -> Expectation
    homomorphism h x =
      (pure' h <*> pure' x) `shouldBe` pure' (h x)

    interchange :: Test.Result (Int -> Int) -> Int -> Expectation
    interchange u y =
      (u <*> pure' y) `shouldBe` (pure' ($ y) <*> u)

    --
    -- Functions f and g used in Monad laws.
    --

    f = \case
      x | x `mod` 3 == 0 -> Test.Success $ x * 123
      x | x `mod` 3 == 1 -> Test.Failure "Holla"
      _                  -> Test.Skipped

    g = \case
      x | x `mod` 3 == 0 -> Test.Skipped
      x | x `mod` 3 == 1 -> Test.Failure "Oh noes"
      x                  -> Test.Success $ x * 234


spec :: Spec
spec = do
  jsonSpec (Proxy :: Proxy (Test.Result Int))
  binarySpec (Proxy :: Proxy (Test.Result Int))
  readShowSpec (Proxy :: Proxy (Test.Result Int))

  describe "Result" resultLaws
