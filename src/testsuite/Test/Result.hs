{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe          #-}
module Test.Result where

import           Control.Applicative (Applicative, pure, (<*>))


data TestResult a
  = TestSuccess a
  | TestFailure String
  deriving (Functor)


instance Applicative TestResult where
  pure = TestSuccess

  TestSuccess f   <*> x = fmap f x
  TestFailure msg <*> _ = TestFailure msg


instance Monad TestResult where
  return = pure
  fail = TestFailure

  TestSuccess x   >>= f = f x
  TestFailure msg >>= _ = TestFailure msg
