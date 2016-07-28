{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe          #-}
module Data.Result where

import           Control.Applicative (Applicative, pure, (<*>))


data Result a
  = Success a
  | Failure String
  deriving (Functor)


instance Applicative Result where
  pure = Success

  Success f   <*> x = fmap f x
  Failure msg <*> _ = Failure msg


instance Monad Result where
  return = pure
  fail = Failure

  Success x   >>= f = f x
  Failure msg >>= _ = Failure msg
