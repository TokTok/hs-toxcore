{-# LANGUAGE LambdaCase #-}
-- | This module provides helper functions to make writing RPC tests slightly
-- easier.
module Network.Tox.RPCTest where

import           Control.Exception          (catch)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Text                  as Text
import           Test.Hspec
import           Test.QuickCheck            (Arbitrary, Testable, property)

import           Data.MessagePack           (Object (..))
import           Network.MessagePack.Client (Client, RpcError (..))
import           Network.Tox.RPC            (runClient)


-- | Run a 'Client' using 'runClient' and catch "Pending" errors from the
-- system under test (SUT) to turn them into Hspec 'pending' calls. Other
-- errors are turned into calls to 'expectationFailure'.
runTest :: Client () -> IO ()
runTest a3 =
  runClient a3 `catch` \case
    RemoteError (ObjectStr msg) | msg == Text.pack "Pending" -> pending
    e -> expectationFailure $ show e


-- | Common code for the equivN helper functions below. Each equiv function
-- ('equiv1', 'equiv2', ...) evaluates a pure library function and executes an
-- equivalent RPC method and compares their results. This is used to check
-- whether the model implementation and the SUT produce the same output for any
-- given input. It uses 'shouldBe' from Hspec to compare the results.
--
-- Not all functions require equivalence. E.g. @Nonce.newNonce@ is
-- non-deterministic, as it generates a random nonce using the system's random
-- source. Therefore, no 'equiv'-like function exists for functions in the 'IO'
-- monad.
equiv :: (Eq r, Show r)
      => r -> Client r -> Client ()
equiv expected actualM = do
  actual <- actualM
  liftIO $ actual `shouldBe` expected


-- | 'equivProp' and its @equivPropN@ variants produce equivalence property
-- tests. 'equivProp' is @it "msg" . property@ with an appropriate message.
equivProp :: Testable prop => prop -> Spec
equivProp = it "is equivalent to its RPC method" . property


equiv1 :: (Eq r, Show r)
       => (a1 -> r)
       -> (a1 -> Client r)
       -> a1 -> Client ()
equiv1 f1 f2 a1 =
  equiv (f1 a1)
        (f2 a1)

equivProp1 :: ( Eq r, Show r
              , Show a1, Arbitrary a1)
           => (a1 -> r)
           -> (a1 -> Client r)
           -> Spec
equivProp1 f m =
  equivProp $ \a1 -> runTest $ equiv1 f m a1


equiv2 :: (Eq r, Show r)
       => (a1 -> a2 -> r)
       -> (a1 -> a2 -> Client r)
       -> a1 -> a2 -> Client ()
equiv2 f1 f2 a1 a2 =
  equiv (f1 a1 a2)
        (f2 a1 a2)

equivProp2 :: ( Eq r, Show r
              , Show a1, Arbitrary a1
              , Show a2, Arbitrary a2)
           => (a1 -> a2 -> r)
           -> (a1 -> a2 -> Client r)
           -> Spec
equivProp2 f m =
  equivProp $ \a1 a2 -> runTest $ equiv2 f m a1 a2


equiv3 :: (Eq r, Show r)
       => (a1 -> a2 -> a3 -> r)
       -> (a1 -> a2 -> a3 -> Client r)
       -> a1 -> a2 -> a3 -> Client ()
equiv3 f1 f2 a1 a2 a3 =
  equiv (f1 a1 a2 a3)
        (f2 a1 a2 a3)

equivProp3 :: ( Eq r, Show r
              , Show a1, Arbitrary a1
              , Show a2, Arbitrary a2
              , Show a3, Arbitrary a3)
           => (a1 -> a2 -> a3 -> r)
           -> (a1 -> a2 -> a3 -> Client r)
           -> Spec
equivProp3 f m =
  equivProp $ \a1 a2 a3 -> runTest $ equiv3 f m a1 a2 a3


equiv4 :: (Eq r, Show r)
       => (a1 -> a2 -> a3 -> a4 -> r)
       -> (a1 -> a2 -> a3 -> a4 -> Client r)
       -> a1 -> a2 -> a3 -> a4 -> Client ()
equiv4 f1 f2 a1 a2 a3 a4 =
  equiv (f1 a1 a2 a3 a4)
        (f2 a1 a2 a3 a4)

equivProp4 :: ( Eq r, Show r
              , Show a1, Arbitrary a1
              , Show a2, Arbitrary a2
              , Show a3, Arbitrary a3
              , Show a4, Arbitrary a4)
           => (a1 -> a2 -> a3 -> a4 -> r)
           -> (a1 -> a2 -> a3 -> a4 -> Client r)
           -> Spec
equivProp4 f m =
  equivProp $ \a1 a2 a3 a4 -> runTest $ equiv4 f m a1 a2 a3 a4
