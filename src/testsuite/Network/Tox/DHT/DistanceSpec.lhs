\begin{code}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.DistanceSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Monoid              (Monoid, mappend, mempty)
import           Data.Proxy               (Proxy (..))
import qualified Network.Tox.Crypto.Key   as Key
import           Network.Tox.DHT.Distance
import           Network.Tox.EncodingSpec

\end{code}

The XOR metric \texttt{d} satisfies the required conditions:

\begin{enumerate}
  \item Non-negativity \texttt{d(x, y) >= 0}: Since public keys are Crypto
    Numbers, which are by definition positive, their XOR is necessarily
    positive.
  \item Identity of indiscernibles \texttt{d(x, y) == 0} iff \texttt{x == y}:
    The XOR of two integers is zero iff they are equal.
  \item Symmetry \texttt{d(x, y) == d(y, x)}: XOR is a symmetric operation.
  \item Subadditivity \texttt{d(x, z) <= d(x, y) + d(y, z)}: TODO.
\end{enumerate}

\begin{code}

metricSpec :: ( Eq a, Arbitrary a, Show a
              , Eq b, Ord b, Monoid b, Show b)
           => (a -> a -> b) -> Spec
metricSpec d = do
  it "satisfies non-negativity" $
    property $ \x y ->
      d x y > mempty

  it "satisfies identity of indiscernibles" $
    property $ \x y ->
      d x y == mempty `shouldBe` x == y

  it "satisfies symmetry" $
    property $ \x y ->
      d x y `shouldBe` d y x

  it "satisfies triangle inequality" $
    property $ \x y z ->
      d x z <= d x y `mappend` d y z


zeroKey :: Key.PublicKey
zeroKey = read "\"0000000000000000000000000000000000000000000000000000000000000000\""


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy Distance)

  describe "xorDistance" $ do
    metricSpec xorDistance

    it "should not partition the network at 0x7f/0x80" $
      let
        o = zeroKey
        x = read "\"8000000000000000000000000000000000000000000000000000000000000000\""
        y = read "\"7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""

        ox = xorDistance o x
        oy = xorDistance o y
      in

      oy < ox
\end{code}

Example: Given three nodes with keys 2, 5, and 6:

\begin{itemize}
  \item \texttt{2 XOR 5 = 7}
  \item \texttt{2 XOR 6 = 4}
  \item \texttt{5 XOR 2 = 7}
  \item \texttt{5 XOR 6 = 3}
  \item \texttt{6 XOR 2 = 4}
  \item \texttt{6 XOR 5 = 3}
\end{itemize}

The closest node from both 2 and 5 is 6.  The closest node from 6 is 5 with
distance 3.  This example shows that a key that is close in terms of integer
addition may not necessarily be close in terms of XOR.

\begin{code}

    it "should yield the values from the example from the spec" $
      let
        k1 = read "\"0000000000000000000000000000000000000000000000000000000000000002\""
        k2 = read "\"0000000000000000000000000000000000000000000000000000000000000005\""
        k3 = read "\"0000000000000000000000000000000000000000000000000000000000000006\""
      in do

      xorDistance k1 k2 `shouldBe` Distance 7
      xorDistance k1 k3 `shouldBe` Distance 4
      xorDistance k2 k1 `shouldBe` Distance 7
      xorDistance k2 k3 `shouldBe` Distance 3
      xorDistance k3 k1 `shouldBe` Distance 4
      xorDistance k3 k2 `shouldBe` Distance 3

  describe "log2" $ do
    it "should result in 0 <= value for any Distance" $
      property $ \distance ->
        log2 distance `shouldSatisfy` \case
          Nothing    -> True
          Just value -> 0 <= value

    it "should result in 0 <= value < 256 for public key distances" $
      property $ \pk1 pk2 ->
        log2 (xorDistance pk1 pk2) `shouldSatisfy` \case
          Nothing    -> True
          Just value -> 0 <= value && value < 256

    it "should result in 255 for maximum distance" $
      let
        k1 = read "\"0000000000000000000000000000000000000000000000000000000000000000\""
        k2 = read "\"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""
      in
      log2 (xorDistance k1 k2) `shouldBe` Just 255

    it "should result in 255 for the highest bit set" $
      let
        k1 = read "\"0000000000000000000000000000000000000000000000000000000000000000\""
        k2 = read "\"8000000000000000000000000000000000000000000000000000000000000000\""
      in
      log2 (xorDistance k1 k2) `shouldBe` Just 255

    it "should result in 254 for the highest-but-one bit set" $
      let
        k1 = read "\"0000000000000000000000000000000000000000000000000000000000000000\""
        k2 = read "\"7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\""
      in
      log2 (xorDistance k1 k2) `shouldBe` Just 254

    it "should result in Nothing for distance 0" $
      let
        k = read "\"0000000000000000000000000000000000000000000000000000000000000000\""
      in
      log2 (xorDistance k k) `shouldBe` Nothing

\end{code}
