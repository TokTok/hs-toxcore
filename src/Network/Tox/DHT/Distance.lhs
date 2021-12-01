\section{Distance}

\begin{code}
{-# LANGUAGE MagicHash   #-}
{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.Distance where

import           Control.Applicative       ((<$>))
import           Control.Arrow             (first)
import           Data.Bits                 (xor)
import           Data.Monoid               (Monoid, mappend, mempty)
import           Data.Semigroup            (Semigroup, (<>))
import           GHC.Exts                  (Int (I#))
import           GHC.Integer.Logarithms    (integerLog2#)
import           Network.Tox.Crypto.Key    (PublicKey)
import qualified Network.Tox.Crypto.Key    as Key (keyToInteger)
import           Numeric                   (readHex, showHex)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A Distance is a positive integer.  Its human-readable representation is a
base-16 number.  Distance (type) is an
\href{https://en.wikipedia.org/wiki/Ordered_semigroup}{ordered monoid} with the
associative binary operator \texttt{+} and the identity element \texttt{0}.

\begin{code}

newtype Distance = Distance Integer
  deriving (Eq, Ord)


instance Semigroup Distance where
  (Distance x) <> (Distance y) = Distance (x + y)

instance Monoid Distance where
  mempty = Distance 0
  mappend (Distance x) (Distance y) = Distance (x + y)


instance Show Distance where
  show (Distance distance) = showHex distance ""

instance Read Distance where
  readsPrec _ string = map (first Distance) $ readHex string


log2 :: Distance -> Maybe Int
log2 (Distance 0) = Nothing
log2 (Distance x) = Just $ I# (integerLog2# x)


\end{code}

The DHT uses a
\href{https://en.wikipedia.org/wiki/Metric_(mathematics)}{metric} to determine
the distance between two nodes.  The Distance type is the co-domain of this
metric. The metric currently used by the Tox DHT is the \texttt{XOR} of the
nodes' public keys: \texttt{distance(x, y) = x XOR y}.  For this computation,
public keys are interpreted as Big Endian integers (see \href{#key-1}{Crypto
Numbers}).

When we speak of a "close node", we mean that its Distance to the node under
consideration is small compared to the Distance to other nodes.

\begin{code}

xorDistance :: PublicKey -> PublicKey -> Distance
xorDistance a b =
  Distance $ Key.keyToInteger a `xor` Key.keyToInteger b

-- | rebaseDistance a b (xorDistance a c) == xorDistance b c
rebaseDistance :: PublicKey -> PublicKey -> Distance -> Distance
rebaseDistance a b (Distance d) =
  Distance $ d `xor` Key.keyToInteger a `xor` Key.keyToInteger b

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary Distance where
  arbitrary = Distance . abs <$> arbitrary
\end{code}

An implementation is not required to provide a Distance type, so it has no
specified binary representation.  For example, instead of computing a distance
and comparing it against another distance, the implementation can choose to
implement Distance as a pair of public keys and define an ordering on Distance
without computing the complete integral value.  This works, because as soon as
an ordering decision can be made in the most significant bits, further bits
won't influence that decision.

\input{test/Network/Tox/DHT/DistanceSpec.lhs}
