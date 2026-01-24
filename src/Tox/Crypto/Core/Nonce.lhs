\subsection{Nonce}

A random nonce is generated using the cryptographically secure random number
generator from the NaCl library \texttt{randombytes}.

A nonce is incremented by interpreting it as a Big Endian number and adding 1.
If the nonce has the maximum value, the value after the increment is 0.

Most parts of the protocol use random nonces.  This prevents new nonces from
being associated with previous nonces.  If many different packets could be tied
together due to how the nonces were generated, it might for example lead to
tying DHT and onion announce packets together.  This would introduce a flaw in
the system as non friends could tie some people's DHT keys and long term keys
together.

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE StrictData #-}
module Tox.Crypto.Core.Nonce where

import qualified Crypto.Saltine.Class    as Sodium (decode, encode, nudge)
import qualified Crypto.Saltine.Core.Box as Sodium (newNonce)
import qualified Data.ByteString         as BS
import           Tox.Crypto.Core.Key


newNonce :: IO Nonce
newNonce = Key <$> Sodium.newNonce


reverseNonce :: Nonce -> Nonce
reverseNonce (Key nonce) =
  let Just reversed = Sodium.decode $ BS.reverse $ Sodium.encode nonce in
  Key reversed


nudge :: Nonce -> Nonce
nudge =
  Key . Sodium.nudge . unKey


increment :: Nonce -> Nonce
increment =
  reverseNonce . nudge . reverseNonce

nonceToInteger :: Nonce -> Integer
nonceToInteger (Key nonce) =
  BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (Sodium.encode nonce)

nonceLimit :: Integer
nonceLimit = 256 ^ (24 :: Int)

integerToNonce :: Integer -> Nonce
integerToNonce n =
  let bs = BS.pack . reverse . take 24 . (++ repeat 0) $ toBytes (n `mod` nonceLimit)
      Just nonce = Sodium.decode bs
  in Key nonce
  where
    toBytes 0 = []
    toBytes x = fromIntegral (x `rem` 256) : toBytes (x `quot` 256)

\end{code}
