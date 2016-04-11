\chapter{Test protocol}

The test framework consists of a model implementation and a test runner. A
“system under test” (SUT) is a protocol implementation that is tested by the
test runner. The SUT is presented to the test runner as a standalone executable
that communicates with it using pipes.

The test runner and SUT both implement the binary test protocol.

The test input is a length-prefixed test name and an arbitrary piece of data.
The meaning of that data depends on the test name.

\begin{tabular}{l|l|l}
  Length            & Type          & Contents \\
  \hline
  \texttt{8}        & Int           & Length of name \\
  \texttt{\$length} & Message Kind  & Test name \\
  \texttt{[0,]}     & Bytes         & Payload \\
\end{tabular}


\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy        #-}
{-# LANGUAGE TypeFamilies       #-}
module Network.Tox.ExternalTest.Test where

import           Control.Applicative                    (Applicative, pure,
                                                         (<*>))
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.Binary                            (Binary)
import           Data.Word                              (Word32, Word8)
import           GHC.Generics                           (Generic)
import qualified Network.Tox.Crypto.Key                 as T (Nonce, PublicKey)
import qualified Network.Tox.Crypto.Text                as T (PlainText)
import qualified Network.Tox.DHT.KBuckets               as T (KBucketIndex)
import qualified Network.Tox.NodeInfo.HostAddress       as T (HostAddress)
import qualified Network.Tox.NodeInfo.NodeInfo          as T (NodeInfo (NodeInfo))
import qualified Network.Tox.NodeInfo.PortNumber        as T (PortNumber)
import qualified Network.Tox.NodeInfo.SocketAddress     as T (SocketAddress (SocketAddress))
import qualified Network.Tox.NodeInfo.TransportProtocol as T (TransportProtocol)
import           Test.QuickCheck.Arbitrary              (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen                    as Gen


{-------------------------------------------------------------------------------
 -
 - :: Test API.
 -
 ------------------------------------------------------------------------------}

\end{code}

\section{Basic data encoding}

The test protocol uses a limited and well-defined set of types. Their binary
encodings are specified here.

\texttt{List}s are encoded as 64 bit length (big endian encoded) followed by
each element concatenated.

\begin{code}

data DataFormat a where

\end{code}
A \texttt{String} is a \texttt{List} of bytes containing the UTF-8 encoded
code points making up the string.
\begin{code}

  String     :: DataFormat String

\end{code}
A \texttt{Byte String} is a \texttt{List} of bytes.
\begin{code}

  ByteString :: DataFormat [Word8]

\end{code}
\texttt{Word32} is a 32 bit unsigned integer that is encoded in big endian.
\begin{code}

  Word32     :: DataFormat Word32

\end{code}
\texttt{Node Info} is encoded in the
\href{#node-info-packed-node-format}{packed node format}.
\begin{code}

  NodeInfo   :: DataFormat T.NodeInfo

deriving instance Show (DataFormat a)

\end{code}

\section{Deconstructed values}

Deconstructed values are used to test binary decoding and encoding capabilities
of the implementation.  To avoid simple echo implementations, the binary
representation of a deconstructed value of complex types is usually different
from the usual packet encoding.  A deconstructed value contains sufficient
information to construct a value.

An isomorphism exists between the deconstructed value and the constructed
value.
\begin{code}

class Construct a where

  type Deconstruct a
  type Deconstruct a = a

\end{code}

The \texttt{construct} function shows how a value is constructed from its
components.  By default, there is no special construction, so the construct
function is the identity function.

\begin{code}

  construct :: Deconstruct a -> a

  default construct :: a -> a
  construct = id

\end{code}

The \texttt{deconstruct} function produces the separate components from a
constructed value.  Similarly to the construct function, the default
deconstruct function is the identity function.

\begin{code}

  deconstruct :: a -> Deconstruct a

  default deconstruct :: a -> a
  deconstruct = id

\end{code}
The types \texttt{Word32}, \texttt{String}, and \texttt{Byte String} have no
special deconstructed type and are just used as-is.
\begin{code}

instance Construct Word32
instance Construct String
instance Construct [Word8]

\end{code}

\subsection{Deconstructed: Node Info}

The deconstructed Node Info value is a simple serialisation of each field,
without packing the protocol and address family into the same byte using magic
values.

\begin{tabular}{l|l|l}
  Field               & Type              & Length \\
  \hline
  \texttt{is_tcp}     & \texttt{Bool}     & 1 \\
  \texttt{is_ipv6}    & \texttt{Bool}     & 1 \\
  IP address          & \texttt{Bytes}    & 4 for IPv4, 16 for IPv6 \\
  Port number         & \texttt{Word16}   & 2 \\
  Public key          & \texttt{Bytes}    & 32 \\
\end{tabular}

The transport protocol flag (\texttt{is_tcp}) is \texttt{False} (0x00) for UDP
or \texttt{True} (0x01) for TCP. The address family flag is \texttt{False} for
IPv4 or \texttt{True} for IPv6.

\begin{code}

instance Construct T.NodeInfo where

  type Deconstruct T.NodeInfo =
    ( T.TransportProtocol
    , T.HostAddress
    , T.PortNumber
    , T.PublicKey
    )

  construct (protocol, addr, port, key) =
    T.NodeInfo protocol (T.SocketAddress addr port) key
  deconstruct (T.NodeInfo protocol (T.SocketAddress addr port) key) =
    (protocol, addr, port, key)

\end{code}

\section{Test names}

Each test has a name that is used to identify the test so the rest of the input
message (the payload) can be interpreted correctly.

\begin{code}


data Test input output where

\end{code}
\subsection{Test: Failure Test}

This test receives no data and returns no data, but expects the test to always
fail.

\begin{tabular}{l|l|l}
  Length            & Type          & \href{#test-protocol}{Contents} \\
  \hline
  0                 & -             & No data \\
\end{tabular}
\begin{code}

  FailureTest :: Test () ()

\end{code}
\subsection{Test: Success Test}

This test always succeeds, also with no data.

\begin{tabular}{l|l|l}
  Length            & Type          & \href{#test-protocol}{Contents} \\
  \hline
  0                 & -             & No data \\
\end{tabular}
\begin{code}

  SuccessTest :: Test () ()

\end{code}
\subsection{Test: Skipped Test}

This test must be skipped, also with no data. If the SUT always returns
Success or Failure, the test fails.

\begin{tabular}{l|l|l}
  Length            & Type          & \href{#test-protocol}{Contents} \\
  \hline
  0                 & -             & No data \\
\end{tabular}
\begin{code}

  SkippedTest :: Test () ()

\end{code}
\subsection{Test: Binary Decode}

Checks whether the SUT can correctly decode values and produce the
deconstructed value.

Input:
\begin{tabular}{l|l|l}
  Length            & Type          & \href{#test-protocol}{Contents} \\
  \hline
  \texttt{8}        & Int           & Length \\
  \texttt{[0,]}     & Bytes         & Binary encoding of value \\
\end{tabular}

This test is parameterised by a \href{#basic-data-encoding}{data format}. The
data format is part of the test name string. The test name "BinaryDecode" is
followed by a space and the data format name (e.g. "Word32", "NodeInfo", ...).
Thus, the actual length is \texttt{12 + 1 + n} where \texttt{n} is the length
of the data format name.

Not all binary encodings in the Tox protocol are self-delimiting, so an
explicit length is prefixed to the bytes containing the binary encoding of the
value.

Output: The binary encoding of the \href{#deconstructed-values}{deconstructed
value} in a \href{#success-result}{Success} message.  On decoding failure, this
test must return \href{#failure-result}{Failure}.  If the SUT incorrectly
determines that the byte array was a correct encoding of the data type, the
test fails.

\begin{code}

  BinaryDecode :: DataFormat a -> Test (T.PlainText a) (Deconstruct a)

\end{code}
\subsection{Test: Binary Encode}

Checks whether the SUT can correctly encode values given its constituent parts.
This test is similar to the Binary Decode test, but receives the components the
value is made of, instead of the encoded value itself.

Input:
\begin{tabular}{l|l|l}
  Length            & Type          & \href{#test-protocol}{Contents} \\
  \hline
  \texttt{[0,]}     & Bytes         & Binary encoding of deconstructed value \\
\end{tabular}

Like \href{#test-binary-decode}{Binary Decode}, this test is also parameterised
by \href{#basic-data-encoding}{data format}.

All binary encodings of deconstructed values are self-delimiting, so an
explicit length is not passed here.

Output: The encoded value in a \href{#success-result}{Success} message, not
length-prefixed.
\begin{code}

  BinaryEncode :: DataFormat a -> Test (Deconstruct a) a

\end{code}
\subsection{Test: Distance}

Checks whether the xor-distance metric works correctly.

Input:
\begin{tabular}{l|l|l}
  Length        & Type          & \href{#test-protocol}{Contents} \\
  \hline
  \texttt{32}   & Public Key    & Origin key \\
  \texttt{32}   & Public Key    & Alice key \\
  \texttt{32}   & Public Key    & Bob key \\
\end{tabular}

Output:
\begin{tabular}{l|l|l}
  Length        & Type          & \href{#success-result}{Contents} \\
  \hline
  \texttt{1}    & Ordering      & Less, Equal, or Greater \\
\end{tabular}

The ordering value is encoded as follows:
\begin{tabular}{l|l|l}
  Value   & Encoding & When \\
  \hline
  Less    & 0x00     & distance(Origin, Alice) < distance(Origin, Bob) \\
  Equal   & 0x01     & the distances are equal \\
  Greater & 0x02     & distance(Origin, Alice) > distance(Origin, Bob). \\
\end{tabular}
\begin{code}

  Distance :: Test (T.PublicKey, T.PublicKey, T.PublicKey) Ordering

\end{code}
\subsection{Test: Nonce Increment}

Checks whether the function to increment a nonce works correctly.

Input: A 24-byte nonce.
\begin{tabular}{l|l|l}
  Length        & Type          & \href{#test-protocol}{Contents} \\
  \hline
  \texttt{24}   & Nonce         & Base nonce \\
\end{tabular}

Output:
\begin{tabular}{l|l|l}
  Length        & Type          & \href{#success-result}{Contents} \\
  \hline
  \texttt{24}   & Nonce         & Base nonce + 1 \\
\end{tabular}
\begin{code}

  NonceIncrement :: Test T.Nonce T.Nonce

\end{code}
\subsection{Test: K-Bucket Index}

Checks whether the K-bucket index is computed correctly.

Input: Two public keys for Self and Other.
\begin{tabular}{l|l|l}
  Length        & Type          & \href{#test-protocol}{Contents} \\
  \hline
  \texttt{32}   & Public Key    & Base key \\
  \texttt{32}   & Public Key    & Node key \\
\end{tabular}

Output: either \texttt{Nothing} or \texttt{Just i} in a
\href{#success-result}{Success} message.

\begin{tabular}{l|l|l}
  Length     & Value            & When \\
  \hline
  \texttt{1} & 0x00             & Base key == Node key: \texttt{Nothing} \\
  \texttt{2} & 0x01, \texttt{i} & otherwise: \texttt{Just i} \\
\end{tabular}

The value of \texttt{i} is the k-bucket index of the Node key in a k-buckets
instance with the given Base key.
\begin{code}

  KBucketIndex :: Test (T.PublicKey, T.PublicKey) (Maybe T.KBucketIndex)

\end{code}
\subsection{Test: K-Bucket Nodes}

Input:

\begin{tabular}{l|l|l}
  Length          & Type          & \href{#test-protocol}{Contents} \\
  \hline
  \texttt{8}      & Int           & Bucket size (k) \\
  \texttt{32}     & Public Key    & Base key \\
  \texttt{[0,]}   & [Node Info]   & Added nodes \\
  \texttt{[0,]}   & [Public Key]  & Removed node keys \\
\end{tabular}

The base key is the DHT public key of the simulated node. The added nodes is a
list of nodes to consecutively add to the K-buckets. The removed nodes is a
list of keys for which to consecutively remove the nodes from the K-buckets
after adding all nodes from the added nodes list.

Node Info is encoded with the packet node format.  Recall that all lists are
prefixed with a 64 bit length encoded in big endian.

Output: The buckets, sorted by bucket index. Empty buckets should not appear in
this list.
\begin{tabular}{l|l|l}
  Length          & Type          & \href{#success-result}{Contents} \\
  \hline
  \texttt{8}      & Int           & Length of bucket list \\
  \texttt{[0,]}   & [Bucket]      & The bucket list \\
\end{tabular}

Each bucket is encoded as follows:
\begin{tabular}{l|l|l}
  Length          & Type          & Contents \\
  \hline
  \texttt{1}      & Int           & Bucket index \\
  \texttt{8}      & Int           & Length of nodes list \\
  \texttt{[0,]}   & [Node Info]   & Nodes in the bucket (nodes list) \\
\end{tabular}
\begin{code}

  KBucketNodes :: Test (Int, T.PublicKey, [T.NodeInfo], [T.PublicKey]) [(T.KBucketIndex, [T.NodeInfo])]

deriving instance Show (Test input output)


\end{code}

\section{Result}

The Result type is written to stdout by the SUT. It is a single byte for
Failure (0x00), Success (0x01), and Skipped (0x02), followed by the result
data.

\begin{code}

data Result a

\end{code}
\subsection{Failure Result}

In case of error, a \texttt{Failure} message is returned with an UTF-8 encoded
failure message.

\begin{tabular}{l|l|l}
  Length            & Type            & Contents \\
  \hline
  \texttt{1}        & \texttt{Tag}    & 0x00 (Failure) \\
  \texttt{8}        & \texttt{Int}    & length \\
  \texttt{\$length} & \texttt{String} & error message \\
\end{tabular}
\begin{code}

  = Failure String

\end{code}
\subsection{Success Result}

In case of success, a \texttt{Success} message with an arbitrary piece of data
is returned, depending on the test name in the input.

\begin{tabular}{l|l|l}
  Length            & Type            & Contents \\
  \hline
  \texttt{1}        & \texttt{Tag}    & 0x01 (Success) \\
  \texttt{[0,]}     & \texttt{Bytes}  & Payload \\
\end{tabular}
\begin{code}

  | Success a

\end{code}
\subsection{Skipped Result}

Tests can be skipped by returning a \texttt{Skipped} message. These tests will
be ignored and reported as successful.

\begin{tabular}{l|l|l}
  Length            & Type            & Contents \\
  \hline
  \texttt{1}        & \texttt{Tag}    & 0x02 (Skipped) \\
\end{tabular}
\begin{code}

  | Skipped

  deriving (Eq, Read, Show, Generic, Functor)

instance Binary   a => Binary   (Result a)
instance ToJSON   a => ToJSON   (Result a)
instance FromJSON a => FromJSON (Result a)


{-------------------------------------------------------------------------------
 -
 - :: Test internals.
 -
 ------------------------------------------------------------------------------}



type TestInput a =
  ( Binary    a, Binary    (Deconstruct a)
  , Eq        a, Eq        (Deconstruct a)
  , Show      a, Show      (Deconstruct a)
  , Read      a, Read      (Deconstruct a)
  , Arbitrary a, Arbitrary (Deconstruct a)
  , Construct a
  )


instance Applicative Result where
  pure = Success

  Success f   <*> x = fmap f x
  Failure msg <*> _ = Failure msg
  Skipped     <*> _ = Skipped


instance Monad Result where
  return = pure

  Success x   >>= f = f x
  Failure msg >>= _ = Failure msg
  Skipped     >>= _ = Skipped

  fail = Failure


eraseFailure :: Result a -> Result a
eraseFailure (Failure _) = Failure ""
eraseFailure x = x


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance (Arbitrary a) => Arbitrary (Result a) where
  arbitrary =
    Gen.oneof
      [ fmap Failure arbitrary
      , fmap Success arbitrary
      , return Skipped
      ]

\end{code}

All tests are ran with randomly generated inputs. The test runner has a
\verb!--seed! parameter to set the random seed to a fixed value. This helps
make tests reproducible.
