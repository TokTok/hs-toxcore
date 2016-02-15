{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DefaultSignatures  #-}
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
import           Data.Word                              (Word32)
import           GHC.Generics                           (Generic)
import qualified Network.Tox.Crypto.Key                 as T (PublicKey)
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


-- A Construct instance shows the isomorphism between the deconstructed value
-- and the actual value.
class Construct a where

  -- A deconstructed value contains sufficient information to construct a
  -- value.  The binary representation of a deconstructed value is usually
  -- different from (and simpler than) the actual packed value.
  type Deconstruct a
  type Deconstruct a = a

  -- The construct function shows how a value is constructed from its
  -- components.
  construct :: Deconstruct a -> a

  -- By default, there is no special construction, so the construct function is
  -- the identity function.
  default construct :: a -> a
  construct = id

  -- The deconstruct value produces the separate components from a constructed
  -- value.
  deconstruct :: a -> Deconstruct a

  -- Similarly to the construct function, the default deconstruct function is
  -- the identity function.
  default deconstruct :: a -> a
  deconstruct = id


instance Construct T.NodeInfo where

  -- The NodeInfo deconstructed value is a simple serialisation of each field,
  -- without packing the protocol and address family into the same byte using
  -- magic values.
  type Deconstruct T.NodeInfo =
    -- 0x00 for UDP, 0x01 for TCP.
    ( T.TransportProtocol
    -- 0x00 for IPv4, 0x01 for IPv6.
    -- 4 bytes for IPv4, 16 bytes for IPv6.
    , T.HostAddress
    -- 2 bytes.
    , T.PortNumber
    -- 32 bytes
    , T.PublicKey
    )

  construct (protocol, addr, port, key) =
    T.NodeInfo protocol (T.SocketAddress addr port) key
  deconstruct (T.NodeInfo protocol (T.SocketAddress addr port) key) =
    (protocol, addr, port, key)


-- All other types have no special deconstructed type and are just used as-is.
instance Construct Word32
instance Construct String


data Test input output where

  -- This test receives no data and returns no data, but expects the test to
  -- always fail.
  FailureTest :: Test () ()

  -- This test always succeeds, also with no data.
  SuccessTest :: Test () ()

  -- This test must be skipped, also with no data. If the SUT always returns
  -- Success or Failure, the test fails.
  SkippedTest :: Test () ()

  -- Checks whether the SUT can correctly decode and re-encode values.
  --
  -- Input: 8 bytes length, followed by a byte array of that length. The byte
  -- array contains the binary encoding of a value according to the protocol
  -- specification.
  --
  -- Output: The binary encoding of the deconstructed value (see Deconstruct).
  -- On decoding failure, this test must return Failure. If the SUT incorrectly
  -- determines that the byte array was a correct encoding of the data type,
  -- the test fails.
  --
  -- This test is parameterised by a Data Format. The Data Format is part of
  -- the test name string. The test name "BinaryDecode" is followed by a space
  -- and the Data Format name.
  BinaryDecode :: DataFormat a -> Test (T.PlainText a) (Deconstruct a)

  -- Checks whether the SUT can correctly encode values given its constituent
  -- parts.
  --
  -- Input: A deconstructed value. See the associated Deconstruct type instance
  -- for what this looks like.
  --
  -- Output: An encoded value.
  --
  -- This test is similar to BinaryDecode, but receives the components the
  -- value is made of, instead of the encoded value itself.
  BinaryEncode :: DataFormat a -> Test (Deconstruct a) a

  -- Checks whether the xor-distance metric works correctly.
  --
  -- Input: Three 32 byte public keys (not separated by anything, they are
  -- simply concatenated) for Origin, Alice, and Bob.
  --
  -- Output: A single byte containing
  --   0 if distance(Origin, Alice) < distance(Origin, Bob)
  --   1 if the distances are equal
  --   2 if distance(Origin, Alice) > distance(Origin, Bob).
  Distance :: Test (T.PublicKey, T.PublicKey, T.PublicKey) Ordering

  -- Checks whether the K-bucket index is computed correctly.
  --
  -- Input: Two public keys for Self and Other.
  --
  -- Output: One or two bytes.
  --   0 for Nothing (if Self == Other)
  --   1 followed by the index in a single byte otherwise.
  KBucketIndex :: Test (T.PublicKey, T.PublicKey) (Maybe T.KBucketIndex)

  -- Input:
  --   - The bucket size (k): 8 bytes.
  --   - The base key (DHT public key): 32 bytes.
  --   - A list of nodes to consecutively add to the K-buckets.
  --   - A list of nodes to consecutively remove from the K-buckets after
  --     adding all nodes.
  --
  -- Lists are encoded as 64 bit length followed by each element concatenated.
  -- NodeInfo is encoded with the packet node format.
  --
  -- Output: The buckets, sorted by bucket index. Empty buckets should not
  -- appear in this list.
  KBucketNodes :: Test (Int, T.PublicKey, [T.NodeInfo], [T.NodeInfo]) [(T.KBucketIndex, [T.NodeInfo])]

deriving instance Show (Test input output)


-- This is the list of data types used in the BinaryDecode and BinaryEncode
-- tests. They correspond to the types defined in the protocol specification.
data DataFormat a where
  Word32     :: DataFormat Word32
  String     :: DataFormat String
  NodeInfo   :: DataFormat T.NodeInfo

deriving instance Show (DataFormat a)


-- The Result type is written to stdout by the SUT. It is a single byte (0, 1,
-- 2) for Failure, Success, and Skipped, followed by the result data.
data Result a

  -- Byte 0, followed by 8 bytes length and a byte array of that length
  -- containing an UTF-8 encoded failure message.
  = Failure String

  -- Byte 1, followed by the expected test result data (see the Test type).
  | Success a

  -- Byte 2.
  | Skipped
  deriving (Eq, Read, Show, Generic)

instance Binary a => Binary (Result a)
instance ToJSON a => ToJSON (Result a)
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


instance Functor Result where
  fmap f (Success x  ) = Success $ f x
  fmap _ (Failure msg) = Failure msg
  fmap _  Skipped      = Skipped


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
