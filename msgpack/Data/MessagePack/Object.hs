{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
module Data.MessagePack.Object (Object (..)) where

import           Control.Applicative       ((<$), (<$>), (<*>), (<|>))
import           Control.DeepSeq           (NFData (..))
import           Data.Binary               (Binary (get, put), Get, Put)
import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as L
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import           Data.Typeable             (Typeable)
import qualified Data.Vector               as V
import           Data.Word                 (Word8)
import           Prelude                   hiding (putStr)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen       as Gen

import           Data.MessagePack.Get
import           Data.MessagePack.Put


-- | Object Representation of MessagePack data.
data Object
  = ObjectNil
    -- ^ represents nil
  | ObjectBool                  !Bool
    -- ^ represents true or false
  | ObjectInt    {-# UNPACK #-} !Int
    -- ^ represents an integer
  | ObjectFloat  {-# UNPACK #-} !Float
    -- ^ represents a floating point number
  | ObjectDouble {-# UNPACK #-} !Double
    -- ^ represents a floating point number
  | ObjectStr                   !T.Text
    -- ^ extending Raw type represents a UTF-8 string
  | ObjectBin                   !S.ByteString
    -- ^ extending Raw type represents a byte array
  | ObjectArray                 !(V.Vector Object)
    -- ^ represents a sequence of objects
  | ObjectMap                   !(V.Vector (Object, Object))
    -- ^ represents key-value pairs of objects
  | ObjectExt    {-# UNPACK #-} !Word8 !S.ByteString
    -- ^ represents a tuple of an integer and a byte array where
    -- the integer represents type information and the byte array represents data.
  deriving (Show, Eq, Ord, Typeable)

instance NFData Object where
  rnf obj = case obj of
    ObjectArray a -> rnf a
    ObjectMap   m -> rnf m
    _             -> ()

instance Binary Object where
  get = getObject
  put = putObject


getObject :: Get Object
getObject =
      ObjectNil    <$  getNil
  <|> ObjectBool   <$> getBool
  <|> ObjectInt    <$> getInt
  <|> ObjectFloat  <$> getFloat
  <|> ObjectDouble <$> getDouble
  <|> ObjectStr    <$> getStr
  <|> ObjectBin    <$> getBin
  <|> ObjectArray  <$> getArray getObject
  <|> ObjectMap    <$> getMap getObject getObject
  <|> uncurry ObjectExt <$> getExt

putObject :: Object -> Put
putObject = \case
  ObjectNil      -> putNil
  ObjectBool   b -> putBool b
  ObjectInt    n -> putInt n
  ObjectFloat  f -> putFloat f
  ObjectDouble d -> putDouble d
  ObjectStr    t -> putStr t
  ObjectBin    b -> putBin b
  ObjectArray  a -> putArray putObject a
  ObjectMap    m -> putMap putObject putObject m
  ObjectExt  b r -> putExt b r


instance Arbitrary Object where
  arbitrary = Gen.sized $ \n -> Gen.oneof
    [ return ObjectNil
    , ObjectBool   <$> arbitrary
    , ObjectInt    <$> arbitrary
    , ObjectFloat  <$> arbitrary
    , ObjectDouble <$> arbitrary
    , ObjectStr    <$> arbitrary
    , ObjectBin    <$> arbitrary
    , ObjectArray  <$> Gen.resize (n `div` 2) arbitrary
    , ObjectMap    <$> Gen.resize (n `div` 4) arbitrary
    , ObjectExt    <$> arbitrary <*> arbitrary
    ]

instance Arbitrary S.ByteString where
  arbitrary = S.pack <$> arbitrary

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary LT.Text where
  arbitrary = LT.pack <$> arbitrary

instance Arbitrary v => Arbitrary (V.Vector v) where
  arbitrary = V.fromList <$> arbitrary
