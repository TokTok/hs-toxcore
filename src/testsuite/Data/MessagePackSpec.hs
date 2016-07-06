{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Data.MessagePackSpec where

import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen        as Gen

import           Control.Applicative        ((<$>), (<*>))
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IntMap
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import qualified Data.Text.Lazy             as LT
import           GHC.Generics               (Generic)

import           Data.MessagePack


data Record = Record Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack Record


data Foo
  = Foo1
  | Foo2 Int
  | Foo3 Int
  | Foo4 Int
  | Foo5 Int
  | Foo6 { unFoo3 :: Int }
  | Foo7 (Maybe Foo)
  | Foo8 Int
  | Foo9 Int Int
  | Foo10 Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack Foo

instance Arbitrary Foo where
  arbitrary = Gen.oneof
    [ return Foo1
    , Foo2 <$> arbitrary
    , Foo3 <$> arbitrary
    , Foo4 <$> arbitrary
    , Foo5 <$> arbitrary
    , Foo6 <$> arbitrary
    , Foo7 <$> arbitrary
    , Foo8 <$> arbitrary
    , Foo9 <$> arbitrary <*> arbitrary
    , Foo10 <$> arbitrary <*> arbitrary <*> arbitrary
    ]


newtype Map k v = Map (Map.Map k v) deriving (Eq, Show, Generic)
newtype IntMap v = IntMap (IntMap.IntMap v) deriving (Eq, Show, Generic)

instance (Ord k, MessagePack k, MessagePack v) => MessagePack (Map k v)
instance MessagePack v => MessagePack (IntMap v)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = Map . Map.fromList <$> arbitrary

instance Arbitrary v => Arbitrary (IntMap v) where
  arbitrary = IntMap . IntMap.fromList <$> arbitrary

instance (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap.HashMap k v) where
  arbitrary = HashMap.fromList <$> arbitrary

instance (Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5, Arbitrary a6) => Arbitrary (a1, a2, a3, a4, a5, a6) where
  arbitrary = (,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5, Arbitrary a6, Arbitrary a7) => Arbitrary (a1, a2, a3, a4, a5, a6, a7) where
  arbitrary = (,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5, Arbitrary a6, Arbitrary a7, Arbitrary a8) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8) where
  arbitrary = (,,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5, Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  arbitrary = (,,,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

mid :: MessagePack a => a -> a
mid = Maybe.fromJust . unpack . pack


intMid :: Int64 -> Int64
intMid = mid


coerce :: (MessagePack a, MessagePack b) => a -> Maybe b
coerce = unpack . pack


spec :: Spec
spec = do
  describe "type coercion" $ do
    it "bool<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` (Nothing :: Maybe Bool)

    it "int<-bool" $
      property $ \(a :: Bool) -> coerce a `shouldBe` (Nothing :: Maybe Int)

    it "float<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` Just (fromIntegral a :: Float)
    it "float<-double" $
      property $ \(a :: Double) -> coerce a `shouldBe` Just (realToFrac a :: Float)
    it "float<-string" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe Float)

    it "double<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` Just (fromIntegral a :: Double)
    it "double<-float" $
      property $ \(a :: Float) -> coerce a `shouldBe` Just (realToFrac a :: Double)
    it "double<-string" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe Double)

    it "bin<-string" $
      property $ \(a :: S.ByteString) -> coerce a `shouldBe` (Nothing :: Maybe String)

    it "string<-bin" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe S.ByteString)

  describe "Identity Properties" $ do
    let sizes = [0xf, 0x10, 0x1f, 0x20, 0xff, 0x100, 0xffff, 0x10000]

    it "map encodings" $ do
      let rt n = let a = IntMap $ IntMap.fromList [(x, -x) | x <- [0..n]] in a `shouldBe` mid a
      mapM_ rt sizes

    it "list encodings" $ do
      let rt n = let a = replicate n "hello" in a `shouldBe` mid a
      mapM_ rt sizes

    it "string encodings" $ do
      let rt n = let a = replicate n 'a' in a `shouldBe` mid a
      mapM_ rt sizes

    it "bytestring encodings" $ do
      let rt n = let a = S.pack $ replicate n 'a' in a `shouldBe` mid a
      mapM_ rt sizes

    it "ext encodings" $ do
      let rt n = let a = ObjectExt 0 $ S.pack $ replicate n 'a' in a `shouldBe` mid a
      mapM_ rt [0..20]
      mapM_ rt sizes

    it "int encodings" $ do
      (-0x7fffffffffffffff) `shouldBe` intMid (-0x7fffffffffffffff)
      (-0x80000000) `shouldBe` intMid (-0x80000000)
      (-0x7fffffff) `shouldBe` intMid (-0x7fffffff)
      (-0x8000) `shouldBe` intMid (-0x8000)
      (-0x7fff) `shouldBe` intMid (-0x7fff)
      (-1) `shouldBe` intMid (-1)
      0 `shouldBe` intMid 0
      1 `shouldBe` intMid 1
      0x7fff `shouldBe` intMid 0x7fff
      0x8000 `shouldBe` intMid 0x8000
      0x7fffffff `shouldBe` intMid 0x7fffffff
      0x80000000 `shouldBe` intMid 0x80000000
      0x7fffffffffffffff `shouldBe` intMid 0x7fffffffffffffff

    it "int" $
      property $ \(n, a) -> ObjectExt n a `shouldBe` mid (ObjectExt n a)
    it "int" $
      property $ \(a :: Int) -> a `shouldBe` mid a
    it "nil" $
      property $ \(a :: ()) -> a `shouldBe` mid a
    it "bool" $
      property $ \(a :: Bool) -> a `shouldBe` mid a
    it "float" $
      property $ \(a :: Float) -> a `shouldBe` mid a
    it "double" $
      property $ \(a :: Double) -> a `shouldBe` mid a
    it "string" $
      property $ \(a :: String) -> a `shouldBe` mid a
    it "bytestring" $
      property $ \(a :: S.ByteString) -> a `shouldBe` mid a
    it "lazy-bytestring" $
      property $ \(a :: L.ByteString) -> a `shouldBe` mid a
    it "lazy-text" $
      property $ \(a :: LT.Text) -> a `shouldBe` mid a
    it "maybe int" $
      property $ \(a :: (Maybe Int)) -> a `shouldBe` mid a
    it "[int]" $
      property $ \(a :: [Int]) -> a `shouldBe` mid a
    it "[string]" $
      property $ \(a :: [String]) -> a `shouldBe` mid a
    it "(int, int)" $
      property $ \(a :: (Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int)" $
      property $ \(a :: (Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "[(int, double)]" $
      property $ \(a :: [(Int, Double)]) -> a `shouldBe` mid a
    it "[(string, string)]" $
      property $ \(a :: [(String, String)]) -> a `shouldBe` mid a
    it "Assoc [(string, int)]" $
      property $ \(a :: Assoc [(String, Int)]) -> a `shouldBe` mid a
    it "Map String Int" $
      property $ \(a :: Map String Int) -> a `shouldBe` mid a
    it "IntMap Int" $
      property $ \(a :: IntMap Int) -> a `shouldBe` mid a
    it "HashMap String Int" $
      property $ \(a :: HashMap.HashMap String Int) -> a `shouldBe` mid a
    it "maybe int" $
      property $ \(a :: Maybe Int) -> a `shouldBe` mid a
    it "maybe nil" $
      property $ \(a :: Maybe ()) -> a `shouldBe` mid a

   -- FIXME: this test is also failing
   --
   -- it should probably be decoded somewhat specially with ObjectExt ?
   --
   -- it "maybe maybe int" $
   --   property $ \(a :: Maybe (Maybe Int)) -> a `shouldBe` mid a
   --
   -- by looking at msgpack specification it looks like Haskells Maybe
   -- type should be probably decoded with custom ObjectExt
   --
    it "maybe bool" $
      property $ \(a :: Maybe Bool) -> a `shouldBe` mid a
    it "maybe double" $
      property $ \(a :: Maybe Double) -> a `shouldBe` mid a
    it "maybe string" $
      property $ \(a :: Maybe String) -> a `shouldBe` mid a
    it "maybe bytestring" $
      property $ \(a :: Maybe S.ByteString) -> a `shouldBe` mid a
    it "maybe lazy-bytestring" $
      property $ \(a :: Maybe L.ByteString) -> a `shouldBe` mid a
    it "maybe [int]" $
      property $ \(a :: Maybe [Int]) -> a `shouldBe` mid a
    it "maybe [string]" $
      property $ \(a :: Maybe [String]) -> a `shouldBe` mid a
    it "maybe (int, int)" $
      property $ \(a :: Maybe (Int, Int)) -> a `shouldBe` mid a
    it "maybe (int, int, int)" $
      property $ \(a :: Maybe (Int, Int, Int)) -> a `shouldBe` mid a
    it "maybe (int, int, int, int)" $
      property $ \(a :: Maybe (Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "maybe (int, int, int, int, int)" $
      property $ \(a :: Maybe (Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "maybe [(int, double)]" $
      property $ \(a :: Maybe [(Int, Double)]) -> a `shouldBe` mid a
    it "maybe [(string, string)]" $
      property $ \(a :: Maybe [(String, String)]) -> a `shouldBe` mid a
    it "maybe (Assoc [(string, int)])" $
      property $ \(a :: Maybe (Assoc [(String, Int)])) -> a `shouldBe` mid a

    it "generics" $
      property $ \(a :: Foo) -> a `shouldBe` mid a
    it "arbitrary message" $
      property $ \(a :: Object) -> a `shouldBe` mid a

  describe "show" $ do
    it "Foo" $ do
      show (toObject Foo1) `shouldBe` "ObjectInt 0"
      show (toObject $ Foo3 3) `shouldBe` "ObjectArray [ObjectInt 2,ObjectInt 3]"
      show (toObject $ Foo9 3 5) `shouldBe` "ObjectArray [ObjectInt 8,ObjectArray [ObjectInt 3,ObjectInt 5]]"
      show (toObject $ Foo10 3 5 7) `shouldBe` "ObjectArray [ObjectInt 9,ObjectArray [ObjectInt 3,ObjectInt 5,ObjectInt 7]]"

    it "Record" $
      show (toObject $ Record 3 5 7) `shouldBe` "ObjectArray [ObjectInt 3,ObjectInt 5,ObjectInt 7]"
