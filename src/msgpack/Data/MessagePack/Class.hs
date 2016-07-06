{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Object
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack object definition
--
--------------------------------------------------------------------

module Data.MessagePack.Class
  ( MessagePack (..)
  , GMessagePack (..)
  ) where

import           Control.Applicative     ((<$>), (<*>), (<|>))
import           Control.Arrow           ((***))
import           Control.Monad           ((>=>))
import           Data.Bits               (shiftR)
import qualified Data.ByteString         as S
import qualified Data.ByteString.Lazy    as L
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Int                (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Vector             as V
import           Data.Word               (Word16, Word32, Word64, Word8)
import           Debug.Trace             (trace)
import           GHC.Generics

import           Data.MessagePack.Assoc
import           Data.MessagePack.Object


class MessagePack a where
  toObject   :: a -> Object
  fromObject :: Object -> Maybe a

  default toObject :: (Generic a, GMessagePack (Rep a)) => a -> Object
  toObject = genericToObject
  default fromObject :: (Generic a, GMessagePack (Rep a)) => Object -> Maybe a
  fromObject = genericFromObject


genericToObject :: (Generic a, GMessagePack (Rep a)) => a -> Object
genericToObject = gToObject . from

genericFromObject :: (Generic a, GMessagePack (Rep a)) => Object -> Maybe a
genericFromObject x = to <$> gFromObject x


-- integral instances

toInt :: Integral a => a -> Int64
toInt = fromIntegral

fromInt :: Integral a => Int64 -> a
fromInt = fromIntegral

instance MessagePack Int64 where
  toObject = ObjectInt
  fromObject = \case
    ObjectInt n -> Just n
    _           -> Nothing

instance MessagePack Int8   where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Int16  where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Int32  where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Int    where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }

instance MessagePack Word8  where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word16 where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word32 where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word64 where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }


-- core instances

instance MessagePack Object where
  toObject = id
  fromObject = Just

instance MessagePack () where
  toObject _ = ObjectArray V.empty
  fromObject = \case
    ObjectArray v | V.null v -> Just ()
    _                        -> Nothing

instance MessagePack Bool where
  toObject = ObjectBool
  fromObject = \case
    ObjectBool b -> Just b
    _            -> Nothing

instance MessagePack Float where
  toObject = ObjectFloat
  fromObject = \case
    ObjectInt    n -> Just $ fromIntegral n
    ObjectFloat  f -> Just f
    ObjectDouble d -> Just $ realToFrac d
    _              -> Nothing

instance MessagePack Double where
  toObject = ObjectDouble
  fromObject = \case
    ObjectInt    n -> Just $ fromIntegral n
    ObjectFloat  f -> Just $ realToFrac f
    ObjectDouble d -> Just d
    _              -> Nothing

instance MessagePack S.ByteString where
  toObject = ObjectBin
  fromObject = \case
    ObjectBin r -> Just r
    _           -> Nothing

-- Because of overlapping instance, this must be above [a].
-- IncoherentInstances and TypeSynonymInstances are required for this to work.
instance MessagePack String where
  toObject = toObject . T.pack
  fromObject obj = T.unpack <$> fromObject obj

instance MessagePack a => MessagePack (V.Vector a) where
  toObject = ObjectArray . V.map toObject
  fromObject = \case
    ObjectArray xs -> V.mapM fromObject xs
    _              -> Nothing

instance (MessagePack a, MessagePack b) => MessagePack (Assoc (V.Vector (a, b))) where
  toObject (Assoc xs) = ObjectMap $ V.map (toObject *** toObject) xs
  fromObject = \case
    ObjectMap xs ->
      Assoc <$> V.mapM (\(k, v) -> (,) <$> fromObject k <*> fromObject v) xs
    _ ->
      Nothing


-- util instances

-- nullable

instance MessagePack a => MessagePack (Maybe a) where
  toObject = \case
    Just a  -> toObject a
    Nothing -> ObjectNil

  fromObject = \case
    ObjectNil -> Just Nothing
    obj       -> Just <$> fromObject obj


-- UTF8 string like

instance MessagePack L.ByteString where
  toObject = ObjectBin . L.toStrict
  fromObject obj = L.fromStrict <$> fromObject obj

instance MessagePack T.Text where
  toObject = ObjectStr
  fromObject = \case
    ObjectStr s -> Just s
    _           -> Nothing

instance MessagePack LT.Text where
  toObject = toObject . LT.toStrict
  fromObject obj = LT.fromStrict <$> fromObject obj


-- array like

instance MessagePack a => MessagePack [a] where
  toObject = toObject . V.fromList
  fromObject obj = V.toList <$> fromObject obj


-- map like

instance (MessagePack k, MessagePack v) => MessagePack (Assoc [(k, v)]) where
  toObject = toObject . Assoc . V.fromList . unAssoc
  fromObject obj = Assoc . V.toList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Ord k) => MessagePack (Map.Map k v) where
  toObject = toObject . Assoc . Map.toList
  fromObject obj = Map.fromList . unAssoc <$> fromObject obj

instance MessagePack v => MessagePack (IntMap.IntMap v) where
  toObject = toObject . Assoc . IntMap.toList
  fromObject obj = IntMap.fromList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Hashable k, Eq k) => MessagePack (HashMap.HashMap k v) where
  toObject = toObject . Assoc . HashMap.toList
  fromObject obj = HashMap.fromList . unAssoc <$> fromObject obj


-- tuples

instance (MessagePack a1, MessagePack a2) => MessagePack (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  fromObject (ObjectArray [a1, a2]) = (,) <$> fromObject a1 <*> fromObject a2
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3) => MessagePack (a1, a2, a3) where
  toObject (a1, a2, a3) = ObjectArray [toObject a1, toObject a2, toObject a3]
  fromObject (ObjectArray [a1, a2, a3]) = (,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4) => MessagePack (a1, a2, a3, a4) where
  toObject (a1, a2, a3, a4) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4]
  fromObject (ObjectArray [a1, a2, a3, a4]) = (,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5) => MessagePack (a1, a2, a3, a4, a5) where
  toObject (a1, a2, a3, a4, a5) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5]
  fromObject (ObjectArray [a1, a2, a3, a4, a5]) = (,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6) => MessagePack (a1, a2, a3, a4, a5, a6) where
  toObject (a1, a2, a3, a4, a5, a6) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6]) = (,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7) => MessagePack (a1, a2, a3, a4, a5, a6, a7) where
  toObject (a1, a2, a3, a4, a5, a6, a7) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7]) = (,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8]) = (,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8, MessagePack a9) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8, toObject a9]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8, a9]) = (,,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8 <*> fromObject a9
  fromObject _ = Nothing


-- Generic serialisation.

class GMessagePack f where
  gToObject   :: f a -> Object
  gFromObject :: Object -> Maybe (f a)
