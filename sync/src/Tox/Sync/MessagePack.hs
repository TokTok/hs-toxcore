{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
-- | MessagePack as tox-proto writes it (@rs-toxcore-c/tox-proto/ENCODING.md@):
-- integers in their shortest form, structures as arrays of their fields,
-- options as arrays of none or one, enums as a bare tag or @[tag, payload]@.
--
-- Reading is strict about types and lengths, and skips values without
-- recursion or allocating for the lengths they claim.
module Tox.Sync.MessagePack
  ( -- * Writing
    encode
  , uint
  , sint
  , bool
  , str
  , bin
  , array
  , opt
  , raw
    -- * Reading
  , Get
  , decodeExact
  , getUint
  , getSint
  , getBool
  , getStr
  , getBin
  , getKey
  , isArray
  , getArrayLength
  , skip
  , getRaw
  , list
    -- * Fields of a structure
  , Fields
  , field
  , left
  , fields
  , tuple
  , optional
  , laterFlag
  , end
  , variant
  , lenient
  , carry
  ) where

import           Control.Monad           (replicateM, unless)
import           Data.Bifunctor          (first)
import           Data.Bits               (shiftL, (.&.), (.|.))
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Either             (fromRight)
import           Data.Int                (Int64)
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as Text
import           Data.Word               (Word64, Word8)

-- Writing

encode :: B.Builder -> BS.ByteString
encode = BL.toStrict . B.toLazyByteString

uint :: Word64 -> B.Builder
uint n
  | n < 0x80 = B.word8 (fromIntegral n)
  | n < 0x100 = B.word8 0xcc <> B.word8 (fromIntegral n)
  | n < 0x10000 = B.word8 0xcd <> B.word16BE (fromIntegral n)
  | n < 0x100000000 = B.word8 0xce <> B.word32BE (fromIntegral n)
  | otherwise = B.word8 0xcf <> B.word64BE n

-- | Non-negative values as 'uint' writes them, as rmp does.
sint :: Int64 -> B.Builder
sint n
  | n >= 0 = uint (fromIntegral n)
  | n >= -32 = B.int8 (fromIntegral n)
  | n >= -128 = B.word8 0xd0 <> B.int8 (fromIntegral n)
  | n >= -32768 = B.word8 0xd1 <> B.int16BE (fromIntegral n)
  | n >= -2147483648 = B.word8 0xd2 <> B.int32BE (fromIntegral n)
  | otherwise = B.word8 0xd3 <> B.int64BE n

bool :: Bool -> B.Builder
bool b = B.word8 (if b then 0xc3 else 0xc2)

str :: Text -> B.Builder
str t = header (BS.length bytes) <> B.byteString bytes
  where
    bytes = Text.encodeUtf8 t
    header n
      | n < 32 = B.word8 (0xa0 .|. fromIntegral n)
      | n < 0x100 = B.word8 0xd9 <> B.word8 (fromIntegral n)
      | n < 0x10000 = B.word8 0xda <> B.word16BE (fromIntegral n)
      | otherwise = B.word8 0xdb <> B.word32BE (fromIntegral n)

bin :: BS.ByteString -> B.Builder
bin bytes = header (BS.length bytes) <> B.byteString bytes
  where
    header n
      | n < 0x100 = B.word8 0xc4 <> B.word8 (fromIntegral n)
      | n < 0x10000 = B.word8 0xc5 <> B.word16BE (fromIntegral n)
      | otherwise = B.word8 0xc6 <> B.word32BE (fromIntegral n)

array :: [B.Builder] -> B.Builder
array items = header (length items) <> mconcat items
  where
    header n
      | n < 16 = B.word8 (0x90 .|. fromIntegral n)
      | n < 0x10000 = B.word8 0xdc <> B.word16BE (fromIntegral n)
      | otherwise = B.word8 0xdd <> B.word32BE (fromIntegral n)

-- | An absent value is @[]@, a present one @[value]@.
opt :: Maybe B.Builder -> B.Builder
opt = array . maybe [] pure

-- | A value encoded already.
raw :: BS.ByteString -> B.Builder
raw = B.byteString

-- Reading

newtype Get a = Get { runGet :: BS.ByteString -> Int -> Either String (a, Int) }

instance Functor Get where
  fmap f (Get g) = Get $ \bs i -> fmap (first f) (g bs i)

instance Applicative Get where
  pure a = Get $ \_ i -> Right (a, i)
  Get f <*> Get g = Get $ \bs i -> case f bs i of
    Left e       -> Left e
    Right (h, j) -> fmap (first h) (g bs j)

instance Monad Get where
  Get g >>= k = Get $ \bs i -> case g bs i of
    Left e       -> Left e
    Right (a, j) -> runGet (k a) bs j

instance MonadFail Get where
  fail e = Get $ \_ _ -> Left e

-- | The whole of the bytes as one value, with nothing after it.
decodeExact :: Get a -> BS.ByteString -> Either String a
decodeExact g bs = case runGet g bs 0 of
  Left e -> Left e
  Right (a, i)
    | i == BS.length bs -> Right a
    | otherwise -> Left "Trailing bytes after value"

peek :: Get Word8
peek = Get $ \bs i -> if i < BS.length bs then Right (BS.index bs i, i) else Left "Cut short"

byte :: Get Word8
byte = Get $ \bs i -> if i < BS.length bs then Right (BS.index bs i, i + 1) else Left "Cut short"

-- | The next bytes, checked to be there before anything is allocated.
take' :: Int -> Get BS.ByteString
take' n = Get $ \bs i ->
  if n <= BS.length bs - i then Right (BS.take n (BS.drop i bs), i + n) else Left "Cut short"

-- | A big-endian number of the given width.
word :: Int -> Get Integer
word n = BS.foldl' (\a b -> a `shiftL` 8 .|. fromIntegral b) 0 <$> take' n

signed :: Int -> Get Integer
signed n = (\w -> if w >= 2 ^ (8 * n - 1) then w - 2 ^ (8 * n) else w) <$> word n

integer :: Get Integer
integer = byte >>= \case
  b | b <= 0x7f -> pure (fromIntegral b)
    | b >= 0xe0 -> pure (fromIntegral b - 256)
  0xcc -> word 1
  0xcd -> word 2
  0xce -> word 4
  0xcf -> word 8
  0xd0 -> signed 1
  0xd1 -> signed 2
  0xd2 -> signed 4
  0xd3 -> signed 8
  _ -> fail "Not an integer"

getUint :: Get Word64
getUint = integer >>= \n ->
  if n >= 0 && n <= fromIntegral (maxBound :: Word64) then pure (fromIntegral n) else fail "Not an unsigned integer"

getSint :: Get Int64
getSint = integer >>= \n ->
  if n >= fromIntegral (minBound :: Int64) && n <= fromIntegral (maxBound :: Int64)
    then pure (fromIntegral n) else fail "Integer out of range"

getBool :: Get Bool
getBool = byte >>= \case
  0xc2 -> pure False
  0xc3 -> pure True
  _ -> fail "Not a boolean"

getStr :: Get Text
getStr = byte >>= \b -> do
  n <- if b .&. 0xe0 == 0xa0 then pure (fromIntegral (b .&. 0x1f)) else case b of
    0xd9 -> fromIntegral <$> word 1
    0xda -> fromIntegral <$> word 2
    0xdb -> fromIntegral <$> word 4
    _    -> fail "Not a string"
  either (const (fail "Not UTF-8")) pure . Text.decodeUtf8' =<< take' n

getBin :: Get BS.ByteString
getBin = byte >>= \b -> do
  n <- case b of
    0xc4 -> fromIntegral <$> word 1
    0xc5 -> fromIntegral <$> word 2
    0xc6 -> fromIntegral <$> word 4
    _    -> fail "Not binary"
  take' n

-- | Binary of exactly 32 bytes: keys, hashes and IDs.
getKey :: Get BS.ByteString
getKey = getBin >>= \b -> if BS.length b == 32 then pure b else fail "Not 32 bytes"

isArray :: Get Bool
isArray = (\b -> b .&. 0xf0 == 0x90 || b == 0xdc || b == 0xdd) <$> peek

getArrayLength :: Get Int
getArrayLength = byte >>= \b ->
  if b .&. 0xf0 == 0x90 then pure (fromIntegral (b .&. 0x0f)) else case b of
    0xdc -> fromIntegral <$> word 2
    0xdd -> fromIntegral <$> word 4
    _    -> fail "Not an array"

-- | Moves past one value, however deeply nested, without building it.
skip :: Get ()
skip = go (1 :: Integer)
  where
    go 0 = pure ()
    go n = byte >>= \b -> case () of
      _ | b <= 0x7f || b >= 0xe0 || b `elem` [0xc0, 0xc2, 0xc3] -> go (n - 1)
        | b .&. 0xf0 == 0x90 -> go (n - 1 + fromIntegral (b .&. 0x0f))
        | b .&. 0xf0 == 0x80 -> go (n - 1 + 2 * fromIntegral (b .&. 0x0f))
        | b .&. 0xe0 == 0xa0 -> take' (fromIntegral (b .&. 0x1f)) >> go (n - 1)
        | otherwise -> case b of
            0xdc -> word 2 >>= \k -> go (n - 1 + k)
            0xdd -> word 4 >>= \k -> go (n - 1 + k)
            0xde -> word 2 >>= \k -> go (n - 1 + 2 * k)
            0xdf -> word 4 >>= \k -> go (n - 1 + 2 * k)
            _ | Just width <- lookup b sized -> word width >>= take' . fromIntegral >> go (n - 1)
              | Just size <- lookup b fixed -> take' size >> go (n - 1)
              -- An extension's length, then its type and data.
              | Just width <- lookup b extensions -> word width >>= take' . (+ 1) . fromIntegral >> go (n - 1)
              | otherwise -> fail "Not a value"
    sized = [(0xc4, 1), (0xc5, 2), (0xc6, 4), (0xd9, 1), (0xda, 2), (0xdb, 4)]
    fixed = [ (0xca, 4), (0xcb, 8), (0xcc, 1), (0xcd, 2), (0xce, 4), (0xcf, 8), (0xd0, 1), (0xd1, 2), (0xd2, 4)
            , (0xd3, 8), (0xd4, 2), (0xd5, 3), (0xd6, 5), (0xd7, 9), (0xd8, 17) ]
    extensions = [(0xc7, 1), (0xc8, 2), (0xc9, 4)]

-- | The next value, as it's encoded.
getRaw :: Get BS.ByteString
getRaw = Get $ \bs i -> case runGet skip bs i of
  Left e       -> Left e
  Right (_, j) -> Right (BS.take (j - i) (BS.drop i bs), j)

-- | An array of values.
list :: Get a -> Get [a]
list item = getArrayLength >>= \n -> replicateM n item

-- Fields

-- | Reading the items of an array, with how many are left.
newtype Fields a = Fields { runFields :: Int -> Get (a, Int) }

instance Functor Fields where
  fmap f (Fields g) = Fields $ \n -> fmap (first f) (g n)

instance Applicative Fields where
  pure a = Fields $ \n -> pure (a, n)
  Fields f <*> Fields g = Fields $ \n -> do
    (h, m) <- f n
    (a, k) <- g m
    pure (h a, k)

instance Monad Fields where
  Fields g >>= k = Fields $ \n -> do
    (a, m) <- g n
    runFields (k a) m

instance MonadFail Fields where
  fail e = lift' (fail e)

lift' :: Get a -> Fields a
lift' g = Fields $ \n -> (, n) <$> g

-- | The next item.
field :: Get a -> Fields a
field g = Fields $ \n -> if n == 0 then fail "Missing field" else (, n - 1) <$> g

left :: Fields Int
left = Fields $ \n -> pure (n, n)

-- | A structure: its fields, skipping those after them this version doesn't
-- know.
fields :: Fields a -> Get a
fields f = do
  n <- getArrayLength
  (a, m) <- runFields f n
  a <$ skipN m

-- | A tuple, which has exactly the items read.
tuple :: Fields a -> Get a
tuple f = do
  n <- getArrayLength
  (a, m) <- runFields f n
  unless (m == 0) (fail "Unexpected fields")
  pure a

skipN :: Int -> Get ()
skipN m = mapM_ (const skip) [1 .. m]

-- | An option, as 'opt' writes it. Missing at the end, as in older
-- encodings, it's absent too.
optional :: Get a -> Fields (Maybe a)
optional g = left >>= \case
  0 -> pure Nothing
  _ -> field $ do
    array' <- isArray
    unless array' (fail "Not an option")
    n <- getArrayLength
    case n of
      0 -> pure Nothing
      1 -> Just <$> g
      _ -> fail "Not an option"

-- | A flag added to a structure later: false where missing, as in older
-- encodings.
laterFlag :: Fields Bool
laterFlag = left >>= \case
  0 -> pure False
  _ -> field getBool

-- | Skips the items left.
end :: Fields ()
end = left >>= \n -> mapM_ (const (field skip)) [1 .. n]

-- | An enum value: a bare tag, or @[tag, payload]@. @known@ reads the
-- payload of a tag it knows, from the items after the tag, or gives nothing
-- for one it doesn't, whose payload is then skipped for @unknown@.
variant :: (Word64 -> Fields (Maybe a)) -> (Word64 -> Get a) -> Get a
variant known unknown = isArray >>= \case
  False -> getUint >>= unknown
  True -> do
    n <- getArrayLength
    (tag, m) <- runFields (field getUint) n
    (value, rest) <- runFields (known tag) m
    case value of
      Just a -> do
        unless (rest == 0) (fail "Unexpected fields")
        pure a
      Nothing -> skipN rest >> unknown tag

-- | A @#[tox(lenient)]@ enum value: @[kind, [fields]]@. A kind whose fields
-- don't decode is unknown, as one this version doesn't know.
lenient :: (Word64 -> Fields (Maybe a)) -> (Word64 -> Get a) -> Get a
lenient known = variant $ \kind -> left >>= \case
  1 -> field getRaw >>= \bytes -> pure $ fromRight Nothing (decodeExact (fields (known kind)) bytes)
  _ -> pure Nothing

-- | @new@, the encoding of a structure, with the fields of @old@, an
-- encoding of it by a version that knows more of them, after its own.
carry :: BS.ByteString -> BS.ByteString -> Either String BS.ByteString
carry old new = do
  (had, oldFields) <- runGet getArrayLength old 0
  (has, newFields) <- runGet getArrayLength new 0
  if had <= has then pure new else do
    (_, kept) <- runGet (skipN has) old oldFields
    pure $ encode $ arrayHeader had <> B.byteString (BS.drop newFields new) <> B.byteString (BS.drop kept old)
  where
    arrayHeader n
      | n < 16 = B.word8 (0x90 .|. fromIntegral n)
      | n < 0x10000 = B.word8 0xdc <> B.word16BE (fromIntegral n)
      | otherwise = B.word8 0xdd <> B.word32BE (fromIntegral n)
