{-# LANGUAGE LambdaCase #-}
-- | Settings: what the account chooses for itself or its conversations, the
-- same on every device of it.
module Tox.Sync.Setting
  ( Scope (..)
  , Setting (..)
  , PictureEncoding (..)
  , Kept (..)
  , kept
  , settingKind
  , maxSetting
  , maxTime
  , scope
  , getScope
  , keptSetting
  , getKeptSetting
  , withSetting
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import           Data.Int                (Int64)
import           Data.Word               (Word64)

import           Tox.Sync.Id
import qualified Tox.Sync.MessagePack    as MP

-- | What a setting applies to.
data Scope
  = AccountScope
  | ConversationScope GraphId
  | UnknownScope Word64
    -- ^ From a newer version: applies to nothing here.
  deriving (Eq, Ord, Show)

data PictureEncoding = Original | Qoi
  deriving (Eq, Show, Enum, Bounded)

data Setting
  = Mute (Maybe Word64) Bool
    -- ^ No notifications until the time, in ms since the epoch, or ever
    -- where it's none, but for messages naming the account where the flag
    -- is set.
  | PictureFormat PictureEncoding
  | UnknownSetting Word64
    -- ^ Of a kind this version doesn't know, or that doesn't decode: kept
    -- and passed on, but not acted on.
  deriving (Eq, Show)

-- | A value with the bytes it was read from, which it's written as, so what
-- this version doesn't know of it is kept. Equal by value.
data Kept a = Kept
  { keptValue :: a
  , keptBytes :: Maybe BS.ByteString
  } deriving (Show)

instance Eq a => Eq (Kept a) where
  a == b = keptValue a == keptValue b

kept :: a -> Kept a
kept a = Kept a Nothing

settingKind :: Setting -> Word64
settingKind = \case
  Mute _ _ -> 0
  PictureFormat _ -> 1
  UnknownSetting kind -> kind

-- | The longest a setting's encoding may be, as every device keeps them all.
maxSetting :: Int
maxSetting = 1024

-- | The most milliseconds from the epoch a time may be, either way: as far
-- as ECMAScript's and Dart's dates go.
maxTime :: Int64
maxTime = 8640000000000000

-- | The account is a bare tag, a conversation @[tag, graph]@.
scope :: Scope -> B.Builder
scope = \case
  AccountScope -> MP.uint 0
  ConversationScope (GraphId g) -> MP.array [MP.uint 1, MP.bin g]
  UnknownScope _ -> error "Unknown scopes are not written"

getScope :: MP.Get Scope
getScope = MP.variant known unknown
  where
    known 1 = Just . ConversationScope . GraphId <$> MP.field MP.getKey
    known _ = pure Nothing
    unknown 0   = pure AccountScope
    unknown tag = pure (UnknownScope tag)

fieldsOf :: Setting -> B.Builder
fieldsOf = \case
  Mute end mentions -> MP.array [MP.opt (MP.uint <$> end), MP.bool mentions]
  PictureFormat format -> MP.array [MP.uint (fromIntegral (fromEnum format))]
  UnknownSetting _ -> error "Unknown settings are not written"

-- | @[kind, [fields]]@, its fields in an array even where there's one, so
-- every kind can gain fields, and a kept one as it came.
keptSetting :: Kept Setting -> B.Builder
keptSetting (Kept _ (Just bytes)) = MP.raw bytes
keptSetting (Kept s Nothing)      = MP.array [MP.uint (settingKind s), fieldsOf s]

getKeptSetting :: MP.Get (Kept Setting)
getKeptSetting = do
  bytes <- MP.getRaw
  either fail (pure . (`Kept` Just bytes)) (MP.decodeExact (MP.lenient known (pure . UnknownSetting)) bytes)
  where
    known = \case
      0 -> do
        end <- MP.optional MP.getUint
        mentions <- MP.field MP.getBool
        case end of
          Just ms | ms > fromIntegral maxTime -> fail "Not a time"
          _                                   -> pure (Just (Mute end mentions))
      -- Unknown formats send pictures as they are.
      1 -> MP.field MP.getUint >>= \n ->
        pure (Just (PictureFormat (if n == 1 then Qoi else Original)))
      _ -> pure Nothing

-- | A setting in place of a kept one, with the fields of the kept one after
-- its own as they came.
withSetting :: Setting -> Kept Setting -> Kept Setting
withSetting s old = Kept s (Just bytes)
  where
    fields = MP.encode (fieldsOf s)
    carried = case keptBytes old of
      Just previous
        | Right (kind, oldFields) <- MP.decodeExact header previous
        , kind == settingKind s
        , Right both <- MP.carry oldFields fields -> both
      _ -> fields
    header = MP.tuple ((,) <$> MP.field MP.getUint <*> MP.field MP.getRaw)
    bytes = MP.encode (MP.array [MP.uint (settingKind s), MP.raw carried])
