{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
module Network.Tox.Binary
  ( typeName
  , encode
  , decode
  ) where

import           Data.Binary          (Binary)
import           Data.ByteString      (ByteString)
import           Data.Kind            (Type)
import           Data.Typeable        (Typeable)
import qualified Data.Typeable        as Typeable

import qualified Network.Tox.Encoding as Encoding


typeName :: Typeable (a :: Type) => proxy a -> String
typeName (_ :: proxy a) =
  show . Typeable.typeOf $ (undefined :: a)



--------------------------------------------------------------------------------
--
-- :: decode
--
--------------------------------------------------------------------------------


decode :: Binary a => ByteString -> Maybe a
decode = Encoding.decode


--------------------------------------------------------------------------------
--
-- :: encode
--
--------------------------------------------------------------------------------


encode :: Binary a => a -> ByteString
encode = Encoding.encode
