{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
module Network.Tox.TypeName
  ( typeName
  ) where

import           Data.Kind     (Type)
import           Data.Typeable (Typeable)
import qualified Data.Typeable as Typeable


typeName :: Typeable (a :: Type) => proxy a -> String
typeName (_ :: proxy a) =
  show . Typeable.typeOf $ (undefined :: a)
