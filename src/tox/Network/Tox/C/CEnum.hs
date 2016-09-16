{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Network.Tox.C.CEnum where

import           Control.Applicative   ((<$>))
import           Foreign.C.Types       (CInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (Storable (..))


newtype CEnum a = CEnum { unCEnum :: CInt }
  deriving (Storable)

instance (Enum a, Show a) => Show (CEnum a) where
  show cen = show (toEnum $ fromIntegral $ unCEnum cen :: a)


toCEnum :: Enum a => a -> CEnum a
toCEnum = CEnum . fromIntegral . fromEnum


fromCEnum :: Enum a => CEnum a -> a
fromCEnum = toEnum . fromIntegral . unCEnum


type CErr err = Ptr (CEnum err)

callErrFun :: (Eq err, Enum err, Bounded err)
           => (CErr err -> IO r) -> IO (Either err r)
callErrFun f = alloca $ \errPtr -> do
  res <- f errPtr
  err <- toEnum . fromIntegral . unCEnum <$> peek errPtr
  return $ if err /= minBound
    then Left  err
    else Right res
