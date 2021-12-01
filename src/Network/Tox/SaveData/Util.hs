{-# LANGUAGE StrictData #-}
module Network.Tox.SaveData.Util where

import           Control.Monad   (when)
import           Data.Binary     (Binary (get))
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import           Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import           Data.Word       (Word16, Word32)


-- | Consumes the entire stream and parses some Binary out of it in a loop.
getList :: (Binary a, Show a) => Get [a]
getList = go []
  where
    go xs = do
        isEmpty <- Get.isEmpty
        if isEmpty
            then return $ reverse xs
            else go =<< (: xs) <$> get

getSectionHeader :: Word16 -> Get (Int, Word16)
getSectionHeader sectionMagic = do
    len   <- Get.getWord32le
    ty    <- Get.getWord16le
    magic <- Get.getWord16le
    when (magic /= sectionMagic) $
        fail $ "wrong magic number for section: "
            ++ show magic ++ " != " ++ show sectionMagic

    return (fromIntegral len, ty)

putSectionHeader :: Word16 -> Word32 -> Word16 -> Put
putSectionHeader sectionMagic len ty = do
    Put.putWord32le len
    Put.putWord16le ty
    Put.putWord16le sectionMagic
