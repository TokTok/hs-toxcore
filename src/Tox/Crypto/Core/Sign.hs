{-# LANGUAGE StrictData #-}
-- | Ed25519 signatures, as libsodium's crypto_sign.
module Tox.Crypto.Core.Sign
    ( SignKeyPair
    , signPublicKey
    , signSecretKey
    , signPublicKeyLength
    , signatureLength
    , seedLength
    , secretKeyLength
    , sha256Length
    , newSignKeyPair
    , signKeyPair
    , signKeyPairFromSeed
    , sign
    , verify
    , encPublicKey
    , encSecretKey
    , sha256
    ) where

import           Control.Monad                    (guard, void)
import qualified Crypto.Saltine.Class             as Sodium
import qualified Crypto.Saltine.Core.Sign         as Saltine
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Internal         as BSI
import qualified Data.ByteString.Unsafe           as BSU
import           Data.Maybe                       (fromMaybe)
import           Data.Word                        (Word8)
import           Foreign.C.Types                  (CInt (..), CULLong (..))
import           Foreign.ForeignPtr               (withForeignPtr)
import           Foreign.Ptr                      (Ptr, castPtr)
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes (..))

-- | The public key and the 64-byte secret key, which holds the seed and the
-- public key.
data SignKeyPair = SignKeyPair
  { signPublicKey :: ByteString
  , signSecretKey :: ByteString
  , signKey       :: Saltine.SecretKey
  }

instance Eq SignKeyPair where
  a == b = signSecretKey a == signSecretKey b

instance Show SignKeyPair where
  show keys = "SignKeyPair " ++ show (signPublicKey keys)

signPublicKeyLength, signatureLength, seedLength, secretKeyLength, curveKeyLength, sha256Length :: Int
signPublicKeyLength = 32
signatureLength     = 64
seedLength          = 32
secretKeyLength     = 64
curveKeyLength      = 32
sha256Length        = 32

foreign import ccall unsafe "crypto_sign_seed_keypair"
  c_crypto_sign_seed_keypair :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "crypto_sign_ed25519_pk_to_curve25519"
  c_pk_to_curve25519 :: Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "crypto_sign_ed25519_sk_to_curve25519"
  c_sk_to_curve25519 :: Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "crypto_hash_sha256"
  c_sha256 :: Ptr Word8 -> Ptr Word8 -> CULLong -> IO CInt

-- | Convert an Ed25519 key with a libsodium function, if it takes the key.
convert :: Int -> (Ptr Word8 -> Ptr Word8 -> IO CInt) -> ByteString -> Maybe ByteString
convert size f key
  | BS.length key /= size = Nothing
  | otherwise = unsafeDupablePerformIO . BSU.unsafeUseAsCString key $ \k -> do
      out <- BSI.mallocByteString curveKeyLength
      ok <- withForeignPtr out $ \o -> (== 0) <$> f o (castPtr k)
      return $ if ok then Just (BSI.fromForeignPtr out 0 curveKeyLength) else Nothing

-- | The X25519 public key of an Ed25519 public key.
encPublicKey :: ByteString -> Maybe ByteString
encPublicKey = convert signPublicKeyLength c_pk_to_curve25519

-- | The X25519 secret key of an Ed25519 secret key.
encSecretKey :: ByteString -> Maybe ByteString
encSecretKey = convert secretKeyLength c_sk_to_curve25519

sha256 :: ByteString -> ByteString
sha256 bytes = unsafeDupablePerformIO . BSU.unsafeUseAsCStringLen bytes $ \(p, n) ->
  BSI.create sha256Length $ \o -> void $ c_sha256 o (castPtr p) (fromIntegral n)

newSignKeyPair :: MonadRandomBytes m => m SignKeyPair
newSignKeyPair = randomBytes seedLength >>= maybe newSignKeyPair return . signKeyPairFromSeed

-- | The key pair of a secret key, which ends in its public key.
signKeyPair :: ByteString -> Maybe SignKeyPair
signKeyPair sk = do
  guard $ BS.length sk == secretKeyLength
  SignKeyPair (BS.drop seedLength sk) sk <$> Sodium.decode sk

-- | The key pair libsodium makes from a seed.
signKeyPairFromSeed :: ByteString -> Maybe SignKeyPair
signKeyPairFromSeed seed
  | BS.length seed /= seedLength = Nothing
  | otherwise = signKeyPair . unsafeDupablePerformIO . BSU.unsafeUseAsCString seed $ \s -> do
      pk <- BSI.mallocByteString signPublicKeyLength
      BSI.create secretKeyLength $ \q ->
        withForeignPtr pk $ \p -> void $ c_crypto_sign_seed_keypair p q (castPtr s)

-- | A detached signature of a message.
sign :: SignKeyPair -> ByteString -> ByteString
sign keys = Sodium.encode . Saltine.signDetached (signKey keys)

-- | Whether a detached signature of a message is by the key.
verify :: ByteString -> ByteString -> ByteString -> Bool
verify pk signature message = fromMaybe False $ do
  key <- Sodium.decode pk
  sig <- Sodium.decode signature
  return $ Saltine.signVerifyDetached key sig message
