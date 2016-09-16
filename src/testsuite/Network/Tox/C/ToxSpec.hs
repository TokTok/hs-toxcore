{-# LANGUAGE Trustworthy #-}
module Network.Tox.C.ToxSpec where

import qualified Crypto.Saltine.Core.Box as Sodium
import           Data.Default.Class      (def)
import           Data.Proxy              (Proxy (..))
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.C           as C
import qualified Network.Tox.Crypto.Key  as Key


spec :: Spec
spec = do
  describe "tox_version_is_compatible" $
    it "is compatible with the major/minor/patch of the linked library" $
      C.tox_version_is_compatible
        C.tox_version_major
        C.tox_version_minor
        C.tox_version_patch
      `shouldBe` True

  describe "key size" $
    it "is equal to the hstox expected key size" $ do
      fromIntegral C.tox_public_key_size `shouldBe` Key.encodedByteSize (Proxy :: Proxy Sodium.PublicKey)
      fromIntegral C.tox_secret_key_size `shouldBe` Key.encodedByteSize (Proxy :: Proxy Sodium.SecretKey)
      C.tox_address_size `shouldBe` C.tox_public_key_size + 6
      C.tox_max_name_length `shouldBe` 128
      C.tox_max_status_message_length `shouldBe` 1007
      C.tox_max_friend_request_length `shouldBe` 1016
      C.tox_max_message_length `shouldBe` C.tox_max_custom_packet_size - 1
      C.tox_max_custom_packet_size `shouldBe` 1373
      C.tox_max_filename_length `shouldBe` 255
      C.tox_hash_length `shouldBe` C.tox_file_id_length

  describe "Options" $ do
    it "can be marshalled to C and back" $
      property $ \options -> do
        res <- C.withOptions options C.peekToxOptions
        res `shouldBe` Right options

    it "is saved correctly by pokeToxOptions" $
      property $ \options0 options1 -> do
        res <- C.withOptions options0 $ \ptr -> do
          C.pokeToxOptions options1 ptr (return ())
          C.peekToxOptions ptr
        res `shouldBe` Right options0

    describe "def" $
      it "is equivalent to the C default options" $ do
        res <- C.withToxOptions C.peekToxOptions
        res `shouldBe` Right def
