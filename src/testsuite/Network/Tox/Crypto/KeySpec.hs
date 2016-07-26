{-# LANGUAGE Trustworthy #-}
module Network.Tox.Crypto.KeySpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Crypto.Saltine.Class     as Sodium
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as ByteString
import           Data.Proxy               (Proxy (..))
import           Network.Tox.Crypto.Key   (Key (..))
import qualified Network.Tox.Crypto.Key   as Key
import           Network.Tox.EncodingSpec
import           Test.Result
import qualified Text.Read                as Read


readMaybe :: String -> Maybe Key.PublicKey
readMaybe = Read.readMaybe


decodeM :: Monad m => ByteString -> m Key.PublicKey
decodeM = Key.decode


keyToInteger :: String -> Integer
keyToInteger string =
  Key.keyToInteger (read string :: Key.PublicKey)


encodeDecodePublicKey :: Key.PublicKey -> Expectation
encodeDecodePublicKey key =
  Sodium.decode (Sodium.encode key) `shouldBe` Just key


spec :: Spec
spec = do
  -- PublicKey for both binary and human-readable.
  rpcSpec (Proxy :: Proxy Key.PublicKey)
  binarySpec (Proxy :: Proxy Key.PublicKey)
  readShowSpec (Proxy :: Proxy Key.PublicKey)

  -- Binary only for the others.
  binarySpec (Proxy :: Proxy Key.SecretKey)
  binarySpec (Proxy :: Proxy Key.CombinedKey)
  binarySpec (Proxy :: Proxy Key.Nonce)

  describe "IsEncoding" $
    it "decodes encoded public keys correctly" $
      property encodeDecodePublicKey

  describe "read" $ do
    it "decodes valid hex string to PublicKey" $
      let
        actual = readMaybe "\"0100000000000000000000000000000000000000000000000000000000000010\""
        Just expected = Sodium.decode $ ByteString.pack [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x10]
      in
      actual `shouldBe` (Just $ Key expected)

    it "decodes empty string to Nothing" $ do
      let actual = readMaybe ""
      actual `shouldBe` Nothing
      case decodeM ByteString.empty of
        TestFailure msg -> msg `shouldStartWith` "unable to decode"
        TestSuccess val -> expectationFailure $ "unexpected success: " ++ show val

    it "decodes valid hex string of wrong length to Nothing" $
      let actual = readMaybe "\"0110\"" in
      actual `shouldBe` Nothing

  describe "keyToInteger" $ do
    it "converts keys to Integer in big endian" $ do
      keyToInteger "\"fe00000000000000000000000000000000000000000000000000000000000000\""
        `shouldBe`  0xfe00000000000000000000000000000000000000000000000000000000000000
      keyToInteger "\"00000000000000000000000000000000000000000000000000000000000000fe\""
        `shouldBe`  0x00000000000000000000000000000000000000000000000000000000000000fe

    it "encodes all keys to positive Integers" $
      property $ \key ->
        Key.keyToInteger (key :: Key.PublicKey) `shouldSatisfy` (0 <=)
