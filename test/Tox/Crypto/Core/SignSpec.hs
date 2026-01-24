module Tox.Crypto.Core.SignSpec (spec) where

import qualified Data.ByteString      as BS
import           Test.Hspec
import           Test.QuickCheck

import           Tox.Crypto.Core.Sign

spec :: Spec
spec = describe "sign" $
  it "makes signatures that verify only for the signed message" $ property $ \message ->
    forAll (vectorOf seedLength arbitrary) $ \seed -> case signKeyPairFromSeed (BS.pack seed) of
      Nothing -> expectationFailure "no key pair from a seed"
      Just keys ->
        let msg = BS.pack message
            signature = sign keys msg
        in (verify (signPublicKey keys) signature msg, verify (signPublicKey keys) signature (BS.cons 0 msg))
             `shouldBe` (True, False)
