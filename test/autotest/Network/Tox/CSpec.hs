{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module Network.Tox.CSpec where

import           Control.Applicative    ((<$>))
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (replicateM_, when)
import qualified Crypto.Saltine.Class   as Sodium (encode)
import qualified Data.ByteString        as BS
import           Foreign.Marshal.Alloc  (alloca)
import           Foreign.Marshal.Utils  (with)
import           Foreign.Ptr            (freeHaskellFunPtr, nullPtr)
import           Foreign.Storable       (Storable (..))
import           Test.Hspec

import qualified Network.Tox.C          as C
import qualified Network.Tox.Crypto.Key as Key


bootstrapKey :: Key.PublicKey
bootstrapKey = read "\"F404ABAA1C99A9D37D61AB54898F56793E1DEF8BD46B1038B9D822E8460FAB67\""

bootstrapHost :: BS.ByteString
bootstrapHost = "biribiri.org"


while :: IO Bool -> IO () -> IO ()
while cond io = do
  continue <- cond
  when continue $ io >> while cond io


spec :: Spec
spec =
  describe "tox_iterate" $
    it "works" $ do
      tox <- alloca $ \err -> do
        --options <- C.tox_options_new
        let options = nullPtr
        tox <- C.tox_new options err
        peek err >>= print
        --C.tox_options_free options
        return tox

      BS.useAsCString bootstrapHost $ \host ->
        BS.useAsCString (Sodium.encode bootstrapKey) $ \key ->
          alloca $ \err -> do
            C.tox_bootstrap tox host 33445 key err
            peek err >>= print

      cb <- C.wrapSelfConnectionStatusCb $ \_ conn ud -> do
        print conn
        poke ud 4321
      C.tox_callback_self_connection_status tox cb

      with (1234 :: Int) $ \userData -> while ((/= 4321) <$> peek userData) $ do
        putStrLn "yo"
        C.tox_iterate tox userData
        C.tox_iteration_interval tox >>= threadDelay . (* 10000) . fromIntegral

      C.tox_kill tox
      freeHaskellFunPtr cb
