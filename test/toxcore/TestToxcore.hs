{-# LANGUAGE CPP            #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           Control.Monad   (when)
import           Data.Bits       (shift, (.&.))
import           Data.Word       (Word16, Word32)
import           Foreign.C.Error (Errno (..), eINTR)
import           Foreign.C.Types (CInt)

#include "errors.h"

foreign import ccall "test_main" c_test_main :: Bool -> Bool -> Word16 -> IO Word32


data Result = Result
  { line  :: Int
  , code  :: Int
  , errno :: CInt
  }

testMain :: Bool -> Bool -> Int -> IO Result
testMain debug collectSamples port = do
  result <- c_test_main debug collectSamples (fromIntegral port)
  let line  = fromIntegral $ shift result (-16)
  let errno = fromIntegral $ shift result (- 8) .&. 0xff
  let code  = fromIntegral $       result       .&. 0xff
  return $ Result line code errno


errorDesc :: Int -> String
errorDesc = \case
  E_OK      -> "Success"
  E_NOMEM   -> "Error: Out of memory"
  E_SOCKET  -> "Error: socket creation failed"
  E_BIND    -> "Error: bind failed"
  E_LISTEN  -> "Error: listen failed"
  E_ACCEPT  -> "Error: accept failed"
  E_PARSE   -> "Error: unable to parse msgpack input"
  E_OPEN    -> "Error: open failed"
  E_READ    -> "Error: read failed"
  E_WRITE   -> "Error: write failed"
  E_SODIUM  -> "Error: libsodium initialisation failed"
  e         -> "Unknown error code: " ++ show e


main :: IO ()
main = do
  Result { line, code, errno } <- testMain True True 1234
  let Errno eintr = eINTR
  when (code /= E_OK && errno /= eintr) $
    putStrLn $ errorDesc code ++ ", errno=" ++ show errno ++ ", line=" ++ show line
