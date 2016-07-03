{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Control.Monad (when)
import           Data.Bits     (shift, (.&.))

#include "errors.h"

foreign import ccall "test_main" c_test_main :: Bool -> Int -> IO Int

testMain :: Bool -> Int -> IO (Int, Int)
testMain collectSamples port = do
  result <- c_test_main collectSamples port
  return (result .&. 0xff, shift result (-8))


errorDesc :: Int -> String
errorDesc = \case
  E_OK      -> "Success"
  E_NOMEM   -> "Error: Out of memory"
  E_BIND    -> "Error: bind failed"
  E_LISTEN  -> "Error: listen failed"
  E_ACCEPT  -> "Error: accept failed"
  E_PARSE   -> "Error: unable to parse msgpack input"
  E_READ    -> "Error: read failed"
  E_WRITE   -> "Error: write failed"
  e         -> "Unknown error code: " ++ show e


main :: IO ()
main = do
  (result, errno) <- testMain True 1234
  when (result /= E_OK) $
    putStrLn $ errorDesc result ++ ", errno=" ++ show errno
