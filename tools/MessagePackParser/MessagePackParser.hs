{-# LANGUAGE Safe #-}
---------------------------
--
-- A MessagePack parser.
--
-- Example usage:
--   $ echo -ne "\x94\x01\xa1\x32\xa1\x33\xa4\x50\x6f\x6f\x66" | ./msgpack-parser
-- or
--   $ echo 'ObjectArray [ObjectInt 97, ObjectStr "test",  ObjectBool True]' | ./msgpack-parser
--
-- This tool performs two symmetrical functions:
--   1. It can decode binary data representing a
--      Data.MessagePack.Object into a human-readable string.
--   2. It can do the reverse: encode a human-readable string into
--      a binary representation of Data.MessagePack.Object.
--
-- No flags are required as it automatically detects which of these
-- two functions it should perform.  This is done by first assuming
-- the input is human readable.  If it fails to parse it, it then
-- considers it as binary data.
--
-- Therefore, given a valid input, the tool has the following property
--   $ cat input.bin | ./msgpack-parser | ./msgpack-parser
-- will output back the contents of input.bin.
--
-- In case the input is impossible to parse, nothing is output.
--
-- Known bugs:
--   - If no input is given, the tool exits with
--     "Data.Binary.Get.runGet at position 0: not enough bytes"
--   - The tool does not check that all the input is parsed.
--     Therefore, "abc" is interpreted as just "ObjectInt 97".
--
module Main where

import           Data.MessagePack           (pack, unpack, Object)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Text.Read                  (readMaybe)

tryReadAsObject :: L.ByteString -> Maybe Object
tryReadAsObject = readMaybe . L8.unpack

tryUnpackAsObject :: L.ByteString -> Maybe Object
tryUnpackAsObject = unpack

parse :: L.ByteString -> L.ByteString
parse bstr = case tryReadAsObject bstr of
  Just o -> pack o
  Nothing -> case tryUnpackAsObject bstr of
    Just o -> L8.pack ((show o) ++ "\n")
    Nothing -> L.empty

main :: IO ()
main = do
  bytestring <- L8.getContents
  L.putStr (parse bytestring)
