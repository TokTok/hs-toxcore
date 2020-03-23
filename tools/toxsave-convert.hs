module Main (main) where

import           Control.Applicative        ((<$>), (<|>))
import qualified Data.Binary                as Binary
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Maybe                 (fromMaybe)
import           Network.Tox.SaveData       (SaveData)
import           Text.Groom                 (groom)
import           Text.Read                  (readMaybe)


parse :: LBS.ByteString -> LBS.ByteString
parse str = fromMaybe LBS.empty $
    (Binary.encode <$> (readMaybe $ LBS8.unpack str :: Maybe SaveData))
    <|>
    (Just . LBS8.pack . (++ "\n") . groom $ (Binary.decode str :: SaveData))


main :: IO ()
main = parse <$> LBS.getContents >>= LBS.putStr
