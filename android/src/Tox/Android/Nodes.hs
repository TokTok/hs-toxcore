{-# LANGUAGE OverloadedStrings #-}
-- | The nodes Tox joins the network through: a list fetched from
-- nodes.tox.chat, kept for the next start, and some built in
-- for before there is one.
module Tox.Android.Nodes
  ( Node (..)
  , builtIn
  , nodesUrl
  , readNodes
  , usable
  ) where

import           Control.Monad          (guard)
import           Data.Bifunctor         (bimap)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import           Data.Char              (chr, isDigit, isHexDigit, isSpace)
import           Data.Either            (fromRight)
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Word              (Word16)
import           Numeric                (readHex)

data Node = Node
  { nodeHost    :: String
  , nodePort    :: Word16
  , nodeTcpPort :: Maybe Word16
    -- ^ Where it relays over TCP, if it does.
  , nodeKey     :: BS.ByteString
  , nodeUp      :: Bool
    -- ^ Whether the list's last check reached it over TCP.
  } deriving (Eq, Show)

nodesUrl :: Text
nodesUrl = "https://nodes.tox.chat/json"

-- | Nodes from nodes.tox.chat that relay over TCP.
builtIn :: [Node]
builtIn =
  [ Node host port (Just port) (fromRight BS.empty (Base16.decode key)) True
  | (host, port, key) <-
      [ ("144.217.167.73", 33445, "7E5668E0EE09E19F320AD47902419331FFEE147BB3606769CFBE921A2A2FD34C")
      , ("49.12.229.145", 33445, "B3E5FA80DC8EBD1149AD2AB35ED8B85BD546DEDE261CA593234C619249419506")
      , ("49.13.50.193", 33445, "70EA214FDE161E7432530605213F18F7427DC773E276B3E317A07531F548545F")
      , ("172.105.109.31", 33445, "D46E97CF995DC1820B92B7D899E152A217D36ABE22730FEA4B6BF1BFC06C617C")
      , ("144.172.88.203", 33445, "2016A0F2797EE3A8B004BA623F11AAFC8146F1B8F45107232A1A1AECCE856674")
      , ("172.104.215.182", 33445, "DA2BD927E01CD05EBCC2574EBE5BEBB10FF59AE0B2105A7D1E2B40E49BB20239")
      , ("114.35.245.150", 33445, "3F0A45A268367C1BEA652F258C85F4A66DA76BCAA667A49E770BCC4917AB6A25")
      , ("51.79.254.78", 33445, "F4FC9398B7167668ED2BCF85634E04D4CDCDD2F95DA5F305BD234888B6E6A771")
      , ("188.214.122.30", 33445, "2A9F7A620581D5D1B09B004624559211C5ED3D1D712E8066ACDB0896A7335705")
      , ("165.227.194.41", 443, "5D57B95EE4A7F37BA031DAD0CBD9510A9C96FFE09C1CE24A9C33746F39817D6E")
      , ("141.95.53.16", 33445, "DCD342A0D5E2AA8E35C2BD2C7988F906EEB631B35100170A7F30E77D7F596442")
      , ("86.107.187.54", 33445, "2C0F90965134C7BEFAFE72B077A19221628D7045BB51C1165A2C75CDB2B32634")
      , ("119.59.101.63", 33445, "197F746696062FA3BD07BB3BC0656ABD6692B4DAA27DACF0F474754F2B09B060")
      , ("167.17.40.142", 33445, "E84453123B44A47120FFB469CBCDEEF078D3785D7AD7F6C5B2351CB5DDE2C54C")
      , ("172.86.77.39", 33445, "AFFD3FAD3460E62A894E439534B27E5A5DCFE379C1C0FB78DEF1B150A87E900F")
      , ("145.239.1.105", 33445, "1658A9A64046C20F48FB2A47E56045233AB0AC0706974FC5904F9E74F452D908")
      , ("5.19.249.240", 38296, "DA98A4C0CD7473A133E115FEA2EBDAEEA2EF4F79FD69325FC070DA4DE4BA3238")
      ] ]

-- | The nodes of a list as nodes.tox.chat serves it, if it reads and has
-- any: one without, as it serves when its scan finds none, never takes the
-- place of one with.
readNodes :: BS.ByteString -> Maybe [Node]
readNodes bytes = do
  (Object top, rest) <- value (Char8.dropWhile isSpace bytes)
  guard (Char8.all isSpace rest)
  Array entries <- lookup "nodes" top
  let nodes = mapMaybe node entries
  guard (not (null nodes))
  pure nodes
  where
    node (Object fields) = do
      String host <- lookup "ipv4" fields
      guard (host `notElem` ["", "-", "NONE"])
      port <- portOf =<< lookup "port" fields
      String key <- lookup "public_key" fields
      Right raw <- Just (Base16.decode (Text.encodeUtf8 key))
      guard (BS.length raw == 32)
      let tcp = case lookup "tcp_ports" fields of
            Just (Array (p : _)) -> portOf p
            _                    -> Nothing
      pure (Node (Text.unpack host) port tcp raw (lookup "status_tcp" fields /= Just (Bool False)))
    node _ = Nothing
    portOf (Number n) | n > 0 && n < 65536 && n == fromIntegral (round n :: Int) = Just (round n)
    portOf _ = Nothing

-- | Those Tox can start from, those the list last reached first.
usable :: [Node] -> [Node]
usable nodes = filter nodeUp relaying ++ filter (not . nodeUp) relaying
  where relaying = [n | n <- nodes, Just _ <- [nodeTcpPort n]]

-- | As much of JSON as the list needs.
data Json
  = Object [(Text, Json)]
  | Array [Json]
  | String Text
  | Number Double
  | Bool Bool
  | Null
  deriving (Eq, Show)

-- | A value and what follows it, without the spaces between.
value :: BS.ByteString -> Maybe (Json, BS.ByteString)
value input = case Char8.uncons input of
  Just ('{', rest) -> members [] (skip rest)
  Just ('[', rest) -> items [] (skip rest)
  Just ('"', rest) -> bimap String skip <$> string [] rest
  Just ('t', _) | Just rest <- BS.stripPrefix "true" input -> Just (Bool True, skip rest)
  Just ('f', _) | Just rest <- BS.stripPrefix "false" input -> Just (Bool False, skip rest)
  Just ('n', _) | Just rest <- BS.stripPrefix "null" input -> Just (Null, skip rest)
  Just (c, _) | c == '-' || isDigit c ->
    let (digits, rest) = Char8.span (\d -> isDigit d || d `elem` ("+-.eE" :: String)) input
    in case reads (Char8.unpack (fixed digits)) of
      [(n, "")] -> Just (Number n, skip rest)
      _         -> Nothing
  _ -> Nothing
  where
    skip = Char8.dropWhile isSpace
    -- Haskell reads neither "1." nor ".5" nor "1e5" with a plus.
    fixed = Char8.filter (/= '+')
    members acc s = case Char8.uncons s of
      Just ('}', rest) | null acc -> Just (Object [], skip rest)
      Just ('"', rest) -> do
        (key, afterKey) <- string [] rest
        (':', afterColon) <- Char8.uncons (skip afterKey)
        (v, afterValue) <- value (skip afterColon)
        let acc' = (key, v) : acc
        case Char8.uncons afterValue of
          Just (',', more) -> members acc' (skip more)
          Just ('}', more) -> Just (Object (reverse acc'), skip more)
          _                -> Nothing
      _ -> Nothing
    items acc s = case Char8.uncons s of
      Just (']', rest) | null acc -> Just (Array [], skip rest)
      _ -> do
        (v, afterValue) <- value s
        case Char8.uncons afterValue of
          Just (',', more) -> items (v : acc) (skip more)
          Just (']', more) -> Just (Array (reverse (v : acc)), skip more)
          _                -> Nothing
    string acc s = do
      let (plain, rest) = BS.break (\b -> b == 0x22 || b == 0x5c) s
          acc' = Text.decodeUtf8Lenient plain : acc
      (c, more) <- Char8.uncons rest
      if c == '"'
        then Just (Text.concat (reverse acc'), more)
        else do
          (e, after) <- Char8.uncons more
          case e of
            'u' -> do
              let (code, after') = BS.splitAt 4 after
              guard (BS.length code == 4 && Char8.all isHexDigit code)
              [(n, "")] <- Just (readHex (Char8.unpack code))
              -- Halves of a pair stand for nothing alone.
              let ch = if n >= 0xd800 && n < 0xe000 then '\xfffd' else chr n
              string (Text.singleton ch : acc') after'
            _ -> do
              ch <- lookup e [('"', '"'), ('\\', '\\'), ('/', '/'), ('b', '\b'), ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t')]
              string (Text.singleton ch : acc') after
