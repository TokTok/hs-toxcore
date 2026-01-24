{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Reaching TCP relays through a proxy, as toxcore does: HTTP CONNECT or
-- SOCKS5 without authentication, to the relay's address.  Only TCP goes
-- through it.  A client that wants all of its traffic there also turns UDP
-- off.
module Tox.Application.Proxy
  ( Proxy (..)
  , ProxyType (..)
  , connectVia
  , socks5Request
  , socks5ReplyLength
  , httpRequest
  , httpAccepted
  ) where

import           Control.Exception         (IOException, bracketOnError,
                                            throwIO)
import           Control.Monad             (unless)
import           Data.Bits                 (shiftR)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as Char8
import           Data.Word                 (Word16, Word8)
import qualified Network.Socket            as Socket
import qualified Network.Socket.ByteString as SocketBS
import           Numeric                   (showHex)

data ProxyType = HttpProxy | Socks5Proxy
  deriving (Eq, Show)

data Proxy = Proxy
  { proxyType :: ProxyType
  , proxyHost :: String
  , proxyPort :: Word16
  } deriving (Eq, Show)

-- | A stream socket to the address, through the proxy if there is one.
connectVia :: Maybe Proxy -> Socket.SockAddr -> IO Socket.Socket
connectVia Nothing to = connected to
connectVia (Just proxy) to = do
  info : _ <- Socket.getAddrInfo
    (Just Socket.defaultHints { Socket.addrSocketType = Socket.Stream })
    (Just (proxyHost proxy)) (Just (show (proxyPort proxy)))
  bracketOnError (connected (Socket.addrAddress info)) Socket.close $ \sock -> do
    case proxyType proxy of
      Socks5Proxy -> do
        SocketBS.sendAll sock (BS.pack [5, 1, 0])
        method <- receive sock 2
        unless (method == BS.pack [5, 0]) $ refused "SOCKS5 proxy wants authentication"
        SocketBS.sendAll sock (socks5Request to)
        reply <- receive sock 5
        case socks5ReplyLength reply of
          Just rest -> () <$ receive sock rest
          Nothing   -> refused "SOCKS5 proxy refused the connection"
      HttpProxy -> do
        SocketBS.sendAll sock (httpRequest to)
        header <- untilBlankLine sock BS.empty
        unless (httpAccepted header) $ refused "HTTP proxy refused the connection"
    pure sock
  where
    refused = throwIO . userError

connected :: Socket.SockAddr -> IO Socket.Socket
connected to = bracketOnError (Socket.socket family Socket.Stream Socket.defaultProtocol) Socket.close $ \sock ->
  sock <$ Socket.connect sock to
  where
    family = case to of
      Socket.SockAddrInet6 {} -> Socket.AF_INET6
      _                       -> Socket.AF_INET

-- | Exactly so many bytes, or an error if the connection ends first.
receive :: Socket.Socket -> Int -> IO BS.ByteString
receive sock = go BS.empty
  where
    go got n
      | n <= 0 = pure got
      | otherwise = SocketBS.recv sock n >>= \case
          chunk | BS.null chunk -> throwIO (userError "Proxy closed the connection" :: IOException)
                | otherwise -> go (got <> chunk) (n - BS.length chunk)

-- | The reply's header, a byte at a time, so nothing of what follows is
-- read: at most 4 KiB.
untilBlankLine :: Socket.Socket -> BS.ByteString -> IO BS.ByteString
untilBlankLine sock got
  | "\r\n\r\n" `BS.isSuffixOf` got = pure got
  | BS.length got > 4096 = throwIO (userError "HTTP proxy reply too long")
  | otherwise = receive sock 1 >>= untilBlankLine sock . (got <>)

-- | CONNECT to an IPv4 or IPv6 address.
socks5Request :: Socket.SockAddr -> BS.ByteString
socks5Request to = BS.pack ([5, 1, 0] ++ address ++ port16 (portOf to))
  where
    address = case to of
      Socket.SockAddrInet _ host -> 1 : tuple4 (Socket.hostAddressToTuple host)
      Socket.SockAddrInet6 _ _ host _ ->
        let (a, b, c, d, e, f, g, h) = Socket.hostAddress6ToTuple host
        in 4 : concatMap port16 [a, b, c, d, e, f, g, h]
      _ -> []
    tuple4 (a, b, c, d) = [a, b, c, d]

-- | How many bytes of a successful reply follow its first five, which tell
-- the kind of address, or Nothing if the proxy refused.
socks5ReplyLength :: BS.ByteString -> Maybe Int
socks5ReplyLength reply = case BS.unpack reply of
  [5, 0, _, 1, _] -> Just (4 + 2 - 1)
  [5, 0, _, 4, _] -> Just (16 + 2 - 1)
  [5, 0, _, 3, n] -> Just (fromIntegral n + 2)
  _               -> Nothing

httpRequest :: Socket.SockAddr -> BS.ByteString
httpRequest to = "CONNECT " <> target <> " HTTP/1.1\r\nHost: " <> target <> "\r\n\r\n"
  where
    target = Char8.pack (host <> ":" <> show (portOf to))
    host = case to of
      Socket.SockAddrInet _ a -> let (b, c, d, e) = Socket.hostAddressToTuple a in
        show b <> "." <> show c <> "." <> show d <> "." <> show e
      Socket.SockAddrInet6 _ _ a _ -> let (b, c, d, e, f, g, h, i) = Socket.hostAddress6ToTuple a in
        "[" <> foldr1 (\x y -> x <> ":" <> y) [showHex x "" | x <- [b, c, d, e, f, g, h, i]] <> "]"
      _ -> ""

-- | Whether the proxy's status line says 200.
httpAccepted :: BS.ByteString -> Bool
httpAccepted header = case Char8.words (Char8.takeWhile (/= '\r') header) of
  version : "200" : _ -> "HTTP/1." `BS.isPrefixOf` version
  _                   -> False

portOf :: Socket.SockAddr -> Word16
portOf = \case
  Socket.SockAddrInet port _      -> fromIntegral port
  Socket.SockAddrInet6 port _ _ _ -> fromIntegral port
  _                               -> 0

port16 :: Word16 -> [Word8]
port16 p = [fromIntegral (p `shiftR` 8), fromIntegral p]
