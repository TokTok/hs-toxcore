{-# LANGUAGE OverloadedStrings #-}
module Tox.Application.ProxySpec (spec) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (bracket)
import           Control.Monad             (void)
import qualified Data.ByteString           as BS
import qualified Network.Socket            as Socket
import qualified Network.Socket.ByteString as SocketBS
import           Test.Hspec

import           Tox.Application.Proxy

relay :: Socket.SockAddr
relay = Socket.SockAddrInet 33445 (Socket.tupleToHostAddress (1, 2, 3, 4))

-- | A proxy on a local port that answers the handshake as given and then
-- echoes what comes.
proxying :: (Socket.Socket -> IO ()) -> (Socket.PortNumber -> IO a) -> IO a
proxying handshake use = bracket listening Socket.close $ \listener -> do
  port <- Socket.socketPort listener
  void . forkIO $ do
    (sock, _) <- Socket.accept listener
    handshake sock
    let echo = SocketBS.recv sock 4096 >>= \b -> if BS.null b then pure () else SocketBS.sendAll sock b >> echo
    echo
  use port
  where
    listening = do
      sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
      sock <$ Socket.listen sock 1

through :: ProxyType -> Socket.PortNumber -> IO BS.ByteString
through kind port = bracket (connectVia (Just (Proxy kind "127.0.0.1" (fromIntegral port))) relay) Socket.close $ \sock ->
  SocketBS.sendAll sock "ping" >> SocketBS.recv sock 4

spec :: Spec
spec = do
  it "asks for the relay's address as toxcore does" $ do
    socks5Request relay `shouldBe` BS.pack [5, 1, 0, 1, 1, 2, 3, 4, 0x82, 0xa5]
    httpRequest relay `shouldBe` "CONNECT 1.2.3.4:33445 HTTP/1.1\r\nHost: 1.2.3.4:33445\r\n\r\n"
    socks5ReplyLength (BS.pack [5, 0, 0, 1, 0]) `shouldBe` Just 5
    socks5ReplyLength (BS.pack [5, 1, 0, 1, 0]) `shouldBe` Nothing
    httpAccepted "HTTP/1.1 200 Connection established\r\n\r\n" `shouldBe` True
    httpAccepted "HTTP/1.1 403 Forbidden\r\n\r\n" `shouldBe` False

  it "reaches a relay through a SOCKS5 proxy" $
    proxying (\sock -> do
        _ <- SocketBS.recv sock 3
        SocketBS.sendAll sock (BS.pack [5, 0])
        request <- SocketBS.recv sock 10
        request `shouldBe` socks5Request relay
        SocketBS.sendAll sock (BS.pack [5, 0, 0, 1, 0, 0, 0, 0, 0, 0]))
      (through Socks5Proxy) `shouldReturn` "ping"

  it "reaches a relay through an HTTP proxy" $
    proxying (\sock -> do
        _ <- SocketBS.recv sock (BS.length (httpRequest relay))
        SocketBS.sendAll sock "HTTP/1.1 200 OK\r\n\r\n")
      (through HttpProxy) `shouldReturn` "ping"

  it "fails when the proxy refuses" $
    proxying (\sock -> SocketBS.recv sock 3 >> SocketBS.sendAll sock (BS.pack [5, 0xff]))
      (through Socks5Proxy) `shouldThrow` anyIOException
