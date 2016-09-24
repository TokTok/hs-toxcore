\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
module Network.Tox.RPC
  ( MessagePack.MessagePack (..)
  , MessagePack.Object (..)
  , Client
  , Server
  , Client.call
  , Server.Method
  , Server.method
  , fun1
  , fun2
  , fun3
  , ioFun0
  , stubs
  , defaultPort
  , runClient
  , runServer
  ) where

import           Control.Monad.IO.Class     (liftIO)

import qualified Data.MessagePack           as MessagePack
import           Network.MessagePack.Client (Client)
import qualified Network.MessagePack.Client as Client
import           Network.MessagePack.Server (Server)
import qualified Network.MessagePack.Server as Server


--stubs :: forall t f (m :: * -> *). (Server.MethodType m f, Client.RpcType t)
--      => String -> f -> (t, Server.Method m)
stubs name fun method = (Client.call name, Server.method name (fun method))


defaultPort :: Int
defaultPort = 1234


runClient :: Client a -> IO a
runClient = Client.execClient "localhost" defaultPort


runServer :: Int -> [Server.Method IO] -> IO ()
runServer = Server.serve


fun1 :: (a -> result) -> a -> Server result
fun1 f a = return $ f a

fun2 :: (a -> b -> result) -> a -> b -> Server result
fun2 f a b = return $ f a b

fun3 :: (a -> b -> c -> result) -> a -> b -> c -> Server result
fun3 f a b c = return $ f a b c


ioFun0 :: IO result -> Server result
ioFun0 = liftIO

\end{code}
