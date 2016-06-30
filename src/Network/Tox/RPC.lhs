\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
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
  , runClient
  , runServer
  , jsonToObject
  , jsonFromObject
  ) where

import           Control.Monad              ((>=>))
import           Control.Monad.IO.Class     (liftIO)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson
import qualified Data.MessagePack           as MessagePack
import           Data.MessagePack.Aeson     (fromAeson, toAeson)
import           Network.MessagePack.Client (Client)
import qualified Network.MessagePack.Client as Client
import           Network.MessagePack.Server (Server)
import qualified Network.MessagePack.Server as Server


--stubs :: forall t f (m :: * -> *). (Server.MethodType m f, Client.RpcType t)
--      => String -> f -> (t, Server.Method m)
stubs name fun method = (Client.call name, Server.method name (fun method))


port :: Int
port = 1234


runClient :: Client a -> IO ()
runClient = Client.execClient "localhost" 1234


runServer :: [Server.Method IO] -> IO ()
runServer = Server.serve port


jsonToObject :: Aeson.ToJSON a => a -> MessagePack.Object
jsonToObject = fromAeson . Aeson.toJSON


jsonFromObject :: Aeson.FromJSON a => MessagePack.Object -> Maybe a
jsonFromObject = toAeson >=> Aeson.parseMaybe Aeson.parseJSON


fun1 :: (a -> result) -> a -> Server result
fun1 f a = return $ f a

fun2 :: (a -> b -> result) -> a -> b -> Server result
fun2 f a b = return $ f a b

fun3 :: (a -> b -> c -> result) -> a -> b -> c -> Server result
fun3 f a b c = return $ f a b c


ioFun0 :: IO result -> Server result
ioFun0 = liftIO

\end{code}
