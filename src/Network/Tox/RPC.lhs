\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
module Network.Tox.RPC
  ( Client
  , Server
  , Client.call
  , Server.Method
  , Server.method
  , fun2
  , fun3
  , stubs
  , runClient
  , runServer
  ) where

import           Control.Monad.IO.Class     (liftIO)

import           Network.MessagePack.Client (Client)
import qualified Network.MessagePack.Client as Client
import           Network.MessagePack.Server (Server)
import qualified Network.MessagePack.Server as Server


fun2 :: (Show a, Show b, Show result)
      => String -> (a -> b -> result) -> a -> b -> Server result
fun2 name f a b = do
  liftIO $ putStrLn $ name ++ " (" ++ show a ++ ") (" ++ show b ++ ")"
  let r = f a b
  liftIO $ putStrLn $ name ++ " (" ++ show a ++ ") (" ++ show b ++ ") = " ++ show r
  return r


fun3 :: (Show a, Show b, Show c, Show result)
      => String -> (a -> b -> c -> result) -> a -> b -> c -> Server result
fun3 name f a b c = do
  liftIO $ putStrLn $ name ++ " (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  let r = f a b c
  liftIO $ putStrLn $ name ++ " (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ") = " ++ show r
  return r


--stubs :: forall t f (m :: * -> *). (Server.MethodType m f, Client.RpcType t)
--      => String -> f -> (t, Server.Method m)
stubs name fun method = (Client.call name, Server.method name (fun name method))


port :: Int
port = 1234


runClient :: Client a -> IO ()
runClient = Client.execClient "localhost" 1234


runServer :: [Server.Method IO] -> IO ()
runServer = Server.serve port

\end{code}
