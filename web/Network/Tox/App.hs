{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Tox.App (app) where

import           Control.Monad             ((>=>))
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Binary               as Binary
import           Data.ByteString.Lazy      (ByteString)
import           Data.Maybe                (fromMaybe)
import qualified Data.MessagePack          as MessagePack
import           Servant
import           Test.QuickCheck.Arbitrary (arbitrary)
import qualified Test.QuickCheck.Gen       as Gen
import           Text.Groom                (groom)
import           Text.Read                 (readEither)

import           Network.Tox.SaveData      (SaveData)


instance MimeRender OctetStream SaveData where
    mimeRender p = mimeRender p . Binary.encode

instance MimeUnrender OctetStream SaveData where
    mimeUnrender _ bytes =
        case Binary.decodeOrFail bytes of
          Left (_, _, err) -> Left err
          Right (_, _, ok) -> Right ok

instance MimeRender PlainText SaveData where
    mimeRender p = mimeRender p . groom

instance MimeUnrender PlainText SaveData where
    mimeUnrender p = mimeUnrender p >=> readEither


-- API specification
type ToxcoreApi =
       -- Link to the source code repository, to comply with AGPL.
       "source" :> Get '[PlainText] String
       -- Get a random binary savedata.
  :<|> "random" :> QueryParam "size" Int :> Get '[OctetStream] SaveData
       -- Parse a binary string as savedata and output in human-readable form.
  :<|> "parse" :> ReqBody '[OctetStream] SaveData :> Post '[PlainText] SaveData
       -- Serialise a human readable savedata back to binary.
  :<|> "to-save" :> ReqBody '[PlainText] SaveData :> Post '[OctetStream] SaveData
       -- Serialise a human readable savedata to msgpack.
  :<|> "to-msgpack" :> ReqBody '[PlainText] SaveData :> Post '[OctetStream] ByteString

toxcoreApi :: Proxy ToxcoreApi
toxcoreApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server ToxcoreApi
server =
         sourceH
    :<|> randomH
    :<|> parseH
    :<|> toSaveH
    :<|> toMsgpackH
  where
    sourceH = return "https://github.com/TokTok/hs-toxcore\n"

    randomH size = liftIO $ Gen.generate $ Gen.resize (fromMaybe 30 size) arbitrary
    parseH = return
    toSaveH = return
    toMsgpackH = return . MessagePack.pack

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: Application
app = serve toxcoreApi server
