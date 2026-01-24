-- | Our account in toxcore's save format, a .tox file, which other Tox
-- clients import: the keys, the profile and the contacts as the Store has
-- them, offline friends' names included.
module Tox.Android.Export
  ( exportSave
  ) where

import qualified Crypto.Saltine.Class    as Sodium
import qualified Data.Binary             as Binary
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Lazy    as LBS
import           Data.Either             (fromRight)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Word               (byteSwap32)
import qualified Tox.Application.Save    as Save
import qualified Tox.Crypto.Core.KeyPair as KeyPair
import qualified Tox.Session.Friend      as Messenger

import           Tox.Android.Model
import           Tox.Android.Store       (Identity (..))

-- | The .tox file of who we are and whom we know, if our key reads.
exportSave :: Identity -> Model -> Maybe BS.ByteString
exportSave (Identity secret nospam) model = do
  sk <- Sodium.decode secret
  pure . LBS.toStrict . Binary.encode . Save.toSaveData $ Save.Profile
    { Save.profileKeys = KeyPair.fromSecretKey sk
      -- As Tox keeps it, the other way round from what it says.
    , Save.profileNospam = byteSwap32 nospam
    , Save.profileName = utf8 (profileNickname (modelProfile model))
    , Save.profileStatusMessage = utf8 (profileStatus (modelProfile model))
    , Save.profileStatus = Messenger.Online
    , Save.profileFriends =
        [ Save.SavedFriend
            { Save.sfPublicKey = pk
            , Save.sfRequest = request <$> contactRequest c
            , Save.sfName = maybe BS.empty utf8 (contactName c)
            , Save.sfStatusMessage = utf8 (contactStatusMessage c)
            , Save.sfStatus = Messenger.Online
            , Save.sfLastSeen = 0
            }
        | (ContactId key, c) <- Map.toList (modelContacts model)
        , Right bytes <- [Base16.decode (utf8 key)]
        , Just pk <- [Sodium.decode bytes] ]
    , Save.profileDhtNodes = []
    , Save.profileTcpRelays = []
    , Save.profilePathNodes = []
    , Save.profileConferences = []
    , Save.profileGroups = []
    }
  where
    utf8 = Text.encodeUtf8
    -- Our request to one not online yet: its nospam, from the Tox ID, and
    -- what it says.
    request (address, message) =
      (fromRight BS.empty (Base16.decode (utf8 (Text.take 8 (Text.drop 64 address)))), utf8 message)
