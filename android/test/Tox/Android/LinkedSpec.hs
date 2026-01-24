{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.LinkedSpec (spec) where

import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Text
import           Data.Time          (UTCTime)
import           Test.Hspec

import qualified Tox.Android.Linked as Linked
import           Tox.Android.Model
import           Tox.Android.Sample (at)
import qualified Tox.Sync.Event     as Sync
import qualified Tox.Sync.Fact      as Sync
import qualified Tox.Sync.Frame     as Frame
import qualified Tox.Sync.Id        as Sync
import qualified Tox.Sync.Protocol  as Sync
import qualified Tox.Sync.Store     as Sync

homeKey, device, bobKey, tabletKey :: Sync.PublicKey
homeKey = Sync.PublicKey (BS.replicate 32 0xaa)
device = Sync.PublicKey (BS.replicate 32 0xdd)
bobKey = Sync.PublicKey (BS.replicate 32 0x11)
tabletKey = Sync.PublicKey (BS.replicate 32 0x22)

home, bob :: ContactId
home = ContactId (Text.replicate 32 "AA")
bob = ContactId (Text.replicate 32 "11")

homeAddress :: Text.Text
homeAddress = Text.replicate 32 "AA" <> "000000000000"

pairing :: Text.Text
pairing = "pair:00112233445566778899aabbccddeeff"

-- | Linked an hour before the home's events.
linked :: Model
linked = initial
  { modelLinked = Just (Linked.newLinked homeAddress (Just pairing) (at 5 9 0) device Sync.emptyStore)
  , modelShown = True }

account, bobChat :: Sync.GraphId
account = Sync.accountGraph homeKey
bobChat = Sync.friendGraph homeKey bobKey

-- | What the home wrote: Bob as a friend, by name, and his message.
homeEvents :: [BS.ByteString]
homeEvents = snd (foldl write (Sync.emptyStore, []) contents)
  where
    contents =
      [ (account, at 5 10 0, Sync.WantFriend bobKey Nothing Nothing)
      , (account, at 5 10 0, Sync.FactContent (Sync.FriendSubject bobKey) (Sync.Name "Bob"))
      , (bobChat, at 5 10 1, Sync.Received Nothing "Bob" "hi" Sync.Normal)
      -- Another device of the account, which the home has as a friend.
      , (account, at 5 10 2, Sync.WantFriend tabletKey Nothing Nothing)
      , (account, at 5 10 2, Sync.Device tabletKey "Tablet") ]
    write (store, written) (g, time, content) =
      let (_, bytes, store') = Sync.create g homeKey (Linked.toMillis time) content store
      in (store', written ++ [bytes])

-- | The home's message, as the packets that carry it.
fromHome :: Sync.SessionMessage -> [Event]
fromHome = map (SyncReceived home) . Frame.frames . Sync.encodeMessage

-- | The model after events, with every command they gave.
run :: [Event] -> Model -> (Model, [Command])
run events model = foldl (\(m, cs) e -> let (m', cs') = update e m in (m', cs ++ cs')) (model, []) events

-- | The session messages a device sent the home.
toHome :: [Command] -> [Sync.SessionMessage]
toHome commands = concat [messages packets | SendSync to packets <- commands, to == home]
  where
    messages = go Frame.reassembler
    go _ [] = []
    go r (p : ps) = case Frame.push p r of
      Right (Just bytes, r') | Right (Just m) <- Sync.decodeMessage bytes -> m : go r' ps
      Right (_, r') -> go r' ps
      Left (_, r') -> go r' ps

-- | The content of the events sent to the home.
sent :: [Command] -> [Sync.Content]
sent commands = [Sync.eventContent e | Sync.Events es <- toHome commands, Right e <- map Sync.decodeEvent es]

synced :: Model
synced = fst (run (fromHome Sync.hello ++ fromHome (Sync.Events homeEvents)) linked)

time' :: UTCTime
time' = at 5 11 0

spec :: Spec
spec = do
  it "pairs with the home by friend request, and greets it when it comes online" $ do
    snd (update (Started "") linked) `shouldBe` [RequestFriend homeAddress pairing]
    toHome (snd (update (FriendOnline home True) linked)) `shouldBe` [Sync.hello]

  it "shows the account's friends and messages from the home's events, and none of its devices" $ do
    let (model, commands) = run (fromHome Sync.hello) linked
    [() | Sync.Summary _ <- toHome commands] `shouldBe` [()]
    snd (update (Started "") model) `shouldBe` [Befriend home]
    Map.keys (modelContacts synced) `shouldBe` [bob]
    (\c -> (contactName c, map messageText (contactMessages c), contactUnread c)) <$> Map.lookup bob (modelContacts synced)
      `shouldBe` Just (Just "Bob", ["hi"], 1)

  it "tells the other devices what the user read and sent" $ do
    let (opened, commands) = update (Open (Chat bob)) synced
    [rank | Sync.ReadUpTo g _ rank <- sent commands, g == bobChat] `shouldBe` [0]
    contactUnread <$> Map.lookup bob (modelContacts opened) `shouldBe` Just 0
    let (written, sending) = run [DraftChanged "hello", Send time'] opened
    [text | Sync.Send _ text Sync.Normal <- sent sending] `shouldBe` ["hello"]
    map messageDelivery . contactMessages <$> Map.lookup bob (modelContacts written) `shouldBe` Just [Delivered, Pending]

  it "asks the home to found and join chats" $ do
    let founding = [Open NewGroupScreen, FoundingChosen PrivateGroup, GroupNameEdited " Lab "]
    [c | c@Sync.NewChat {} <- sent (snd (run (founding ++ [Found]) synced))]
      `shouldBe` [Sync.NewChat Sync.GroupKind "Lab" Sync.Private]
    let chat = BS.replicate 32 0x33
        joining = [Open JoinGroupScreen, ChatIdEdited (Text.replicate 32 "33"), PasswordChanged "pw"]
    [c | c@Sync.JoinChat {} <- sent (snd (run (joining ++ [JoinPublic]) synced))]
      `shouldBe` [Sync.JoinChat chat (Just "pw")]
