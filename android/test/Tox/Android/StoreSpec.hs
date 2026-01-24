{-# LANGUAGE OverloadedStrings #-}
module Tox.Android.StoreSpec (spec) where

import           Control.Monad          (foldM_, void, (<=<))
import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (listToMaybe)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Database.SQLite.Simple as SQLite
import           Test.Hspec

import qualified Tox.Application.Tox    as Tox

import           Tox.Android.Model
import           Tox.Android.Sample
import           Tox.Android.Store

-- | The store of the account online, with each of the events saved as it
-- happened.
saved :: [Event] -> IO Store
saved events = open ":memory:" >>= \database -> savedIn database events

savedIn :: Database -> [Event] -> IO Store
savedIn database events = do
  store <- profileStore database <$> activeProfile database
  foldM_ (\old event -> let new = fst (update event old) in new <$ save store old new)
    initial events
  pure store

spec :: Spec
spec = do
  it "keeps the profile, friends, requests and chats" $ do
    model <- load =<< saved history
    model `shouldBe` sample
      { modelProfile = (modelProfile sample) { profileToxId = "" }
      , modelProfileId = modelProfileId model
      , modelProfiles = [Account (modelProfileId model) "Tox User" ""]
      , modelConnection = Offline
      , modelJump = 0
      , modelContacts = (\c -> c { contactOnline = False }) <$> modelContacts sample
      }

  it "keeps our pending request and the rejected ones" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
        events = history ++ [Open AddContact, AddressChanged address, AddFriend, Reject carol]
    model <- load =<< saved events
    contactRequest <$> Map.lookup (ContactId (Text.take 64 address)) (modelContacts model)
      `shouldBe` Just (Just (address, "Hey, I'm Tox User, come chat with me on Tox!"))
    Map.keys (modelDeclined model) `shouldBe` [carol]

  it "keeps status messages, mutes, and chats kept of deleted contacts" $ do
    let events = history ++
          [ FriendStatusMessage alice "Busy day", MuteChat alice (Just (at 9 0 0))
          , Open (Chat bob), DraftChanged "hi", Send (at 3 9 0), DeleteContact bob True ]
    model <- load =<< saved events
    (contactStatusMessage <$> Map.lookup alice (modelContacts model)) `shouldBe` Just "Busy day"
    (contactMute <$> Map.lookup alice (modelContacts model)) `shouldBe` Just (Just (MutedUntil (at 9 0 0)))
    map messageText . contactMessages <$> Map.lookup bob (modelFormer model) `shouldBe` Just ["hi"]
    gone <- load =<< saved (events ++ [DeleteContact alice False])
    Map.member alice (modelContacts gone) || Map.member alice (modelFormer gone) `shouldBe` False

  it "keeps their messages as Tox carried them" $ do
    let bytes = "hi" <> BS.pack (0 : 0 : replicate 36 1)
    model <- load =<< saved (history ++ [Received alice (at 3 9 0) Tox.MessageAction bytes])
    [ (messageKind m, messageRaw m) | Just c <- [Map.lookup alice (modelContacts model)], m <- drop 3 (contactMessages c) ]
      `shouldBe` [(Tox.MessageAction, Just bytes)]

  it "keeps when our message was delivered" $ do
    model <- load =<< saved history
    (messageDelivered <=< listToMaybe . drop 1 . contactMessages <=< Map.lookup alice) (modelContacts model)
      `shouldBe` Just (at 1 15 16)

  it "keeps settings, new ones taking the default file size" $ do
    fresh <- load =<< saved history
    settingAutoAccept (modelAccount fresh) `shouldBe` 4 * 1024 * 1024
    let device = DeviceSettings False True False False (Just (Tox.Proxy Tox.Socks5Proxy "127.0.0.1" 9050))
    model <- load =<< saved (history ++ [SetAutoAccept 0, SetDevice device, ToggleFormat "message_v3"])
    (modelAccount model, modelDevice model) `shouldBe` (AccountSettings 0 (Set.fromList ["message_v3"]), device)

  it "keeps the pictures we and friends show" $ do
    model <- load =<< saved (history ++ [AvatarPicked "ours", FriendAvatar alice (Just "theirs")])
    ((`Map.lookup` modelAvatars model) =<< modelAvatar model) `shouldBe` Just "ours"
    ((`Map.lookup` modelAvatars model) =<< contactAvatar =<< Map.lookup alice (modelContacts model)) `shouldBe` Just "theirs"

  it "keeps drafts" $ do
    model <- load =<< saved (history ++ [Open (Chat bob), DraftChanged "later"])
    Map.lookup bob (modelDrafts model) `shouldBe` Just "later"

  it "keeps how far a message got" $ do
    let events = history ++ [Open (Chat bob), DraftChanged "hi", Send (at 3 9 0), Delivering bob 0 Sent]
    model <- load =<< saved events
    map messageDelivery . contactMessages <$> Map.lookup bob (modelContacts model)
      `shouldBe` Just [Sent]

  it "keeps how many parts of a message the friend has" $ do
    let events = history ++ [Open (Chat bob), DraftChanged (Text.replicate 3000 "a"), Send (at 3 9 0), Confirmed bob 0 2 (at 3 9 1)]
    model <- load =<< saved events
    map messageParts . contactMessages <$> Map.lookup bob (modelContacts model) `shouldBe` Just [2]

  it "brings a database of the first version up to date, keeping its chats" $ do
    db <- SQLite.open ":memory:"
    mapM_ (SQLite.execute_ db)
      [ "CREATE TABLE identity\
        \ (id INTEGER PRIMARY KEY CHECK (id = 0), secret_key BLOB NOT NULL, nospam INTEGER NOT NULL)"
      , "CREATE TABLE profile\
        \ (id INTEGER PRIMARY KEY CHECK (id = 0), nickname TEXT NOT NULL, status TEXT NOT NULL)"
      , "CREATE TABLE contacts (key TEXT PRIMARY KEY, name TEXT, unread INTEGER NOT NULL)"
      , "CREATE TABLE messages\
        \ (contact TEXT NOT NULL REFERENCES contacts (key), place INTEGER NOT NULL,\
        \ mine INTEGER NOT NULL, text TEXT NOT NULL, time INTEGER NOT NULL,\
        \ delivery TEXT NOT NULL,\
        \ file_size INTEGER, file_path TEXT, file_done INTEGER, file_state TEXT,\
        \ call_video INTEGER, call_duration INTEGER, call_missed INTEGER,\
        \ PRIMARY KEY (contact, place))"
      , "CREATE TABLE requests (key TEXT PRIMARY KEY, message TEXT NOT NULL)"
      , "INSERT INTO identity VALUES (0, X'00', 0)"
      , "INSERT INTO contacts VALUES ('E8743B05', 'Bob', 0)"
      , "INSERT INTO messages VALUES ('E8743B05', 0, 1, 'hi', 0, 'sent',\
        \ NULL, NULL, NULL, NULL, NULL, NULL, NULL)"
      , "INSERT INTO messages VALUES ('E8743B05', 1, 0, 'a.txt', 0, 'delivered',\
        \ 10, '/received/a.txt', 5, 'stopped', NULL, NULL, NULL)"
      , "PRAGMA user_version = 1"
      ]
    database <- opened db
    model <- load . profileStore database =<< activeProfile database
    map (\m -> (messageText m, messageDelivery m, messageParts m)) . contactMessages
      <$> Map.lookup (ContactId "E8743B05") (modelContacts model)
      `shouldBe` Just [("hi", Sent, 0), ("a.txt", Delivered, 0)]
    -- What hTox took before stays.
    settingAutoAccept (modelAccount model) `shouldBe` 32 * 1024 * 1024
    -- Files from before IDs can't go on.
    [ attachmentState a | Just c <- [Map.lookup (ContactId "E8743B05") (modelContacts model)]
                        , m <- contactMessages c, Just a <- [messageFile m] ]
      `shouldBe` [Cancelled]

  it "refuses a database of a newer version, leaving it" $ do
    db <- SQLite.open ":memory:"
    SQLite.execute_ db "PRAGMA user_version = 99"
    void (opened db) `shouldThrow` (\(Newer v) -> v == 99)
    SQLite.query_ db "PRAGMA user_version" `shouldReturn` [SQLite.Only (99 :: Int)]

  it "keeps files, and stops those a restart cut off" $ do
    let events = history ++
          [ Open (Chat alice), Attach "/sent/a.png" "a.png" 10 "ours", Send (at 3 9 0)
          , FileOffered alice (Receiving 1) (at 3 9 1) "b.txt" 20 "/received/b.txt" "theirs"
          , FileMoved alice (Receiving 1) 5 ]
    model <- load =<< saved events
    [ (messageText m, f) | m <- maybe [] contactMessages (Map.lookup alice (modelContacts model)), Just f <- [messageFile m] ]
      `shouldBe` [ ("a.png", Attachment 10 "/sent/a.png" 0 Waiting (Just "ours"))
                 , ("b.txt", Attachment 20 "/received/b.txt" 5 Interrupted (Just "theirs")) ]

  it "keeps what is picked for a draft" $ do
    let staged = [StagedFile "/sent/a.png" "a.png" 10 "ours", StagedPlace 1.5 2.5]
    model <- load =<< saved (history ++ [Open (Chat alice), Attach "/sent/a.png" "a.png" 10 "ours", ShareLocation 1.5 2.5])
    Map.lookup alice (modelStaged model) `shouldBe` Just staged

  it "keeps calls" $ do
    let events = history ++
          [ Open (Chat alice), StartCall (at 3 9 0) True, CallPeer alice (at 3 9 0) True, HangUp (at 3 9 2)
          , CallInvited alice (at 3 9 3) False, CallEnded alice (at 3 9 4) ]
    model <- load =<< saved events
    [ c | m <- maybe [] contactMessages (Map.lookup alice (modelContacts model)), Just c <- [messageCall m] ]
      `shouldBe` [CallRecord True (Just 120) False, CallRecord False Nothing True]

  it "keeps each account's chats apart, and which is online" $ do
    database <- open ":memory:"
    first <- activeProfile database
    _ <- savedIn database history
    second <- newProfile database Nothing (modelProfile initial) { profileNickname = "Other" }
    setActive database second
    other <- load =<< savedIn database [Requested carol "hi"]
    (Map.keys (modelContacts other), Map.keys (modelRequests other)) `shouldBe` ([], [carol])
    map accountName (modelProfiles other) `shouldBe` ["Tox User", "Other"]
    activeProfile database `shouldReturn` second
    deleteProfile database first
    map accountId <$> profiles database `shouldReturn` [second]
    Map.keys . modelContacts <$> load (profileStore database first) `shouldReturn` []

  it "keeps groups with their chats, and invites" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        events = history ++
          [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team True
          , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0), GroupSaved team "state"
          , GroupReceived team (at 3 9 1) 1 Tox.MessageAction "waves", DraftChanged "later"
          , TopicChanged team (at 3 9 2) (Just 1) "plans", TopicSetByUs team (at 3 9 3) "ours"
          , MuteGroup team Nothing, ToggleMentions team, ChooseRelayBot team (Just (memberKey dana))
          , PrivateReceived team (at 3 9 4) 1 Tox.MessageNormal "psst"
          , GroupModerated team (at 3 9 5) Nothing (Just 1) Tox.ModObserver
          , GroupModerated team (at 3 9 6) (Just 1) Nothing Tox.ModKick
          , Invited bob (GroupId "C0DE") NgcGroup "Other" "other invite"
          , Invited bob (GroupId "01AB") AudioConference "" "cookie"
          , Invited alice (GroupId "00CD") TextConference "" "text cookie", AnswerInvite alice (GroupId "00CD") True ]
        expected = fst (update (Started "") (applying events initial))
    model <- load =<< saved events
    modelGroups model `shouldBe` modelGroups expected
    modelInvites model `shouldBe` modelInvites expected
    modelGroupDrafts model `shouldBe` Map.fromList [(team, "later")]

  it "keeps who we are" $ do
    database <- open ":memory:"
    store <- profileStore database <$> activeProfile database
    identity store `shouldReturn` Nothing
    saveIdentity store (Identity "key" 0xDEADBEEF)
    identity store `shouldReturn` Just (Identity "key" 0xDEADBEEF)
