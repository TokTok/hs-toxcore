{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Concurrent        (forkIO, myThreadId, threadDelay,
                                            throwTo)
import           Control.Exception         (IOException, finally, try)
import           Control.Monad             (forM_, forever, join, replicateM_,
                                            unless, void, when, (>=>))
import           Control.Monad.Logger      (LogLevel (..))
import           Data.Aeson                (FromJSON, decode, parseJSON,
                                            withObject, (.:), (.:?))
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Char8     as BS8
import           Data.Char                 (toUpper)
import           Data.IORef                (IORef, atomicModifyIORef',
                                            modifyIORef, newIORef, readIORef)
import           Data.List                 (find)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe, isJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           Network.HTTP.Simple       (getResponseBody, httpLBS,
                                            parseRequest)
import           Options.Applicative
import           System.Directory          (createDirectoryIfMissing,
                                            listDirectory, renameFile)
import           System.Exit               (ExitCode (..))
import           System.FilePath           (takeExtension, takeFileName, (<.>),
                                            (</>))
import           System.IO                 (BufferMode (..), hSetBuffering,
                                            stdout)
import           System.Posix.Signals      (Handler (..), installHandler,
                                            sigTERM)
import           Text.Read                 (readMaybe)

import qualified Crypto.Saltine.Class      as Sodium
import qualified Data.Binary               as Binary
import qualified Data.ByteString.Lazy      as LBS
import qualified Tox.Application.GroupChat as Group
import qualified Tox.Application.Save      as Save
import qualified Tox.Application.Tox       as Tox
import           Tox.Application.Tox       (Event (..))
import qualified Tox.Crypto.Core.Key       as Key
import           Tox.Crypto.Core.Key       (PublicKey)
import qualified Tox.Crypto.Core.KeyPair   as KeyPair
import           Tox.Network.Core.NodeInfo (NodeInfo)

data ToxNodeJSON = ToxNodeJSON
    { jsonIpv4      :: Maybe String
    , jsonIpv6      :: Maybe String
    , jsonPort      :: Int
    , jsonTcpPorts  :: [Int]
    , jsonPublicKey :: String
    , jsonStatusUdp :: Bool
    , jsonStatusTcp :: Bool
    } deriving (Show, Generic)

instance FromJSON ToxNodeJSON where
    parseJSON = withObject "ToxNodeJSON" $ \v -> ToxNodeJSON
        <$> v .:? "ipv4"
        <*> v .:? "ipv6"
        <*> v .: "port"
        <*> (fromMaybe [] <$> v .:? "tcp_ports")
        <*> v .: "public_key"
        <*> (fromMaybe False <$> v .:? "status_udp")
        <*> (fromMaybe False <$> v .:? "status_tcp")

newtype NodesList = NodesList
    { nodes :: [ToxNodeJSON]
    } deriving (Show, Generic)

instance FromJSON NodesList

parsePublicKey :: String -> Maybe PublicKey
parsePublicKey s = case Base16.decode (BS8.pack s) of
    Right decoded -> Key.Key <$> Sodium.decode decoded
    Left _        -> Nothing

showPublicKey :: PublicKey -> String
showPublicKey = showHex . Sodium.encode

showHex :: BS.ByteString -> String
showHex = map toUpper . BS8.unpack . Base16.encode

-- | A friend given as a public key, or as a Tox ID.
data FriendArg = FriendKey PublicKey | FriendAddress BS.ByteString

parseFriend :: String -> IO FriendArg
parseFriend s = case Base16.decode (BS8.pack s) of
    Right bytes | Just pk <- Key.Key <$> Sodium.decode bytes -> return $ FriendKey pk
    Right bytes | Just _ <- Tox.parseAddress bytes -> return $ FriendAddress bytes
    _ -> fail $ "Not a public key or valid Tox ID: " ++ s

resolveNode :: ToxNodeJSON -> IO [NodeInfo]
resolveNode j = resolveAt (jsonPort j) j

resolveAt :: Int -> ToxNodeJSON -> IO [NodeInfo]
resolveAt port j = case parsePublicKey (jsonPublicKey j) of
    Nothing -> return []
    Just pk -> concat <$> mapM (\host -> Tox.resolveNode host (fromIntegral port) pk)
        (filter (/= "-") $ catMaybes [jsonIpv4 j, jsonIpv6 j])

-- | How many of the listed nodes we bootstrap from, and use as TCP relays.
listedNodes :: Int
listedNodes = 4

-- | Resolve a node given as HOST:PORT:PUBLIC_KEY, with an IPv6 host in
-- brackets.
resolveNodeSpec :: String -> IO NodeInfo
resolveNodeSpec spec = case hostAndRest spec of
    (host, ':' : rest) | [port, pk] <- splitOn ':' rest, Just p <- readMaybe port -> do
        resolved <- resolveNode (ToxNodeJSON (Just host) Nothing p [] pk True False)
        case resolved of
            node : _ -> return node
            []       -> fail $ "Cannot resolve node: " ++ spec
    _ -> fail $ "Expected HOST:PORT:PUBLIC_KEY, got: " ++ spec
  where
    hostAndRest ('[' : rest) = case break (== ']') rest of
        (host, _ : after) -> (host, after)
        (host, [])        -> (host, [])
    hostAndRest s = break (== ':') s
    splitOn c s = case break (== c) s of
        (a, [])       -> [a]
        (a, _ : rest) -> a : splitOn c rest

data ToxOptions = ToxOptions
    { optFriendPk       :: [String]
    , optBootstrap      :: [String]
    , optTcpRelays      :: [String]
    , optNoUdp          :: Bool
    , optNoLan          :: Bool
    , optTcpPort        :: Maybe Int
    , optPort           :: Maybe Int
    , optBootstrapUrl   :: String
    , optSeed           :: Maybe Int
    , optSave           :: Maybe FilePath
    , optChats          :: Maybe FilePath
    , optName           :: Maybe String
    , optMessage        :: Maybe String
    , optRequestMessage :: String
    , optAcceptRequests :: Bool
    , optSendFile       :: Maybe FilePath
    , optReceiveDir     :: Maybe FilePath
    , optConference     :: Maybe String
    , optConferenceJoin :: Maybe String
    , optGroup          :: Maybe String
    , optGroupNew       :: Maybe String
    , optGroupPublic    :: Maybe String
    , optGroupJoin      :: Maybe String
    , optGroupModerate  :: Bool
    , optGroupLeave     :: Bool
    , optGroupTopic     :: Maybe String
    , optGroupPrivate   :: Maybe String
    , optCustom         :: Maybe String
    , optPrintId        :: Bool
    , optVerbose        :: Bool
    }

toxOptions :: Parser ToxOptions
toxOptions = ToxOptions
    <$> many (strOption
        ( long "friend"
       <> short 'f'
       <> metavar "PUBLIC_KEY|TOX_ID"
       <> help "Friend to add: a public key adds them directly, a Tox ID sends a friend request" ))
    <*> many (strOption
        ( long "bootstrap"
       <> short 'b'
       <> metavar "HOST:PORT:DHT_KEY"
       <> help "Node to bootstrap from instead of the nodes from --bootstrap-url" ))
    <*> many (strOption
        ( long "tcp-relay"
       <> metavar "HOST:PORT:PUBLIC_KEY"
       <> help "TCP relay to connect to" ))
    <*> switch
        ( long "no-udp"
       <> help "Send nothing over UDP: reach the network only through TCP relays" )
    <*> switch
        ( long "no-local-discovery"
       <> help "Do not look for peers on the local network" )
    <*> optional (option auto
        ( long "tcp-port"
       <> metavar "PORT"
       <> help "Run a TCP relay on this port" ))
    <*> optional (option auto
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "Local UDP port to bind to, 0 for any (default: the first free one from 33445 to 33545)" ))
    <*> strOption
        ( long "bootstrap-url"
       <> metavar "URL"
       <> help "URL to fetch bootstrap nodes from"
       <> value "https://nodes.tox.chat/json"
       <> showDefault )
    <*> optional (option auto
        ( long "seed"
       <> short 's'
       <> metavar "INT"
       <> help "Seed for deterministic identity generation" ))
    <*> optional (strOption
        ( long "save"
       <> metavar "FILE"
       <> help "Load the profile from this file if it exists, and keep it saved there" ))
    <*> optional (strOption
        ( long "chats"
       <> metavar "DIR"
       <> help "Keep each group and conference in its own file here, and take only the key from the save file, as clients with their own storage do" ))
    <*> optional (strOption
        ( long "name"
       <> metavar "NAME"
       <> help ("Our nickname (default: the saved one, or " ++ show defaultName ++ ")") ))
    <*> optional (strOption
        ( long "message"
       <> short 'm'
       <> metavar "TEXT"
       <> help "Message to send to the friend when it comes online" ))
    <*> strOption
        ( long "request-message"
       <> metavar "TEXT"
       <> help "Message of the friend request sent to a Tox ID"
       <> value "Hello from Haskell"
       <> showDefault )
    <*> switch
        ( long "accept-requests"
       <> help "Accept all friend requests" )
    <*> optional (strOption
        ( long "send-file"
       <> metavar "FILE"
       <> help "File to send to the friend when it comes online" ))
    <*> optional (strOption
        ( long "receive-dir"
       <> metavar "DIR"
       <> help "Accept files from friends and save them here" ))
    <*> optional (strOption
        ( long "conference"
       <> metavar "TEXT"
       <> help "Invite the friend to a new conference and say this there once it joins" ))
    <*> optional (strOption
        ( long "conference-join"
       <> metavar "TEXT"
       <> help "Join the conferences friends invite us to and say this there" ))
    <*> optional (strOption
        ( long "group"
       <> metavar "TEXT"
       <> help "Join groups friends invite us to, and say this there" ))
    <*> optional (strOption
        ( long "group-new"
       <> metavar "NAME"
       <> help "Invite the friend to a new private group with this name" ))
    <*> optional (strOption
        ( long "group-public"
       <> metavar "NAME"
       <> help "Found a public group with this name and print its chat id" ))
    <*> optional (strOption
        ( long "group-join"
       <> metavar "CHAT_ID"
       <> help "Join the public group with this chat id" ))
    <*> switch
        ( long "group-moderate"
       <> help "In groups we found, try every setting and role on each peer that joins, then kick it" )
    <*> switch
        ( long "group-leave"
       <> help "Leave a group once a peer echoes our message there" )
    <*> optional (strOption
        ( long "group-topic"
       <> metavar "TEXT"
       <> help "Set this topic when a group is ready or a peer joins, saying whether we may and whether it was taken" ))
    <*> optional (strOption
        ( long "group-private"
       <> metavar "TEXT"
       <> help "Say this privately to each peer that joins a group" ))
    <*> optional (strOption
        ( long "custom"
       <> metavar "TEXT"
       <> help "Send this in a custom lossless and lossy packet to the friend when it comes online" ))
    <*> switch
        ( long "print-id"
       <> help "Print our public key, Tox ID and DHT key and exit" )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Log debug messages" )

defaultName :: String
defaultName = "Haskell peer"

nick :: ToxOptions -> BS.ByteString
nick = BS8.pack . fromMaybe defaultName . optName

main :: IO ()
main = do
    opts <- execParser optsInfo
    runToxNode opts
  where
    optsInfo = info (toxOptions <**> helper)
      ( fullDesc
     <> progDesc "Start a full Tox node in Haskell"
     <> header "tox-dht-node - a cleanroom Tox implementation" )

runToxNode :: ToxOptions -> IO ()
runToxNode opts = do
    hSetBuffering stdout LineBuffering
    friend <- mapM parseFriend (optFriendPk opts)
    saved <- case optSave opts of
        Nothing -> return Nothing
        Just file -> either (\(_ :: IOException) -> Nothing) Just <$> try (BS.readFile file)
    let logger level msg = when (optVerbose opts || level >= LevelInfo) $
            putStrLn $ "[" ++ drop 5 (show level) ++ "] " ++ Text.unpack msg
    tox <- Tox.new Tox.defaultOptions
        { Tox.optUdpEnabled = not (optNoUdp opts)
        , Tox.optUdpPorts   = maybe (Tox.optUdpPorts Tox.defaultOptions) (\p -> (fromIntegral p, fromIntegral p)) (optPort opts)
        , Tox.optTcpPort    = fromIntegral <$> optTcpPort opts
        , Tox.optSaveData   = maybe Tox.NoSaveData (if isJust (optChats opts) then keyOnly else Tox.ToxSave) saved
        , Tox.optSeed       = optSeed opts
        , Tox.optLocalDiscovery = not (optNoLan opts)
        , Tox.optLogger     = logger
        } >>= either (fail . ("Cannot start: " ++) . show) return
    forM_ (optChats opts) $ \dir -> do
        names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory dir)
        forM_ [ n | n <- names, takeExtension n == ".group" ] $ \n ->
            BS.readFile (dir </> n) >>= Tox.groupRestore tox >>= putStrLn . maybe "GROUP_RESTORE_FAILED" (("GROUP_RESTORED " ++) . show)
        forM_ [ n | n <- names, takeExtension n == ".conference" ] $ \n ->
            BS.readFile (dir </> n) >>= Tox.conferenceRestore tox
                >>= putStrLn . maybe "CONFERENCE_RESTORE_FAILED" (("CONFERENCE_RESTORED " ++) . show)
    -- A saved profile keeps its name unless we are given one.
    run tox friend =<< case (optName opts, saved) of
        (Nothing, Just _) -> (\n -> opts { optName = if BS.null n then Nothing else Just (BS8.unpack n) }) <$> Tox.selfGetName tox
        _                 -> return opts

run :: Tox.Tox -> [FriendArg] -> ToxOptions -> IO ()
run tox friend opts = do
    putStrLn $ "PUBLIC_KEY " ++ showPublicKey (Tox.selfPublicKey tox)
    Tox.selfAddress tox >>= putStrLn . ("TOX_ID " ++) . showHex
    putStrLn $ "DHT_ID " ++ showPublicKey (Tox.selfDhtId tox)
    Tox.groupList tox >>= mapM_ (Tox.groupChatId tox >=> mapM_ (putStrLn . ("GROUP " ++) . showHex))
    if optPrintId opts then Tox.kill tox else do
        putStrLn $ "UDP_PORT " ++ show (Tox.selfUdpPort tox)
        (bootstrapNodes, listedRelays) <- case optBootstrap opts of
            [] -> do
                putStrLn $ "Fetching nodes from " ++ optBootstrapUrl opts ++ "..."
                req <- parseRequest (optBootstrapUrl opts)
                res <- httpLBS req
                case decode (getResponseBody res) :: Maybe NodesList of
                    Nothing -> putStrLn "Failed to parse nodes JSON" >> return ([], [])
                    Just nl -> do
                        udp <- concat <$> mapM resolveNode (take listedNodes . filter jsonStatusUdp $ nodes nl)
                        tcp <- concat <$> sequence (take listedNodes
                            [ resolveAt port j | j <- nodes nl, jsonStatusTcp j, port : _ <- [jsonTcpPorts j] ])
                        return (udp, tcp)
            specs -> (, []) <$> mapM resolveNodeSpec specs
        mapM_ (Tox.bootstrap tox) bootstrapNodes
        relays <- mapM resolveNodeSpec (optTcpRelays opts)
        mapM_ (Tox.addTcpRelay tox) (relays ++ listedRelays)
        void $ Tox.selfSetName tox (nick opts)
        forM_ friend $ \case
            FriendKey pk -> void $ Tox.friendAddNoRequest tox pk
            FriendAddress address -> Tox.friendAdd tox address (BS8.pack $ optRequestMessage opts)
                >>= either (fail . ("Cannot add friend: " ++) . show) (const $ return ())
        forM_ (optGroupPublic opts) $ \name ->
            Tox.groupNew tox False (BS8.pack name) (nick opts)
                >>= mapM (Tox.groupChatId tox) >>= putStrLn . maybe "GROUP_NEW_FAILED" (("GROUP_CHAT_ID " ++) . showHex) . join
        forM_ (optGroupJoin opts) $ \chatId -> case Base16.decode (BS8.pack chatId) of
            Right bytes -> Tox.groupJoin tox bytes (nick opts) "" >>= putStrLn . ("GROUP_JOIN " ++) . show
            Left err -> fail $ "Invalid chat id: " ++ err
        let save = do
                forM_ (optSave opts) $ \file -> Tox.saveData tox >>= writeAtomically file
                forM_ (optChats opts) $ \dir -> do
                    createDirectoryIfMissing True dir
                    Tox.groupList tox >>= mapM_ (\g -> do
                        chatId <- Tox.groupChatId tox g
                        blob <- Tox.groupSave tox g
                        sequence_ $ (\c b -> writeAtomically (dir </> showHex c <.> "group") b) <$> chatId <*> blob)
                    Tox.conferenceList tox >>= mapM_ (\g -> do
                        conferenceId <- Tox.conferenceId tox g
                        blob <- Tox.conferenceSave tox g
                        sequence_ $ (\c b -> writeAtomically (dir </> showHex c <.> "conference") b) <$> conferenceId <*> blob)
        save
        mainThread <- myThreadId
        void $ installHandler sigTERM (Catch $ Tox.kill tox >> throwTo mainThread ExitSuccess) Nothing
        files <- newIORef Map.empty
        greeting <- newIORef Set.empty
        forever $ Tox.nextEvent tox >>= \event -> do
            handleEvent opts tox files greeting event
            unless (fileData event) save
  where
    fileData = \case
        FileReceiveChunk {} -> True
        FileChunkRequest {} -> True
        _                   -> False

-- | The key of a profile in toxcore's save format, and nothing else.
keyOnly :: BS.ByteString -> Tox.SaveData
keyOnly bytes = case Binary.decodeOrFail (LBS.fromStrict bytes) of
    Right (_, _, sd) | Just p <- Save.fromSaveData sd -> Tox.SecretKey (KeyPair.secretKey (Save.profileKeys p))
    _ -> Tox.ToxSave bytes

writeAtomically :: FilePath -> BS.ByteString -> IO ()
writeAtomically file bytes = BS.writeFile (file ++ ".new") bytes >> renameFile (file ++ ".new") file

-- | Files we send, and files we receive with what arrived so far, by friend
-- and file number.
type Files = IORef (Map (PublicKey, Tox.FileRef) (FilePath, [BS.ByteString]))

-- | Conferences we are trying to say our message in.
type Greeting = IORef (Set Tox.GroupNumber)

handleEvent :: ToxOptions -> Tox.Tox -> Files -> Greeting -> Event -> IO ()
handleEvent opts tox files greeting = \case
    FriendConnectionChanged pk Tox.NotConnected -> putStrLn $ "FRIEND_OFFLINE " ++ showPublicKey pk
    FriendConnectionChanged pk _ -> do
        putStrLn $ "FRIEND_ONLINE " ++ showPublicKey pk
        forM_ (optMessage opts) $ \msg -> do
            sent <- Tox.friendSendMessage tox pk Tox.MessageNormal (BS8.pack msg)
            putStrLn $ either (const "SEND_FAILED ") (const "SENT ") sent ++ msg
        forM_ (optConference opts) $ \_ -> do
            existing <- Tox.conferenceList tox
            conference <- case existing of
                g : _ -> return (Just g)
                [] -> Tox.conferenceNew tox >>= mapM (\g -> g <$ Tox.conferenceSetTitle tox g "Haskell conference")
            forM_ conference $ \g -> do
                invited <- Tox.conferenceInvite tox pk g
                putStrLn $ (if invited then "CONFERENCE_INVITED " else "CONFERENCE_INVITE_FAILED ") ++ show g
        forM_ (optGroupNew opts) $ \name -> do
            existing <- Tox.groupList tox
            group <- case existing of
                g : _ -> return (Just g)
                []    -> Tox.groupNew tox True (BS8.pack name) (nick opts)
            forM_ group $ \g -> do
                invited <- Tox.groupInviteFriend tox g pk
                putStrLn $ (if invited then "GROUP_INVITED " else "GROUP_INVITE_FAILED ") ++ show g
        forM_ (optCustom opts) $ \text -> do
            Tox.friendSendLosslessPacket tox pk (BS8.pack ('\160' : text)) >>= putStrLn . ("LOSSLESS_SENT " ++) . show
            -- Lossy packets may be lost.
            void . forkIO . replicateM_ 10 $ do
                void $ Tox.friendSendLossyPacket tox pk (BS8.pack ('\200' : text))
                threadDelay 200000
        forM_ (optSendFile opts) $ \path -> do
            bytes <- BS.readFile path
            let name = takeFileName path
            Tox.fileSend tox pk Tox.fileKindData (fromIntegral $ BS.length bytes) Nothing (BS8.pack name) >>= \case
                Left err -> putStrLn $ "FILE_SEND_FAILED " ++ show err
                Right n -> modifyIORef files (Map.insert (pk, Tox.Outgoing n) (name, [bytes]))
    FileChunkRequest pk n _ 0 -> do
        sent <- Map.lookup (pk, Tox.Outgoing n) <$> readIORef files
        forM_ sent $ \(name, _) -> putStrLn $ "FILE_SENT " ++ name
        modifyIORef files (Map.delete (pk, Tox.Outgoing n))
    FileChunkRequest pk n position len -> do
        sent <- Map.lookup (pk, Tox.Outgoing n) <$> readIORef files
        forM_ sent $ \(_, chunks) -> void $
            Tox.fileSendChunk tox pk n position (BS.take len . BS.drop (fromIntegral position) $ BS.concat chunks)
    FileReceive pk n _ _ _ name -> forM_ (optReceiveDir opts) $ \dir -> case takeFileName (BS8.unpack name) of
        -- Only a plain name, so the sender cannot write outside the directory.
        file | file `notElem` ["", ".", ".."] -> do
            modifyIORef files (Map.insert (pk, Tox.Incoming n) (dir </> file, []))
            void $ Tox.fileControl tox pk (Tox.Incoming n) Tox.FileResume
        _ -> void $ Tox.fileControl tox pk (Tox.Incoming n) Tox.FileCancel
    FileReceiveChunk pk n _ chunk -> do
        received <- Map.lookup (pk, Tox.Incoming n) <$> readIORef files
        forM_ received $ \(path, chunks) -> if BS.null chunk
            then do
                let bytes = BS.concat (reverse chunks)
                BS.writeFile path bytes
                putStrLn $ "FILE_RECEIVED " ++ takeFileName path ++ " " ++ show (BS.length bytes)
                modifyIORef files (Map.delete (pk, Tox.Incoming n))
            else modifyIORef files (Map.insert (pk, Tox.Incoming n) (path, chunk : chunks))
    FriendMessage pk _ msg -> putStrLn $ "MESSAGE " ++ showPublicKey pk ++ " " ++ BS8.unpack msg
    FriendLosslessPacket pk bytes -> putStrLn $ "LOSSLESS_PACKET " ++ showPublicKey pk ++ " " ++ BS8.unpack (BS.drop 1 bytes)
    -- Only the custom ones: the rest is audio and video.
    FriendLossyPacket pk bytes | Just (ident, rest) <- BS.uncons bytes, ident >= 200 ->
        putStrLn $ "LOSSY_PACKET " ++ showPublicKey pk ++ " " ++ BS8.unpack rest
    FriendRequest pk msg -> do
        putStrLn $ "FRIEND_REQUEST " ++ showPublicKey pk ++ " " ++ BS8.unpack msg
        when (optAcceptRequests opts) $ void $ Tox.friendAddNoRequest tox pk
    FriendName pk name -> putStrLn $ "FRIEND_NAME " ++ showPublicKey pk ++ " " ++ BS8.unpack name
    FriendReadReceipt pk msgId -> putStrLn $ "RECEIPT " ++ showPublicKey pk ++ " " ++ show msgId
    SelfConnectionChanged status -> putStrLn $ "SELF_CONNECTION " ++ show status
    ConferencePeerListChanged g -> do
        peers <- Tox.conferencePeers tox g
        putStrLn $ "CONFERENCE_PEERS " ++ show g ++ " " ++ show (length peers)
        friends <- Tox.friendList tox
        -- Once everyone we invited is there. A rejoined peer is listed before
        -- its connection is up, so retry.
        when (length peers == 1 + length friends) $ forM_ (optConference opts <|> optConferenceJoin opts) $ \msg -> do
            start <- atomicModifyIORef' greeting $ \s -> if Set.member g s then (s, False) else (Set.insert g s, True)
            when start . void . forkIO $
                void (retry (40 :: Int) $ Tox.conferenceSendMessage tox g Tox.MessageNormal (BS8.pack msg))
                    `finally` atomicModifyIORef' greeting (\s -> (Set.delete g s, ()))
    ConferenceMessage g _ _ msg -> putStrLn $ "CONFERENCE_MESSAGE " ++ show g ++ " " ++ BS8.unpack msg
    ConferenceInvite friend _ cookie -> forM_ (optConferenceJoin opts) $ \_ ->
        Tox.conferenceJoin tox friend cookie >>= putStrLn . ("CONFERENCE_JOIN " ++) . show
    GroupInvite friend invite name -> do
        putStrLn $ "GROUP_INVITE " ++ BS8.unpack name
        forM_ (optGroup opts) $ \_ ->
            Tox.groupInviteAccept tox friend invite (nick opts) "" >>= putStrLn . ("GROUP_ACCEPT " ++) . show
    -- A group we rejoin from the save file has its peers before we are in.
    GroupSelfJoin g -> do
        putStrLn $ "GROUP_SELF_JOIN " ++ show g
        setTopic g
        peers <- Tox.groupPeers tox g
        unless (null peers) $ forM_ (optGroup opts) $ greetGroup tox g
    GroupPeerJoin g n _ -> do
        setTopic g
        forM_ (optGroupPrivate opts) $ \text ->
            Tox.groupSendPrivateMessage tox g n Tox.MessageNormal (BS8.pack text)
                >>= putStrLn . ("GROUP_PRIVATE_SENT " ++) . either show (const text)
        peers <- Tox.groupPeers tox g
        putStrLn $ "GROUP_PEER_JOIN " ++ show g ++ " " ++ maybe "" (BS8.unpack . Tox.groupPeerName) (find ((== n) . Tox.groupPeerId) peers)
        forM_ (optGroup opts) $ greetGroup tox g
        when (optGroupModerate opts && isJust (optGroupNew opts)) $ do
            set <- sequence [ Tox.groupSetPeerLimit tox g 50, Tox.groupSetVoice tox g Group.VoiceModerators
                            , Tox.groupSetTopicLock tox g False, Tox.groupSetPassword tox g "secret" ]
            putStrLn $ "GROUP_SET " ++ unwords (map show set)
            forM_ [Group.Moderator, Group.Observer, Group.User] $
                Tox.groupSetRole tox g n >=> putStrLn . ("GROUP_SET_ROLE " ++) . show
            Tox.groupKickPeer tox g n >>= putStrLn . ("GROUP_KICK " ++) . show
    GroupModeration g _ _ m -> putStrLn $ "GROUP_MODERATION " ++ show g ++ " " ++ case m of
        Group.ModKick      -> "KICK"
        Group.ModObserver  -> "OBSERVER"
        Group.ModUser      -> "USER"
        Group.ModModerator -> "MODERATOR"
    GroupPeerLimit g n -> putStrLn $ "GROUP_PEER_LIMIT " ++ show g ++ " " ++ show n
    GroupVoiceState g v -> putStrLn $ "GROUP_VOICE " ++ show g ++ " " ++ case v of
        Group.VoiceAll        -> "ALL"
        Group.VoiceModerators -> "MODERATOR"
        Group.VoiceFounder    -> "FOUNDER"
    GroupTopicLock g locked -> putStrLn $ "GROUP_TOPIC_LOCK " ++ show g ++ (if locked then " ENABLED" else " DISABLED")
    GroupPassword g pw -> putStrLn $ "GROUP_PASSWORD " ++ show g ++ " " ++ BS8.unpack pw
    GroupPrivacy g private -> putStrLn $ "GROUP_PRIVACY " ++ show g ++ (if private then " PRIVATE" else " PUBLIC")
    GroupPrivateMessage g _ _ msg _ -> putStrLn $ "GROUP_PRIVATE_MESSAGE " ++ show g ++ " " ++ BS8.unpack msg
    GroupMessage g _ _ msg _ -> do
        putStrLn $ "GROUP_MESSAGE " ++ show g ++ " " ++ BS8.unpack msg
        when (optGroupLeave opts && "echo: " `BS.isPrefixOf` msg) $
            Tox.groupLeave tox g "bye" >>= putStrLn . (++ " " ++ show g) . (\left -> if left then "GROUP_LEFT" else "GROUP_LEAVE_FAILED")
    GroupCustomPacket g _ bytes -> putStrLn $ "GROUP_CUSTOM " ++ show g ++ " " ++ BS8.unpack bytes
    GroupTopic g _ topic -> putStrLn $ "GROUP_TOPIC " ++ show g ++ " " ++ BS8.unpack topic
    GroupPeerExit g _ exit name _ -> putStrLn $ "GROUP_PEER_EXIT " ++ show g ++ " " ++ BS8.unpack name ++ " " ++ show exit
    GroupJoinFail g reason -> putStrLn $ "GROUP_JOIN_FAIL " ++ show g ++ " " ++ show reason
    event -> print event
  where
    setTopic g = forM_ (optGroupTopic opts) $ \topic -> do
        may <- Tox.groupMayTopic tox g
        set <- Tox.groupSetTopic tox g (BS8.pack topic)
        putStrLn $ "GROUP_TOPIC_SET " ++ show may ++ " " ++ show set

-- | Say something in a group, as a message and as a custom packet.
greetGroup :: Tox.Tox -> Group.GroupNumber -> String -> IO ()
greetGroup tox g msg = do
    void $ Tox.groupSendMessage tox g Tox.MessageNormal (BS8.pack msg)
    void $ Tox.groupSendCustomPacket tox g True (BS8.pack msg)

-- | Try an action until it works, a few times a second.
retry :: Int -> IO Bool -> IO Bool
retry 0 _ = return False
retry n act = act >>= \ok -> if ok then return True else threadDelay 250000 >> retry (n - 1) act
