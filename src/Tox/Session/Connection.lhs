\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
module Tox.Session.Connection where

import           Control.Monad.State          (MonadState, gets, modify)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Word                    (Word64)

import           Tox.Core.Time                (Timestamp)
import           Tox.Core.Timed               (Timed)
import qualified Tox.Core.Timed               as Timed
import           Tox.Crypto.Core.Key               (PublicKey)
import           Tox.Crypto.Core.KeyPair           (KeyPair)
import           Tox.DHT.DhtState             (DhtState)
import           Tox.Network.Core.NodeInfo         (NodeInfo)
import           Tox.Onion.Operation          (OnionState)
import qualified Tox.Onion.Operation          as Onion
import           Tox.Transport.SecureSession    (SecureSessionState)
import qualified Tox.Transport.SecureSession    as SecureSession
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)
import           Tox.Network.Core.Networked        (Networked)
import           Tox.Crypto.Core.Keyed             (Keyed)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

data FriendStatus
  = FriendDisconnected
  | FriendSearching
  | FriendKeyFound PublicKey [NodeInfo] -- ^ DHT PK and initial relays
  | FriendConnecting SecureSessionState
  | FriendConnected SecureSessionState
  deriving (Show)

data FriendConnection = FriendConnection
  { fcRealPk     :: PublicKey
  , fcStatus     :: FriendStatus
  , fcLastSeen   :: Maybe Timestamp
  } deriving (Show)

data ConnectionManager = ConnectionManager
  { friends      :: Map PublicKey FriendConnection
  , ourRealKeys  :: KeyPair
  , ourDhtKeys   :: KeyPair
  }


class (Monad m, Timed m, MonadRandomBytes m, Keyed m, Networked m) => ConnectionMonad m where
  getConnManager :: m ConnectionManager
  putConnManager :: ConnectionManager -> m ()

getsConn :: ConnectionMonad m => (ConnectionManager -> a) -> m a
getsConn f = f <$> getConnManager

modifyConn :: ConnectionMonad m => (ConnectionManager -> ConnectionManager) -> m ()
modifyConn f = getConnManager >>= putConnManager . f


-- | Initialize a new connection manager.
initManager :: KeyPair -> KeyPair -> ConnectionManager
initManager realKeys dhtKeys = ConnectionManager
  { friends = Map.empty
  , ourRealKeys = realKeys
  , ourDhtKeys = dhtKeys
  }


-- | Add a new friend to be managed.
addFriend :: ConnectionMonad m => PublicKey -> m ()
addFriend realPk = modifyConn $ \s ->
  let newFriend = FriendConnection realPk FriendDisconnected Nothing
  in s { friends = Map.insertWith (\_ old -> old) realPk newFriend (friends s) }


-- | Maintenance loop for friend connections.
-- Needs access to OnionState to check for discovered keys.
doFriendConnections :: (ConnectionMonad m, Onion.OnionNodeMonad m) => m ()
doFriendConnections = do
  friendPks <- getsConn (Map.keys . friends)
  forM_ friendPks $ \pk -> do
    f <- getsConn (Map.findWithDefault (error "Friend missing") pk . friends)
    case fcStatus f of
      FriendDisconnected -> do
        -- Start onion search
        Onion.startFriendSearch pk
        updateFriendStatus pk FriendSearching

      FriendSearching -> do
        -- Check if onion found the key
        mRelays <- Onion.getsOnion (Map.lookup pk . Onion.searchNodes)
        case mRelays of
          Nothing -> return ()
          Just relays | Map.null relays -> return ()
          Just relays -> do
            -- Found DHT PK
            let dhtPks = Map.keys relays
                relaysList = map Onion.nodeInfo $ Map.elems relays
            case dhtPks of
              (dhtPk:_) -> updateFriendStatus pk (FriendKeyFound dhtPk relaysList)
              [] -> return ()

      FriendKeyFound dhtPk relays -> do
        cm <- getConnManager
        let ourReal = ourRealKeys cm
            ourDht = ourDhtKeys cm
        -- Pick the best relay to start with
        case relays of
          [] -> return ()
          (r:_) -> do
            ss <- SecureSession.initSession ourReal pk ourDht dhtPk r
            updateFriendStatus pk (FriendConnecting ss)
            -- Handshake will be triggered by first send or periodic retry
            return ()

      FriendConnecting ss -> do
        case SecureSession.ssStatus ss of
          Just SecureSession.SessionConfirmed -> updateFriendStatus pk (FriendConnected ss)
          _ -> return () -- Still waiting for handshake

      FriendConnected _ -> return () -- All good
  where
    forM_ = flip mapM_
    updateFriendStatus pk status = modifyConn $ \s ->
      let f = Map.findWithDefault (error "Friend missing") pk (friends s)
      in s { friends = Map.insert pk (f { fcStatus = status }) (friends s) }

\end{code}

\chapter{Friend connection}

\texttt{friend\_connection} is the module that sits on top of the DHT, onion and
\texttt{net\_crypto} modules and takes care of linking the 3 together.

Friends in \texttt{friend\_connection} are represented by their real public key.
When a friend is added in \texttt{friend\_connection}, an onion search entry is
created for that friend.  This means that the onion module will start looking
for this friend and send that friend their DHT public key, and the TCP relays
it is connected to, in case a connection is only possible with TCP.

Once the onion returns the DHT public key of the peer, the DHT public key is
saved, added to the DHT friends list and a new \texttt{net\_crypto} connection
is created.  Any TCP relays returned by the onion for this friend are passed to
the \texttt{net\_crypto} connection.

If the DHT establishes a direct UDP connection with the friend,
\texttt{friend\_connection} will pass the IP/port of the friend to
\texttt{net\_crypto} and also save it to be used to reconnect to the friend if
they disconnect.

If \texttt{net\_crypto} finds that the friend has a different DHT public key,
which can happen if the friend restarted their client, \texttt{net\_crypto} will
pass the new DHT public key to the onion module and will remove the DHT entry
for the old DHT public key and replace it with the new one.  The current
\texttt{net\_crypto} connection will also be killed and a new one with the
correct DHT public key will be created.

When the \texttt{net\_crypto} connection for a friend goes online,
\texttt{friend\_connection} will tell the onion module that the friend is online
so that it can stop spending resources looking for the friend.  When the friend
connection goes offline, \texttt{friend\_connection} will tell the onion module
so that it can start looking for the friend again.

There are 2 types of data packets sent to friends with the \texttt{net\_crypto}
connection handled at the level of \texttt{friend\_connection}, Alive packets
and TCP relay packets.  Alive packets are packets with the packet id or first
byte of data (only byte in this packet) being 16.  They are used in order to
check if the other friend is still online.  \texttt{net\_crypto} does not have
any timeout when the connection is established so timeouts are caught using
this packet.  In toxcore, this packet is sent every 8 seconds.  If none of
these packets are received for 32 seconds, the connection is timed out and
killed.  These numbers seem to cause the least issues and 32 seconds is not too
long so that, if a friend times out, toxcore won't falsely see them online for
too long.  Usually when a friend goes offline they have time to send a
disconnect packet in the \texttt{net\_crypto} connection which makes them appear
offline almost instantly.

The timeout for when to stop retrying to connect to a friend by creating new
\texttt{net\_crypto} connections when the old one times out in toxcore is the
same as the timeout for DHT peers (122 seconds).  However, it is calculated
from the last time a DHT public key was received for the friend or time the
friend's \texttt{net\_crypto} connection went offline after being online.  The
highest time is used to calculate when the timeout is.  \texttt{net\_crypto}
connections will be recreated (if the connection fails) until this timeout.

\texttt{friend\_connection} sends a list of 3 relays (the same number as the
target number of TCP relay connections in \texttt{TCP\_connections}) to each
connected friend every 5 minutes in toxcore.  Immediately before sending the
relays, they are associated to the current \texttt{net\_crypto->TCP\_connections}
connection.  This facilitates connecting the two friends together using the
relays as the friend who receives the packet will associate the sent relays to
the \texttt{net\_crypto} connection they received it from.  When both sides do
this they will be able to connect to each other using the relays.  The packet
id or first byte of the packet of share relay packets is 0x11.  This is then
followed by some TCP relays stored in packed node format.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x11) \\
  variable           & TCP relays in packed node format (see DHT) \\
\end{tabular}

If local IPs are received as part of the packet, the local IP will be replaced
with the IP of the peer that sent the relay.  This is because we assume this is
the best way to attempt to connect to the TCP relay.  If the peer that sent the
relay is using a local IP, then the sent local IP should be used to connect to
the relay.

For all other data packets, are passed by \texttt{friend\_connection} up to the
upper Messenger module.  It also separates lossy and lossless packets from
\texttt{net\_crypto}.

Friend connection takes care of establishing the connection to the friend and
gives the upper messenger layer a simple interface to receive and send
messages, add and remove friends and know if a friend is connected (online) or
not connected (offline).
