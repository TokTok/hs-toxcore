\begin{code}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Tox.Onion.Path where

import           Control.Monad                    (replicateM, replicateM_,
                                                   unless)
import           Control.Monad.State              (MonadState, gets, modify)
import qualified Data.ByteString                  as BS
import           Data.Function                    (on)
import           Data.List                        (find, nubBy)
import           Data.Word                        (Word32)
import           GHC.Generics                     (Generic)
import           Test.QuickCheck.Arbitrary        (Arbitrary (..))

import qualified Tox.Core.Time                    as Time
import           Tox.Core.Time                    (TimeDiff, Timestamp)
import           Tox.Core.Timed                   (Timed, askTime)
import qualified Tox.Crypto.Core.Box              as Box
import           Tox.Crypto.Core.Key              (Nonce)
import qualified Tox.Crypto.Core.Keyed            as Keyed
import           Tox.Crypto.Core.Keyed            (Keyed)
import qualified Tox.Crypto.Core.KeyPair          as KeyPair
import           Tox.Crypto.Core.KeyPair          (KeyPair (..))
import qualified Tox.Crypto.Core.MonadRandomBytes as MonadRandomBytes (uniformSafe)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes, newKeyPair,
                                                   uniform)
import qualified Tox.Network.Core.NodeInfo        as NodeInfo
import           Tox.Network.Core.NodeInfo        (NodeInfo)
import           Tox.Network.Core.SocketAddress   (SocketAddress)
import qualified Tox.Onion.Tunnel                 as Tunnel


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


class (MonadState OnionPathState m, Timed m, MonadRandomBytes m, Keyed m) => OnionPathMonad m


maxPaths :: Int
maxPaths = 6

pathLifetime :: TimeDiff
pathLifetime = Time.seconds 1200

unconfirmedPathTimeout :: TimeDiff
unconfirmedPathTimeout = Time.seconds 4

confirmedPathTimeout :: TimeDiff
confirmedPathTimeout = Time.seconds 10


data OnionPath = OnionPath
  { pathNodes       :: [NodeInfo] -- Exactly 3 nodes
  , pathKeys        :: [KeyPair]  -- Temporary keypairs for each hop
  , pathConfirmed   :: Bool
  , pathTries       :: Int
  , pathExpires     :: Timestamp
  , pathLastAttempt :: Maybe Timestamp
  , pathNum         :: Word32
  }
  deriving (Eq, Show, Read, Generic)

instance Arbitrary OnionPath where
  arbitrary = OnionPath <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


isPathAlive :: Timestamp -> OnionPath -> Bool
isPathAlive now path =
  pathExpires path > now &&
  case pathLastAttempt path of
    Nothing -> True
    Just lastTime ->
      let timeout = if pathConfirmed path then confirmedPathTimeout else unconfirmedPathTimeout
          maxTries = if pathConfirmed path then 4 else 2
      in now `Time.diffTime` lastTime < timeout || pathTries path < maxTries


data OnionPathState = OnionPathState
  { announcePaths :: [OnionPath]
  , searchPaths   :: [OnionPath]
  , lastPathNum   :: Word32
  }
  deriving (Eq, Show, Read, Generic)

instance Arbitrary OnionPathState where
  arbitrary = OnionPathState <$> arbitrary <*> arbitrary <*> arbitrary


createPath :: MonadRandomBytes m => [NodeInfo] -> Word32 -> Timestamp -> m OnionPath
createPath nodes pNum expires = do
  keys <- replicateM 3 newKeyPair
  return $ OnionPath nodes keys False 0 expires Nothing pNum


-- | Maintain the set of onion paths.
maintainPaths :: OnionPathMonad m => [NodeInfo] -> m ()
maintainPaths nodes = maintainPathsVia nodes nodes

-- | Maintain the paths, with first nodes from the first list.  Without UDP
-- these are the TCP relays we are connected to.
maintainPathsVia :: OnionPathMonad m => [NodeInfo] -> [NodeInfo] -> m ()
maintainPathsVia firstNodes nodes = do
  now <- askTime
  -- Filter out expired or failed paths
  modify $ \OnionPathState{..} -> OnionPathState
    { announcePaths = filter (isPathAlive now) announcePaths
    , searchPaths   = filter (isPathAlive now) searchPaths
    , ..
    }

  -- Fill up announce paths
  numAnnounce <- gets (length . announcePaths)
  replicateM_ (maxPaths - numAnnounce) $ do
    pNodes <- pickNodesVia firstNodes nodes
    unless (null pNodes) $ do
      pNum <- gets lastPathNum
      p <- createPath pNodes (pNum + 1) (now `Time.addTime` pathLifetime)
      modify $ \OnionPathState{..} -> OnionPathState
        { announcePaths = p : announcePaths
        , lastPathNum = pNum + 1
        , ..
        }

  -- Fill up search paths
  numSearch <- gets (length . searchPaths)
  replicateM_ (maxPaths - numSearch) $ do
    pNodes <- pickNodesVia firstNodes nodes
    unless (null pNodes) $ do
      pNum <- gets lastPathNum
      p <- createPath pNodes (pNum + 1) (now `Time.addTime` pathLifetime)
      modify $ \OnionPathState{..} -> OnionPathState
        { searchPaths = p : searchPaths
        , lastPathNum = pNum + 1
        , ..
        }

-- | Select a random path for sending.
selectPath :: OnionPathMonad m => Bool -> m (Maybe OnionPath)
selectPath forAnnounce = do
  paths <- gets (if forAnnounce then announcePaths else searchPaths)
  MonadRandomBytes.uniformSafe paths

-- | The path with the given number, if it is still in use.
findPath :: OnionPathMonad m => Word32 -> m (Maybe OnionPath)
findPath num = gets $ \s -> find ((== num) . pathNum) (announcePaths s ++ searchPaths s)

-- | Update the path with the given number.
modifyPath :: OnionPathMonad m => Word32 -> (OnionPath -> OnionPath) -> m ()
modifyPath num f = modify $ \OnionPathState{..} -> OnionPathState
  { announcePaths = map update announcePaths
  , searchPaths   = map update searchPaths
  , ..
  }
  where
    update p = if pathNum p == num then f p else p

-- | A request was sent through the path.
markPathUsed :: OnionPathMonad m => Word32 -> m ()
markPathUsed num = do
  now <- askTime
  modifyPath num $ \p -> p { pathTries = pathTries p + 1, pathLastAttempt = Just now }

-- | A response came back through the path.
markPathConfirmed :: OnionPathMonad m => Word32 -> m ()
markPathConfirmed num = modifyPath num $ \p -> p { pathConfirmed = True, pathTries = 0 }


-- | Pick 3 distinct random nodes for a path. Returns empty list if not enough nodes.
pickNodes :: MonadRandomBytes m => [NodeInfo] -> m [NodeInfo]
pickNodes = pickDistinct 3

-- | Pick the given number of distinct random nodes, or none if there are fewer.
pickDistinct :: MonadRandomBytes m => Int -> [NodeInfo] -> m [NodeInfo]
pickDistinct count nodes
  | length distinct < count = return []
  | otherwise = go count distinct
  where
    distinct = nubBy ((==) `on` NodeInfo.publicKey) nodes
    go 0 _ = return []
    go n candidates = do
      node <- uniform candidates
      (node :) <$> go (n - 1) (filter ((/= NodeInfo.publicKey node) . NodeInfo.publicKey) candidates)


-- | Pick a first node from the first list and 2 more distinct nodes.
pickNodesVia :: MonadRandomBytes m => [NodeInfo] -> [NodeInfo] -> m [NodeInfo]
pickNodesVia firstNodes nodes
  | null firstNodes = return []
  | otherwise = do
      first <- uniform firstNodes
      rest <- pickDistinct 2 (filter ((/= NodeInfo.publicKey first) . NodeInfo.publicKey) nodes)
      return $ if null rest then [] else first : rest

-- | Wrap data into a nested Onion Request payload, each layer encrypted with
-- the path's key for that node.
wrapPath :: Keyed m
         => OnionPath      -- ^ The path to follow (A, B, C)
         -> SocketAddress  -- ^ Final destination Node D
         -> Nonce          -- ^ Nonce for all layers
         -> BS.ByteString  -- ^ Final raw payload for Node D (e.g. 0x83 envelope)
         -> m Tunnel.OnionRequest0
wrapPath path destAddr nonce innerData = case (pathNodes path, pathKeys path) of
  (nodeA : _, kp1 : _) -> wrapLayers path destAddr nonce innerData >>= Tunnel.wrapOnion0 kp1 (NodeInfo.publicKey nodeA) nonce
  _ -> error "wrapPath: OnionPath must have exactly 3 nodes and 3 keys"

-- | The contents of the first node's layer: what Node A decrypts.  A TCP
-- relay as first node gets this over its encrypted connection.
wrapLayers :: Keyed m => OnionPath -> SocketAddress -> Nonce -> BS.ByteString -> m Tunnel.OnionRequestPayload
wrapLayers path destAddr nonce innerData =
  case (pathNodes path, pathKeys path) of
    ([_, nodeB, nodeC], [_, kp2, kp3]) -> do
      -- Layer 3: Encrypted with kp3 for Node C (nodeC)
      -- Decrypted by C to find D and the raw data.
      let p3 = Tunnel.OnionRequestPayloadFinal (Tunnel.OnionIPPort destAddr) innerData
      combined3 <- Keyed.getCombinedKey (KeyPair.secretKey kp3) (NodeInfo.publicKey nodeC)
      let enc3 = Box.encrypt combined3 nonce (Box.encode p3)

      -- Layer 2: Encrypted with kp2 for Node B (nodeB)
      -- Decrypted by B to find C and Layer 3.
      let p2 = Tunnel.OnionRequestPayload (Tunnel.OnionIPPort (NodeInfo.address nodeC)) (KeyPair.publicKey kp3) enc3
      combined2 <- Keyed.getCombinedKey (secretKey kp2) (NodeInfo.publicKey nodeB)
      let enc2 = Box.encrypt combined2 nonce (Box.encode p2)

      -- Layer 1: For Node A, to find B and Layer 2.
      return $ Tunnel.OnionRequestPayload (Tunnel.OnionIPPort (NodeInfo.address nodeB)) (KeyPair.publicKey kp2) enc2
    _ -> error "wrapLayers: OnionPath must have exactly 3 nodes and 3 keys"
\end{code}

In order to announce itself using onion announce packets toxcore first takes
DHT peers, picks random ones and builds onion paths with them by saving 3
nodes, calling it a path, generating some keypairs for encrypting the onion
packets and using them to send onion packets.  If the peer is only connected
with TCP, the initial nodes will be bootstrap nodes and connected TCP relays
(for the first peer in the path).  Once the peer is connected to the onion he
can fill up his list of known peers with peers sent in announce responses if
needed.

Onion paths have different timeouts depending on whether the path is confirmed
or unconfirmed.  Unconfirmed paths (paths that core has never received any
responses from) have a timeout of 4 seconds with 2 tries before they are deemed
non working.  This is because, due to network conditions, there may be a large
number of newly created paths that do not work and so trying them a lot would
make finding a working path take much longer.  The timeout for a confirmed path
(from which a response was received) is 10 seconds with 4 tries without a
response.  A confirmed path has a maximum lifetime of 1200 seconds to make
possible deanonimization attacks more difficult.

Toxcore saves a maximum of 12 paths: 6 paths are reserved for announcing
ourselves and 6 others are used to search for friends.  This may not be the
safest way (some nodes may be able to associate friends together) however it is
much more performant than having different paths for each friend.  The main
benefit is that the announcing and searching are done with different paths,
which makes it difficult to know that peer with real public key X is friends
with Y and Z.  More research is needed to find the best way to do this.  At
first toxcore did have different paths for each friend, however, that meant
that each friend path was almost never used (and checked).  When using a low
amount of paths for searching there is less resources needed to find good
paths.  6 paths are used because 4 was too low and caused some performance
issues because it took longer to find some good paths at the beginning because
only 4 could be tried at a time.  A too high number meanwhile would mean each
path is used (and tested) less.  The reason why the numbers are the same for
both types of paths is for code simplification purposes.

To search/announce itself to peers, toxcore keeps the 8 closest peers (12 for
announcing) to each key it is searching (or announcing itself to).  To
populate these it starts by sending announce requests to random peers for all
the public keys it is searching for.  It then recursively searches closer and
closer peers (DHT distance function) until it no longer finds any.  It is
important to make sure it is not too aggressive at searching the peers as some
might no longer be online but peers might still send announce responses with
their information. Toxcore keeps lists of last pinged nodes for each key
searched so as not to ping dead nodes too aggressively.

Toxcore decides if it will send an announce packet to one of the 4 peers in the
announce response by checking if the peer would be stored as one of the stored
closest peers if it responded; if it would not be it doesn't send an announce
request, if it would be it sends one.

Peers are only put in the closest peers array if they respond to an announce
request.  If the peers fail to respond to 3 announce requests they are deemed
timed out and removed.  When sending an announce request to a peer to which we
have been announcing ourselves for at least 90 seconds and which has failed to
respond to the previous 2 requests, toxcore uses a random path for the request.
This reduces the chances that a good node will be removed due to bad paths.

The reason for the numbers of peers being 8 and 12 is that lower numbers might
make searching for and announcing too unreliable and a higher number too
bandwidth/resource intensive.

Toxcore uses \texttt{ping\_array} (see \texttt{ping\_array}) for the 8 byte
sendback data in announce packets to store information that it will need to
handle the response (key to decrypt it, why was it sent? (to announce ourselves
or to search? For what key? and some other info)).  For security purposes it
checks to make sure the packet was received from the right ip/port and checks
if the key in the unencrypted part of the packet is the right public key.

For peers we are announcing ourselves to, if we are not announced to them
toxcore tries every 3 seconds to announce ourselves to them until they return
that we have announced ourselves to them, then initially toxcore sends an
announce request packet every 15 seconds to see if we are still announced and
reannounce ourselves at the same time.  Toxcore sends every announce packet
with the \texttt{ping\_id} previously received from that peer with the same
path (if possible).  Toxcore use a timeout of 120 seconds rather than 15
seconds if we have been announcing to the peer for at least 90 seconds, and
the onion path we are are using for the peer has also been alive for at least
90 seconds, and we have not been waiting for at least 15 seconds for a
response to a request sent to the peer, nor for at least 10 seconds for a
response to a request sent via the path. The timeout of at most 120 seconds
means a \texttt{ping\_id} received in the last packet will not have had time
to expire (300 second minimum timeout) before it is resent 120 seconds later.

For friends this is slightly different.  It is important to start searching for
friends after we are fully announced.  Assuming a perfect network, we would
only need to do a search for friend public keys only when first starting the
instance (or going offline and back online) as peers starting up after us would
be able to find us immediately just by searching for us.  If we start searching
for friends after we are announced we prevent a scenario where 2 friends start
their clients at the same time but are unable to find each other right away
because they start searching for each other while they have not announced
themselves.

For this reason, after the peer is announced successfully, for 17 seconds
announce packets are sent aggressively every 3 seconds to each known close peer
(in the list of 8 peers) to search aggressively for peers that know the peer we
are searching for.

After this, toxcore sends requests once per 15 seconds initially, then
uses linear backoff to increase the interval.  In detail, the interval used
when searching for a given friend is at least 15 and at most 2400 seconds, and
within these bounds is calculated as one quarter of the time since we began
searching for the friend, or since the friend was last seen. For this purpose,
a friend is considered to be seen when some peer reports that the friend is
announced, or we receive a DHT Public Key packet from the friend, or we obtain
a new DHT key for them from a group, or a friend connection for the friend
goes offline.

There are other ways this could be done and which would still work but, if
making your own implementation, keep in mind that these are likely not the most
optimized way to do things.

If we find peers (more than 1) that know a friend we will send them an onion
data packet with our DHT public key, up to 2 TCP relays we are connected to and
2 DHT peers close to us to help the friend connect back to us.

Onion data packets are packets sent as the data of data to route packets.

Onion data packets:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{32}        & Long term public key of sender \\
  variable           & Payload \\
\end{tabular}

The payload is encrypted with long term private key of the sender, the long
term public key of the receiver and the nonce used in the data to route request
packet used to send this onion data packet (shaves off 24 bytes).

DHT public key packet:

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x9c) \\
  \texttt{8}         & \texttt{uint64\_t} \texttt{no\_replay} \\
  \texttt{32}        & Our DHT public key \\
  \texttt{[39, 204]} & Maximum of 4 nodes in packed format \\
\end{tabular}

The packet will only be accepted if the \texttt{no\_replay} number is greater
than the \texttt{no\_replay} number in the last packet received.

The nodes sent in the packet comprise 2 TCP relays to which we are
connected (or fewer if there are not 2 available) and a number of DHT nodes
from our Close List, with the total number of nodes sent being at most 4. The
nodes chosen from the Close List are those closest in DHT distance to us. This
allows the friend to find us more easily in the DHT, or to connect to us via a
TCP relay.

Why another round of encryption? We have to prove to the receiver that we own
the long term public key we say we own when sending them our DHT public key.
Friend requests are also sent using onion data packets but their exact format
is explained in Messenger.

The \texttt{no\_replay} number is protection if someone tries to replay an older
packet and should be set to an always increasing number.  It is 8 bytes so you
should set a high resolution monotonic time as the value.

We send this packet every 30 seconds if there is more than one peer (in the 8)
that says they our friend is announced on them.  This packet can also be sent
through the DHT module as a DHT request packet (see DHT) if we know the DHT
public key of the friend and are looking for them in the DHT but have not
connected to them yet.  30 second is a reasonable timeout to not flood the
network with too many packets while making sure the other will eventually
receive the packet.  Since packets are sent through every peer that knows the
friend, resending it right away without waiting has a high likelihood of
failure as the chances of packet loss happening to all (up to to 8) packets
sent is low.

When sent as a DHT request packet (this is the data sent in the DHT request
packet):

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x9c) \\
  \texttt{32}        & Long term public key of sender \\
  \texttt{24}        & Nonce \\
  variable           & Encrypted payload \\
\end{tabular}

The payload is encrypted with long term private key of sender, the long term
public key of receiver and the nonce, and contains the DHT public key packet.

When sent as a DHT request packet the DHT public key packet is (before being
sent as the data of a DHT request packet) encrypted with the long term keys of
both the sender and receiver and put in that format.  This is done for the same
reason as the double encryption of the onion data packet.

Toxcore tries to resend this packet through the DHT every 20 seconds.  20
seconds is a reasonable resend rate which isn't too aggressive.

Toxcore has a DHT request packet handler that passes received DHT public key
packets from the DHT module to this module.

If we receive a DHT public key packet, we will first check if the DHT packet is
from a friend, if it is not from a friend, it will be discarded.  The
\texttt{no\_replay} will then be checked to see if it is good and no packet with
a lower one was received during the session.  The DHT key, the TCP nodes in the
packed nodes and the DHT nodes in the packed nodes will be passed to their
relevant modules.  The fact that we have the DHT public key of a friend means
this module has achieved its goal.

If a friend is online and connected to us, the onion will stop all of its
actions for that friend.  If the peer goes offline it will restart searching
for the friend as if toxcore was just started.

If toxcore goes offline (no onion traffic for 75 seconds) toxcore will
aggressively reannounce itself and search for friends as if it was just
started.
