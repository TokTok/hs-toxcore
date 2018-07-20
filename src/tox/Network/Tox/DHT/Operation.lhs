\section{DHT Operation}

\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Network.Tox.DHT.Operation where

import           Control.Applicative                  (Applicative, pure, (<$>),
                                                       (<*>))
import           Control.Monad                        (guard, msum, replicateM,
                                                       unless, void, when)
import           Control.Monad.Identity               (Identity, runIdentity)
import           Control.Monad.Random                 (RandT, evalRandT)
import           Control.Monad.State                  (MonadState, StateT,
                                                       execStateT, get, gets,
                                                       modify, put, runStateT)
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.Writer                 (MonadWriter, WriterT,
                                                       execWriterT, tell)
import           Data.Binary                          (Binary)
import           Data.Foldable                        (for_)
import           Data.Functor                         (($>))
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe                           (isNothing)
import           Data.Traversable                     (traverse)
import           Lens.Family2                         (Lens')
import           Lens.Family2.State                   (zoom, (%%=), (%=))
import           System.Random                        (StdGen, mkStdGen)
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary)

import           Network.Tox.Crypto.Key               (PublicKey)
import           Network.Tox.Crypto.Keyed             (Keyed)
import           Network.Tox.Crypto.KeyedT            (KeyedT)
import qualified Network.Tox.Crypto.KeyedT            as KeyedT
import qualified Network.Tox.Crypto.KeyPair           as KeyPair
import           Network.Tox.DHT.ClientList           (ClientList)
import qualified Network.Tox.DHT.ClientList           as ClientList
import           Network.Tox.DHT.ClientNode           (ClientNode)
import qualified Network.Tox.DHT.ClientNode           as ClientNode
import qualified Network.Tox.DHT.DhtPacket            as DhtPacket
import           Network.Tox.DHT.DhtRequestPacket     (DhtRequestPacket (..))
import           Network.Tox.DHT.DhtState             (DhtState)
import qualified Network.Tox.DHT.DhtState             as DhtState
import           Network.Tox.DHT.NodeList             (NodeList)
import qualified Network.Tox.DHT.NodeList             as NodeList
import           Network.Tox.DHT.NodesRequest         (NodesRequest (..))
import           Network.Tox.DHT.NodesResponse        (NodesResponse (..))
import qualified Network.Tox.DHT.PendingReplies       as PendingReplies
import           Network.Tox.DHT.PingPacket           (PingPacket (..))
import           Network.Tox.DHT.RpcPacket            (RpcPacket (..))
import qualified Network.Tox.DHT.RpcPacket            as RpcPacket
import qualified Network.Tox.DHT.Stamped              as Stamped
import           Network.Tox.Network.MonadRandomBytes (MonadRandomBytes)
import qualified Network.Tox.Network.MonadRandomBytes as MonadRandomBytes
import           Network.Tox.Network.Networked        (Networked)
import qualified Network.Tox.Network.Networked        as Networked
import           Network.Tox.NodeInfo.NodeInfo        (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo        as NodeInfo
import           Network.Tox.Protocol.Packet          (Packet (..))
import           Network.Tox.Protocol.PacketKind      (PacketKind)
import qualified Network.Tox.Protocol.PacketKind      as PacketKind
import           Network.Tox.Time                     (TimeDiff, Timestamp)
import qualified Network.Tox.Time                     as Time
import           Network.Tox.Timed                    (Timed)
import qualified Network.Tox.Timed                    as Timed
import           Network.Tox.TimedT                   (TimedT)
import qualified Network.Tox.TimedT                   as TimedT


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

class
  ( Networked m
  , Timed m
  , MonadRandomBytes m
  , MonadState DhtState m
  , Keyed m
  ) => DhtNodeMonad m where {}

data RequestInfo = RequestInfo
  { requestTo     :: NodeInfo
  , requestSearch :: PublicKey
  }
  deriving (Eq, Read, Show)

sendDhtPacket :: (DhtNodeMonad m, Binary payload) =>
  NodeInfo -> PacketKind -> payload -> m ()
sendDhtPacket to kind payload = do
  keyPair <- gets DhtState.dhtKeyPair
  nonce <- MonadRandomBytes.randomNonce
  Networked.sendPacket to . Packet kind =<<
    DhtPacket.encodeKeyed keyPair (NodeInfo.publicKey to) nonce payload

sendRpcRequest :: (DhtNodeMonad m, Binary payload) =>
  NodeInfo -> PacketKind -> payload -> m ()
sendRpcRequest to packetKind payload = do
  requestId <- RpcPacket.RequestId <$> MonadRandomBytes.randomWord64
  time <- Timed.askTime
  DhtState._dhtPendingReplies %= PendingReplies.expectReply time to requestId
  sendDhtPacket to packetKind $
    RpcPacket payload requestId

sendNodesRequest :: DhtNodeMonad m => RequestInfo -> m ()
sendNodesRequest (RequestInfo to key) =
  sendRpcRequest to PacketKind.NodesRequest $ NodesRequest key

sendNodesResponse ::
  DhtNodeMonad m => NodeInfo -> RpcPacket.RequestId -> [NodeInfo] -> m ()
sendNodesResponse to requestId nodes =
  sendDhtPacket to PacketKind.NodesResponse $
    RpcPacket (NodesResponse nodes) requestId

sendPingRequest :: DhtNodeMonad m => NodeInfo -> m ()
sendPingRequest to =
  sendRpcRequest to PacketKind.PingRequest PingRequest

sendPingResponse ::
  DhtNodeMonad m => NodeInfo -> RpcPacket.RequestId -> m ()
sendPingResponse to requestId =
  sendDhtPacket to PacketKind.PingResponse $
    RpcPacket PingResponse requestId

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM = (put =<<) . (get >>=)

-- | adapted from michaelt's lens-simple:
-- zoom_ is like zoom but for convenience returns an mtl style
-- abstracted MonadState state, rather than a concrete StateT, recapturing
-- a bit more of the abstractness of Control.Lens.zoom
zoom_ :: MonadState s' m => Lens' s' s -> StateT s m a -> m a
-- full signature:
-- zoom_ :: MonadState s' m =>
--   LensLike' (Zooming m a) s' s -> StateT s m a -> m a
zoom_ l f = abstract $ zoom l f
  where
    abstract :: MonadState s m => StateT s m a -> m a
    abstract st = do
      (a,s') <- runStateT st =<< get
      put s'
      return a

\end{code}

\subsection{DHT Initialisation}
A new DHT node is initialised with a DHT State with a fresh random key pair, an
empty close list, and a search list containing 2 empty search entries searching
for randomly generated public keys.

\begin{code}

initRandomSearches :: Int
initRandomSearches = 2

initDht :: (MonadRandomBytes m, Timed m) => m DhtState
initDht = do
  dhtState <- DhtState.empty <$> Timed.askTime <*> MonadRandomBytes.newKeyPair
  time <- Timed.askTime
  (`execStateT` dhtState) $ replicateM initRandomSearches $ do
    publicKey <- MonadRandomBytes.randomKey
    DhtState._dhtSearchList %=
      Map.insert publicKey (DhtState.emptySearchEntry time publicKey)

bootstrapNode :: DhtNodeMonad m => NodeInfo -> m ()
bootstrapNode nodeInfo =
  sendNodesRequest . RequestInfo nodeInfo =<<
    KeyPair.publicKey <$> gets DhtState.dhtKeyPair

-- TODO
--loadDHT :: ??

\end{code}

\subsection{Periodic sending of Nodes Requests}
For each Nodes List in the DHT State, every 20 seconds we send a Nodes Request
to a random node on the list, searching for the base key of the list.

When a Nodes List first becomes populated with nodes, we send 5 such random
Nodes Requests in quick succession.

Random nodes are chosen since being able to predict which node a node will
send a request to next could make some attacks that disrupt the network
easier, as it adds a possible attack vector.

\begin{code}

randomRequestPeriod :: TimeDiff
randomRequestPeriod = Time.seconds 20

maxBootstrapTimes :: Int
maxBootstrapTimes = 5

randomRequests :: DhtNodeMonad m => WriterT [RequestInfo] m ()
randomRequests = do
  closeList <- gets DhtState.dhtCloseList
  zoom_ DhtState._dhtCloseListStamp $ doList closeList
  zoom_ DhtState._dhtSearchList .
    modifyM . traverse . execStateT $ do
      searchList <- gets DhtState.searchClientList
      zoom_ DhtState._searchStamp $ doList searchList
  where
    doList ::
      ( NodeList l
      , Timed m
      , MonadRandomBytes m
      , MonadState DhtState.ListStamp m
      , MonadWriter [RequestInfo] m
      ) => l -> m ()
    doList nodeList =
      case NodeList.nodeListList nodeList of
        [] -> return ()
        nodes -> do
          time <- Timed.askTime
          DhtState.ListStamp lastTime bootstrapped <- get
          when (time Time.- lastTime >= randomRequestPeriod
              || bootstrapped < maxBootstrapTimes) $ do
            node <- MonadRandomBytes.uniform nodes
            tell [RequestInfo node $ NodeList.baseKey nodeList]
            put $ DhtState.ListStamp time (bootstrapped + 1)

\end{code}

Furthermore, we periodically check every node for responsiveness by sending it a
Nodes Request: for each Nodes List in the DHT State, we send each node on the
list a Nodes Request every 60 seconds, searching for the base key of the list.
We remove from the DHT State any node from which we persistently fail to receive
Nodes Responses.

c-toxcore's implementation of checking and timeouts:
A Last Checked time is maintained for each node in each list. When a node is
added to a list, if doing so evicts a node from the list then the Last Checked
time is set to that of the evicted node, and otherwise it is set to 0. This
includes updating an already present node. Nodes from which we have not
received a Nodes Response for 122 seconds are considered Bad; they remain in the
DHT State, but are preferentially overwritten when adding to the DHT State, and
are ignored for all operations except the once-per-60s checking described above.
If we have not received a Nodes Response for 182 seconds, the node is not even
checked. So one check is sent after the node becomes Bad. In the special case
that every node in the Close List is Bad, they are all checked once more.)

hs-toxcore implementation of checking and timeouts:
We maintain a Last Checked timestamp and a Checks Counter on each node on each
Nodes List in the Dht State. When a node is added to a list, these are set
respectively to the current time and to 0. This includes updating an already
present node. We periodically pass through the nodes on the lists, and for each
which is due a check, we: check it, update the timestamp, increment the counter,
and, if the counter is then 2, remove the node from the list. This is pretty
close to the behaviour of c-toxcore, but much simpler. TODO: currently hs-toxcore
doesn't do anything to try to recover if the Close List becomes empty. We could
maintain a separate list of the most recently heard from nodes, and repopulate
the Close List with that if the Close List becomes empty.

\begin{code}

checkPeriod :: TimeDiff
checkPeriod = Time.seconds 60

maxChecks :: Int
maxChecks = 2

checkNodes :: forall m. DhtNodeMonad m => WriterT [RequestInfo] m ()
checkNodes = modifyM $ DhtState.traverseClientLists checkNodes'
  where
    checkNodes' :: ClientList -> WriterT [RequestInfo] m ClientList
    checkNodes' clientList =
      (\x -> clientList{ ClientList.nodes = x }) <$>
        traverseMaybe checkNode (ClientList.nodes clientList)
      where
        traverseMaybe :: Applicative f =>
          (a -> f (Maybe b)) -> Map k a -> f (Map k b)
        traverseMaybe f = (Map.mapMaybe id <$>) . traverse f

        checkNode :: ClientNode -> WriterT [RequestInfo] m (Maybe ClientNode)
        checkNode clientNode = Timed.askTime >>= \time ->
          if time Time.- lastCheck < checkPeriod
          then pure $ Just clientNode
          else (tell [requestInfo] $>) $
            if checkCount + 1 < maxChecks
            then Just $ clientNode
              { ClientNode.lastCheck = time
              , ClientNode.checkCount = checkCount + 1
              }
            else Nothing
          where
            nodeInfo = ClientNode.nodeInfo clientNode
            lastCheck = ClientNode.lastCheck clientNode
            checkCount = ClientNode.checkCount clientNode
            requestInfo = RequestInfo nodeInfo $ NodeList.baseKey clientList

doDHT :: DhtNodeMonad m => m ()
doDHT =
  execWriterT (randomRequests >> checkNodes) >>= mapM_ sendNodesRequest


\end{code}

\subsection{Handling Nodes Response packets}
When we receive a valid Nodes Response packet, we first check that it is a reply
to a Nodes Request which we sent within the last 60 seconds to the node from
which we received the response, and that no previous reply has been received. If
this check fails, the packet is ignored. If the check succeeds, first we add to
the DHT State the node from which the response was sent. Then, for each node
listed in the response and for each Nodes List in the DHT State which does not
currently contain the node and to which the node is viable for entry, we send a
Nodes Request to the node with the requested public key being the base key of
the Nodes List.

An implementation may choose not to send every such Nodes Request.
(c-toxcore only sends so many per list (8 for the Close List, 4 for a Search
Entry) per 50ms, prioritising the closest to the base key).

\begin{code}

requireNodesResponseWithin :: TimeDiff
requireNodesResponseWithin = Time.seconds 60

handleNodesResponse ::
  DhtNodeMonad m => NodeInfo -> RpcPacket NodesResponse -> m ()
handleNodesResponse from (RpcPacket (NodesResponse nodes) requestId) = do
  isReply <- checkPending requireNodesResponseWithin from requestId
  when isReply $ do
    time <- Timed.askTime
    modify $ DhtState.addNode time from
    for_ nodes $ \node ->
      (>>= mapM_ sendNodesRequest) $ (<$> get) $ DhtState.foldMapNodeLists $
        \nodeList ->
          guard (isNothing (NodeList.lookupPublicKey
              (NodeInfo.publicKey node) nodeList)
            && NodeList.viable node nodeList) >>
          [ RequestInfo node $ NodeList.baseKey nodeList ]

\end{code}

\subsection{Handling Nodes Request packets}
When we receive a Nodes Request packet from another node, we reply with a Nodes
Response packet containing the 4 nodes in the DHT State which are the closest to
the public key in the packet. If there are fewer than 4 nodes in the state, we
reply with all the nodes in the state. If there are no nodes in the state, no
reply is sent.

We also send a Ping Request when this is appropriate; see below.

\begin{code}

responseMaxNodes :: Int
responseMaxNodes = 4

handleNodesRequest ::
  DhtNodeMonad m => NodeInfo -> RpcPacket NodesRequest -> m ()
handleNodesRequest from (RpcPacket (NodesRequest key) requestId) = do
  ourPublicKey <- gets $ KeyPair.publicKey . DhtState.dhtKeyPair
  when (ourPublicKey /= NodeInfo.publicKey from) $ do
    nodes <- gets (DhtState.takeClosestNodesTo responseMaxNodes key)
    unless (null nodes) $ sendNodesResponse from requestId nodes
    sendPingRequestIfAppropriate from

\end{code}

\subsection{Handling Ping Request packets}
When a valid Ping Request packet is received, we reply with a Ping Response.

We also send a Ping Request when this is appropriate; see below.

\begin{code}

handlePingRequest ::
  DhtNodeMonad m => NodeInfo -> RpcPacket PingPacket -> m ()
handlePingRequest from (RpcPacket PingRequest requestId) = do
  sendPingResponse from requestId
  sendPingRequestIfAppropriate from
handlePingRequest _ _ = return ()

\end{code}

\subsection{Handling Ping Response packets}
When we receive a valid Ping Response packet, we first check that it is a reply
to a Ping Request which we sent within the last 5 seconds to the node from
which we received the response, and that no previous reply has been received. If
this check fails, the packet is ignored. If the check succeeds, we add to the
DHT State the node from which the response was sent.

\begin{code}

requirePingResponseWithin :: TimeDiff
requirePingResponseWithin = Time.seconds 5

maxPendingTime :: TimeDiff
maxPendingTime = maximum
  [ requireNodesResponseWithin
  , requirePingResponseWithin
  ]

checkPending :: DhtNodeMonad m =>
  TimeDiff -> NodeInfo -> RpcPacket.RequestId -> m Bool
checkPending timeLimit from requestId = do
  oldTime <- (Time.+ negate maxPendingTime) <$> Timed.askTime
  DhtState._dhtPendingReplies %= Stamped.dropOlder oldTime
  recentCutoff <- (Time.+ negate timeLimit) <$> Timed.askTime
  DhtState._dhtPendingReplies %%=
    PendingReplies.checkExpectedReply recentCutoff from requestId

handlePingResponse ::
  DhtNodeMonad m => NodeInfo -> RpcPacket PingPacket -> m ()
handlePingResponse from (RpcPacket PingResponse requestId) = do
  isReply <- checkPending requirePingResponseWithin from requestId
  ourPublicKey <- gets $ KeyPair.publicKey . DhtState.dhtKeyPair
  when (isReply && ourPublicKey /= NodeInfo.publicKey from) $ do
    time <- Timed.askTime
    modify $ DhtState.addNode time from
handlePingResponse _ _ = return ()

\end{code}

\subsection{Sending Ping Requests}
When we receive a Nodes Request or a Ping Request, in addition to the handling
described above, we sometimes send a Ping Request.
Namely, we send a Ping Request to the node which sent the packet if the node is
viable for entry to the Close List and is not already in the Close List.
An implementation may (TODO: should?) choose not to send every such Ping
Request.
(c-toxcore sends at most 32 every 2 seconds, preferring closer nodes.)

\begin{code}

sendPingRequestIfAppropriate :: DhtNodeMonad m => NodeInfo -> m ()
sendPingRequestIfAppropriate from = do
  closeList <- gets DhtState.dhtCloseList
  when
    (isNothing (NodeList.lookupPublicKey (NodeInfo.publicKey from) closeList)
      && NodeList.viable from closeList) $
    sendPingRequest from

\end{code}

\input{src/tox/Network/Tox/DHT/DhtRequestPacket.lhs}
\subsection{Handling DHT Request packets}

A DHT node that receives a DHT request packet checks whether the addressee
public key is their DHT public key. If it is, they will decrypt and handle
the packet.  Otherwise, they will check whether the addressee DHT public key
is the DHT public key of one of the nodes in their Close List.  If it isn't,
they will drop the packet.  If it is they will resend the packet, unaltered, to
that DHT node.

DHT request packets are used for DHT public key packets (see
\href{#onion}{onion}) and NAT ping packets.

\begin{code}

handleDhtRequestPacket :: DhtNodeMonad m => NodeInfo -> DhtRequestPacket -> m ()
handleDhtRequestPacket _from packet@(DhtRequestPacket addresseePublicKey dhtPacket) = do
  keyPair <- gets DhtState.dhtKeyPair
  if addresseePublicKey == KeyPair.publicKey keyPair
  then void . runMaybeT $ msum
    [ (MaybeT $ DhtPacket.decodeKeyed keyPair dhtPacket) >>= lift . handleNatPingPacket
    , (MaybeT $ DhtPacket.decodeKeyed keyPair dhtPacket) >>= lift . handleDhtPKPacket
    ]
  else void . runMaybeT $ do
    node :: NodeInfo <- MaybeT $
      NodeList.lookupPublicKey addresseePublicKey <$> gets DhtState.dhtCloseList
    lift . Networked.sendPacket node . Packet PacketKind.Crypto $ packet

\end{code}

\subsection{NAT ping packets}

A NAT ping packet is sent as the payload of a DHT request packet.

We use NAT ping packets to see if a friend we are not connected to directly is
online and ready to do the hole punching.

\subsubsection{NAT ping request}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0xfe) \\
  \texttt{1}         & \texttt{uint8\_t} (0x00) \\
  \texttt{8}         & \texttt{uint64\_t} random number \\
\end{tabular}

\subsubsection{NAT ping response}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0xfe) \\
  \texttt{1}         & \texttt{uint8\_t} (0x01) \\
  \texttt{8}         & \texttt{uint64\_t} random number (the same that was received in request) \\
\end{tabular}

TODO: handling these packets.

\begin{code}

-- | TODO
type NatPingPacket = ()
handleNatPingPacket :: DhtNodeMonad m => NatPingPacket -> m ()
handleNatPingPacket _ = return ()

-- | TODO
type DhtPKPacket = ()
handleDhtPKPacket :: DhtNodeMonad m => DhtPKPacket -> m ()
handleDhtPKPacket _ = return ()

\end{code}

\subsection{Effects of chosen constants on performance}
If the bucket size of the k-buckets were increased, it would increase the
amount of packets needed to check if each node is still alive, which would
increase the bandwidth usage, but reliability would go up.  If the number of
nodes were decreased, reliability would go down along with bandwidth usage.
The reason for this relationship between reliability and number of nodes is
that if we assume that not every node has its UDP ports open or is behind a
cone NAT it means that each of these nodes must be able to store a certain
number of nodes behind restrictive NATs in order for others to be able to find
those nodes behind restrictive NATs.  For example if 7/8 nodes were behind
restrictive NATs, using 8 nodes would not be enough because the chances of
some of these nodes being impossible to find in the network would be too high.

TODO(zugz): this seems a rather wasteful solution to this problem.

If the ping timeouts and delays between pings were higher it would decrease the
bandwidth usage but increase the amount of disconnected nodes that are still
being stored in the lists.  Decreasing these delays would do the opposite.

If the maximum size 8 of the DHT Search Entry Client Lists were increased
would increase the bandwidth usage, might increase hole punching efficiency on
symmetric NATs (more ports to guess from, see Hole punching) and might increase
the reliability.  Lowering this number would have the opposite effect.

The timeouts and number of nodes in lists for toxcore were picked by feeling
alone and are probably not the best values.  This also applies to the behavior
which is simple and should be improved in order to make the network resist
better to sybil attacks.

TODO: consider giving min and max values for the constants.

\begin{code}

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

type TestDhtNodeMonad = KeyedT (TimedT (RandT StdGen (StateT DhtState (Networked.NetworkLogged Identity))))
instance DhtNodeMonad TestDhtNodeMonad

runTestDhtNode :: ArbStdGen -> Timestamp -> DhtState -> TestDhtNodeMonad a -> (a, DhtState)
runTestDhtNode seed time s =
  runIdentity
    . Networked.evalNetworkLogged
    . (`runStateT` s)
    . (`evalRandT` unwrapArbStdGen seed)
    . (`TimedT.runTimedT` time)
    . (`KeyedT.evalKeyedT` Map.empty)

evalTestDhtNode :: ArbStdGen -> Timestamp -> DhtState -> TestDhtNodeMonad a -> a
evalTestDhtNode seed time s = fst . runTestDhtNode seed time s
execTestDhtNode :: ArbStdGen -> Timestamp -> DhtState -> TestDhtNodeMonad a -> DhtState
execTestDhtNode seed time s = snd . runTestDhtNode seed time s

initTestDhtState :: ArbStdGen -> Timestamp -> DhtState
initTestDhtState seed time =
  runIdentity
    . (`evalRandT` unwrapArbStdGen seed)
    . (`TimedT.runTimedT` time)
    $ initDht

-- | wrap StdGen so the Arbitrary instance isn't an orphan
newtype ArbStdGen = ArbStdGen { unwrapArbStdGen :: StdGen }
  deriving (Read, Show)

instance Arbitrary ArbStdGen
  where arbitrary = ArbStdGen . mkStdGen <$> arbitrary

\end{code}

