\section{DHT Operation}

\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Network.Tox.DHT.Operation where

import           Control.Applicative                  (Applicative, pure, (*>),
                                                       (<$>), (<*>))
import           Control.Monad                        (guard, msum, replicateM,
                                                       unless, void, when)
import           Control.Monad.Identity               (Identity, runIdentity)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Random                 (RandT, evalRandT)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       ask, runReaderT)
import           Control.Monad.State                  (MonadState, State,
                                                       StateT, evalStateT,
                                                       execStateT, get, gets,
                                                       modify, put)
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.Writer                 (MonadWriter, Writer,
                                                       WriterT, execWriter,
                                                       execWriterT, runWriter,
                                                       tell)
import           Data.Binary                          (Binary)
import           Data.Foldable                        (for_)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import Data.Maybe (isNothing)
import           Data.Traversable                     (for, traverse)
import           System.Random                        (StdGen, getStdGen,
                                                       mkStdGen)
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary,
                                                       shrink)

import           Network.Tox.Crypto.Key               (Nonce, PublicKey)
import qualified Network.Tox.Crypto.KeyPair           as KeyPair
import           Network.Tox.DHT.ClientList           (ClientList)
import qualified Network.Tox.DHT.ClientList           as ClientList
import           Network.Tox.DHT.ClientNode           (ClientNode)
import qualified Network.Tox.DHT.ClientNode           as ClientNode
import           Network.Tox.DHT.DhtPacket            (DhtPacket (..))
import qualified Network.Tox.DHT.DhtPacket            as DhtPacket
import           Network.Tox.DHT.DhtRequestPacket     (DhtRequestPacket (..))
import qualified Network.Tox.DHT.DhtRequestPacket     as DhtRequestPacket
import           Network.Tox.DHT.DhtState             (DhtState)
import qualified Network.Tox.DHT.DhtState             as DhtState
import           Network.Tox.DHT.NodeList             (NodeList)
import qualified Network.Tox.DHT.NodeList             as NodeList
import           Network.Tox.DHT.NodesRequest         (NodesRequest (..))
import           Network.Tox.DHT.NodesResponse        (NodesResponse (..))
import           Network.Tox.DHT.RpcPacket            (RpcPacket (..))
import qualified Network.Tox.DHT.RpcPacket            as RpcPacket
import           Network.Tox.DHT.Stamped              (Stamped)
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
  Networked.sendPacket to . Packet kind $
    DhtPacket.encode keyPair (NodeInfo.publicKey to) nonce

sendRequest :: DhtNodeMonad m => RequestInfo -> m ()
sendRequest (RequestInfo to key) = do
  requestID <- RpcPacket.RequestId <$> MonadRandomBytes.randomWord64
  time <- Timed.askTime
  DhtState.pendingResponsesL . modify $ Stamped.add time (to, requestID)
  sendDhtPacket to PacketKind.NodesRequest $
    RpcPacket (NodesRequest key) requestID

sendResponse :: Networked m => NodeInfo -> [NodeInfo] -> m ()
sendResponse ::
  DhtNodeMonad m => NodeInfo -> RpcPacket.RequestId -> [NodeInfo] -> m ()
sendResponse to requestID nodes =
  sendDhtPacket to PacketKind.NodesResponse $
    RpcPacket (NodesResponse nodes) requestID

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM = (put =<<) . (get >>=)

\end{code}

\subsection{Periodic sending of Nodes Requests}

For each Nodes List in the DHT State, every 20 seconds a Nodes Request is sent
to a random node on the list, searching for the base key of the list.

When a Nodes List first becomes populated with nodes, 5 such random Nodes
Requests are sent in quick succession.

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
  DhtState.dhtCloseListStampL $ doList closeList
  DhtState.dhtSearchListL .
    modifyM . traverse . execStateT $ do
      searchList <- gets DhtState.searchClientList
      DhtState.searchStampL $ doList searchList
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

Furthermore, for each Nodes List in the DHT State, each node on the list is
sent a Nodes Request every 60 seconds, searching for the base key of the list.

Nodes from which we consistently fail to receive Nodes Responses should be
removed from the DHT State.

c-toxcore's implementation of pinging and timeouts:
A Last Pinged time is maintained for each node in each list. When a node is
added to a list, if doing so evicts a node from the list then the Last Pinged
time is set to that of the evicted node, and otherwise it is set to 0.  Nodes
from which we have not received a Nodes Response for 122 seconds are considered
Bad; they remain in the DHT State, but are preferentially overwritten when
adding to the DHT State, and are ignored for all operations except the
once-per-60s pinging described above. If we have not received a Nodes Response
for 182 seconds, the node is not even pinged. So one ping is sent after the node
becomes Bad. In the special case that every node in the Close List is Bad, they
are all pinged once more.)

hs-toxcore implementation of pinging and timeouts:
For each node in the Dht State, a Last Pinged timestamp and a Pings Counter are
maintained.  Nodes are added with these set to the current time and 0,
respectively.  This includes updating an already present node.  The DHT State
nodes are passed through periodically, and for each which is due a ping, we:
ping it, update the timestamp, increment the counter, and, if the counter is
then 2 (configurable constant), remove the node from the list. This is pretty
close to the behaviour of c-toxcore, but much simpler.
TODO: currently it doesn't do anything to try to recover if the Close List
becomes empty. We could maintain a separate list of the most recently heard from
nodes, and repopulate the Close List with that if the Close List becomes empty.

\begin{code}

pingPeriod :: TimeDiff
pingPeriod = Time.seconds 60

maxPings :: Int
maxPings = 2

pingNodes :: forall m. DhtNodeMonad m => WriterT [RequestInfo] m ()
pingNodes = modifyM $ DhtState.traverseClientLists pingNodes'
  where
    pingNodes' :: ClientList -> WriterT [RequestInfo] m ClientList
    pingNodes' clientList =
      (\x -> clientList{ ClientList.nodes = x }) <$>
        traverseMaybe pingNode (ClientList.nodes clientList)
      where
        traverseMaybe :: Applicative f =>
          (a -> f (Maybe b)) -> Map k a -> f (Map k b)
        traverseMaybe f = (Map.mapMaybe id <$>) . traverse f

        pingNode :: ClientNode -> WriterT [RequestInfo] m (Maybe ClientNode)
        pingNode clientNode = Timed.askTime >>= \time ->
          if time Time.- lastPing < pingPeriod
          then pure $ Just clientNode
          else (tell [requestInfo] *>) . pure $
            if pingCount + 1 < maxPings
            then Just $ clientNode
              { ClientNode.lastPing = time
              , ClientNode.pingCount = pingCount + 1
              }
            else Nothing
          where
            nodeInfo = ClientNode.nodeInfo clientNode
            lastPing = ClientNode.lastPing clientNode
            pingCount = ClientNode.pingCount clientNode
            requestInfo = RequestInfo nodeInfo $ NodeList.baseKey clientList

doDHT :: DhtNodeMonad m => m ()
doDHT =
  execWriterT (randomRequests >> pingNodes) >>= mapM_ sendRequest


\end{code}

\subsection{Handling Nodes Response packets}
When a valid Nodes Response packet is received, it is first checked that a
Nodes Request was sent within the last 60 seconds to the node from which the
response was received, and that the Request ID on the received RpcPacket is that
sent with the Nodes Request. If not, the packet is ignored.

Otherwise, firstly the node from which the response was sent is added to the
state; see the k-Buckets and Client List specs for details on this operation.
Secondly, for each node listed in the response and for each Nodes List in the
DHT State which does not currently contain the node and to which the node is
viable for entry, a Nodes Request is sent to the node with the requested public
key being the base key of the Nodes List.

NOTE: c-toxcore actually checks for the node already being in the node list
only for search lists, not for the close list.
I think this should be considered a bug.

\begin{code}

requireResponseWithin :: TimeDiff
requireResponseWithin = Time.seconds 60

handleNodesResponse ::
  DhtNodeMonad m => NodeInfo -> RpcPacket NodesResponse -> m ()
handleNodesResponse from (RpcPacket (NodesResponse nodes) requestID) =
  Timed.askTime >>= \time -> do
    isPending <- DhtState.pendingResponsesL $ do
      modify $ Stamped.dropOlder (time Time.+ negate requireResponseWithin)
      elem (from, requestID) . Stamped.getList <$> get
    when isPending $ do
      modify $ DhtState.addNode time from
      for_ nodes $ \node ->
        (>>= mapM_ sendRequest) $ (<$> get) $ DhtState.foldMapNodeLists $
          \nodeList ->
            guard (isNothing (NodeList.lookupPublicKey
                (NodeInfo.publicKey node) nodeList)
              && NodeList.viable node nodeList) >>
            [ RequestInfo node $ NodeList.baseKey nodeList ]

\end{code}

An implementation may choose not to send every such Nodes Request.
(c-toxcore only sends only so many per list (8 for the Close List, 4 for a
Search Entry) per call to Do\_DHT(), prioritising the closest to the base key).

\subsection{Handling Nodes Request packets}
On receiving a Nodes Request packet, the 4 nodes in the DHT State which are
closest to the public key in the packet are found, and sent back to the node
which sent the request in a Nodes Response packet. If there are fewer than 4
nodes in the state, just those nodes are sent. If there are no nodes in the
state, no response is sent.

TODO: add\_to\_ping() and ping.c

\begin{code}

responseMaxNodes :: Int
responseMaxNodes = 4

handleNodesRequest ::
  DhtNodeMonad m => NodeInfo -> RpcPacket NodesRequest -> m ()
handleNodesRequest from (RpcPacket (NodesRequest key) requestID) = do
  nodes <- DhtState.takeClosestNodesTo responseMaxNodes key <$> get
  unless (null nodes) $ sendResponse from requestID nodes

\end{code}

\input{src/tox/Network/Tox/DHT/DhtRequestPacket.lhs}
\section{Handling DHT Request packets}

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
    [ MaybeT (DhtPacket.decode keyPair DhtPacket) >>= handleNatPingPacket
    , MaybeT (DhtPacket.decode keyPair DhtPacket) >>= handleDhtPKPacket
    ]
  else void . runMaybeT $ do
    node <- MaybeT $
      NodeList.lookupPublicKey addresseePublicKey =<< gets DhtState.dhtCloseList
    lift . Networked.sendPacket node . Packet PacketKind.Crypto $ packet

\end{code}

\subsection{NAT ping packets}

A NAT ping packet is sent as the payload of a DHT request packet. 

NAT ping packets are used to see if a friend we are not connected to directly
is online and ready to do the hole punching.

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

\section{DHT Initialisation}
A new DHT node is initialised with a DHT State with a fresh random key pair, an
empty close list, and a search list containing 2 empty search entries searching
for random public keys. 

\begin{code}

randomSearches :: Int
randomSearches = 2

initDHT :: (MonadRandomBytes m, Timed m) => m DhtState
initDHT = do
  dhtState <- DhtState.empty <$> Timed.askTime <*> MonadRandomBytes.newKeyPair
  time <- Timed.askTime
  (`execStateT` dhtState) $ replicateM randomSearches $ do
    publicKey <- MonadRandomBytes.randomKey
    DhtState.dhtSearchListL . modify . Map.insert publicKey $
      DhtState.emptySearchEntry time publicKey

bootstrapNode :: DhtNodeMonad m => NodeInfo -> m ()
bootstrapNode nodeInfo =
  sendRequest . RequestInfo nodeInfo =<<
    KeyPair.publicKey <$> gets DhtState.dhtKeyPair

-- TODO
--loadDHT :: ??

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

type TestDhtNodeMonad = Timed.TimedT (RandT StdGen (StateT DhtState (Networked.NetworkLogged Identity)))
instance DhtNodeMonad TestDhtNodeMonad

evalTestDhtNode :: ArbStdGen -> Timestamp -> DhtState -> TestDhtNodeMonad a -> a
evalTestDhtNode seed time s =
  runIdentity
    . Networked.evalNetworkLogged
    . (`evalStateT` s)
    . (`evalRandT` unwrapArbStdGen seed)
    . (`Timed.runTimedT` time)

-- | wrap StdGen so the Arbitrary instance isn't an orphan
newtype ArbStdGen = ArbStdGen { unwrapArbStdGen :: StdGen }
  deriving (Read, Show)

instance Arbitrary ArbStdGen
  where arbitrary = ArbStdGen . mkStdGen <$> arbitrary

\end{code}

