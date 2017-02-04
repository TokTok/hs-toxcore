\section{DHT Operation}

\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
module Network.Tox.DHT.Operation where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad                 (guard, when, unless)
import           Control.Monad.Random          (RandT, evalRandT)
import           Control.Monad.Random.Class    (MonadRandom, uniform)
import           Control.Monad.State           (MonadState, execStateT, put, get, gets, modify)
import           Control.Monad.Writer          (MonadWriter, Writer, execWriter, execWriterT,
                                                runWriter, tell)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ask, runReaderT)
import           Data.Foldable                 (for_)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Traversable              (for, traverse)
import           System.Random                 (StdGen, mkStdGen, getStdGen)
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary, shrink)

import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.DHT.ClientList    (ClientList)
import qualified Network.Tox.DHT.ClientList    as ClientList
import           Network.Tox.DHT.ClientNode    (ClientNode)
import qualified Network.Tox.DHT.ClientNode    as ClientNode
import           Network.Tox.DHT.DhtState      (DhtState)
import qualified Network.Tox.DHT.DhtState      as DhtState
import           Network.Tox.DHT.NodeList      (NodeList)
import qualified Network.Tox.DHT.NodeList      as NodeList
import           Network.Tox.DHT.NodesResponse (NodesResponse(..))
import           Network.Tox.DHT.Stamped       (Stamped)
import qualified Network.Tox.DHT.Stamped       as Stamped
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo
import           Network.Tox.Time              (TimeDiff, Timestamp)
import qualified Network.Tox.Time              as Time


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

\subsection{Periodic sending of Nodes Requests}

For each Nodes List in the DHT State, every 20 seconds a Nodes Request is sent
to a random node on the list, searching for the base key of the list.

Random nodes are chosen since being able to predict which node a node will
send a request to next could make some attacks that disrupt the network
easier, as it adds a possible attack vector.

\begin{code}

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM = (put =<<) . (get >>=)

-- | Information required to send a NodesRequest packet
data RequestInfo = RequestInfo
  { requestTo     :: NodeInfo
  , requestSearch :: PublicKey
  }
  deriving (Eq, Read, Show)

randomRequestPeriod :: TimeDiff
randomRequestPeriod = Time.seconds 20

randomRequests ::
  ( MonadRandom m
  , MonadState DhtState m
  , MonadWriter [RequestInfo] m
  , MonadReader Timestamp m
  ) => m ()
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
      , MonadRandom m
      , MonadReader Timestamp m
      , MonadWriter [RequestInfo] m
      , MonadState Timestamp m) => l -> m ()
    doList nodeList = do
      time <- ask
      lastTime <- get
      when (time Time.- lastTime >= randomRequestPeriod) $
        case NodeList.nodeListList nodeList of
          [] -> put time
          nodes -> do
            node <- uniform nodes
            tell [RequestInfo node $ NodeList.baseKey nodeList]
            put time

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

\begin{code}

pingPeriod :: TimeDiff
pingPeriod = Time.seconds 60

maxPings :: Int
maxPings = 2

pingNodes :: forall m.
  ( MonadState DhtState m
  , MonadWriter [RequestInfo] m
  , MonadReader Timestamp m
  ) => m ()
pingNodes = modifyM $ DhtState.traverseClientLists pingNodes'
  where
    pingNodes' :: ClientList -> m ClientList
    pingNodes' clientList =
      (\x -> clientList{ ClientList.nodes = x }) <$>
        traverseMaybe pingNode (ClientList.nodes clientList)
      where
        traverseMaybe :: Applicative f =>
          (a -> f (Maybe b)) -> Map k a -> f (Map k b)
        traverseMaybe f = (Map.mapMaybe id <$>) . traverse f

        pingNode :: ClientNode -> m (Maybe ClientNode)
        pingNode clientNode = ask >>= \time ->
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

\end{code}

\subsection{Handling Nodes Response packets}
When a valid Nodes Response packet is received, it is first checked that a
Nodes Request was sent within the last 60 seconds to the node from which the
response was received; if not, the packet is ignored.

Otherwise, firstly the node from which the response was sent is added to the
state; see the k-Buckets and Client List specs for details on this operation.
Secondly, for each node listed in the response and for each Nodes List in the
DHT State to which the node is viable for entry, a Nodes Request is sent to the
node with the requested public key being the base key of the Nodes List.

\begin{code}

requireResponseWithin :: TimeDiff
requireResponseWithin = Time.seconds 60

handleNodesResponse ::
  ( MonadState DhtState m
  , MonadWriter [RequestInfo] m
  , MonadReader Timestamp m
  ) => NodeInfo -> [NodeInfo] -> m ()
handleNodesResponse responder nodes = ask >>= \time -> do
  isPending <- DhtState.pendingResponsesL $ do
    modify $ Stamped.dropOlder (time Time.+ negate requireResponseWithin)
    elem responder . Stamped.getList <$> get
  when isPending $ do
    modify $ DhtState.addNode time responder
    for_ nodes $ \node ->
      (>>= tell) $ (<$> get) $ DhtState.foldMapNodeLists $ \nodeList ->
        guard (NodeList.viable node nodeList) >>
          [ RequestInfo node $ NodeList.baseKey nodeList ]

\end{code}

An implementation may choose not to send every such Nodes Request.
(c-toxcore only sends only so many per list (8 for the Close List, 4 for a
Search Entry) per call to Do_DHT(), prioritising the closest to the base key).

\subsection{Handling Nodes Request packets}
On receiving a Nodes Request packet, the 4 nodes in the DHT State which are
closest to the public key in the packet are found, and sent back to the node
which sent the request in a Nodes Response packet. If there are fewer than 4
nodes in the state, just those nodes are sent. If there are no nodes in the
state, no response is sent.

\begin{code}

responseMaxNodes :: Int
responseMaxNodes = 4

handleNodesRequest :: PublicKey -> DhtState -> Maybe NodesResponse
handleNodesRequest publicKey dhtState =
  let nodes = DhtState.takeClosestNodesTo responseMaxNodes publicKey dhtState
  in if length nodes == 0 then Nothing else Just $ NodesResponse nodes

initDHT :: IO ()
initDHT = do
  -- TODO: bootstrap
  return ()

doDHT :: (MonadIO m, MonadState DhtState m) => m ()
doDHT =
  (liftIO Time.getTime >>=) . runReaderT $
    (liftIO getStdGen >>=) . evalRandT $
      (execWriterT $ randomRequests >> pingNodes) >>= mapM_ sendRequest

sendRequest :: MonadIO m => RequestInfo -> m ()
sendRequest _ = return () -- TODO

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

runTestOperation :: Monoid w => ArbStdGen -> RandT StdGen (Writer w) a -> (a,w)
runTestOperation seed = runWriter . (`evalRandT` getArbStdGen seed)
execTestOperation :: Monoid w => ArbStdGen -> RandT StdGen (Writer w) a -> w
execTestOperation seed = execWriter . (`evalRandT` getArbStdGen seed)

-- | wrap StdGen so the Arbitrary instance isn't an orphan
newtype ArbStdGen = ArbStdGen { getArbStdGen :: StdGen }
  deriving (Read, Show)

instance Arbitrary ArbStdGen
  where arbitrary = ArbStdGen . mkStdGen <$> arbitrary

\end{code}

