\subsection{Replies to RPC requests}
A \textit{reply} to a Request packet is a Response packet with the Request ID in
the Response packet set equal to the Request ID in the Request packet.  A
response is accepted if and only if it is the first received reply to a request
which was sent sufficiently recently, according to a time limit which depends on
the service.

\begin{code}
{-# LANGUAGE Safe       #-}
{-# LANGUAGE StrictData #-}
module Network.Tox.DHT.PendingReplies where

import qualified Network.Tox.DHT.RpcPacket     as RpcPacket
import           Network.Tox.DHT.Stamped       (Stamped)
import qualified Network.Tox.DHT.Stamped       as Stamped
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import           Network.Tox.Time              (Timestamp)

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

type PendingReplies = Stamped (NodeInfo, RpcPacket.RequestId)

expectReply :: Timestamp -> NodeInfo -> RpcPacket.RequestId ->
  PendingReplies -> PendingReplies
expectReply time node requestId = Stamped.add time (node, requestId)

checkExpectedReply :: Timestamp -> NodeInfo -> RpcPacket.RequestId ->
  PendingReplies -> (Bool, PendingReplies)
checkExpectedReply cutoff node requestId pendingReplies =
  case filter (>= cutoff) $
    Stamped.findStamps (== (node, requestId)) pendingReplies
  of
    []     -> (False, pendingReplies)
    time:_ -> (True, Stamped.delete time (node, requestId) pendingReplies)

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}

\end{code}
