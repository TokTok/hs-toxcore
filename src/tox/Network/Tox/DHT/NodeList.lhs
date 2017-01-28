The Close List and the Search Entries are termed the \texttt{Node Lists} of
the DHT State.

\begin{code}
module Network.Tox.DHT.NodeList where

import           Control.Applicative           (Const (..), getConst)
import           Data.Monoid                   (Dual (..), Endo (..), Monoid,
                                                appEndo, getDual)

import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.DHT.ClientList    (ClientList)
import qualified Network.Tox.DHT.ClientList    as ClientList
import           Network.Tox.DHT.KBuckets      (KBuckets)
import qualified Network.Tox.DHT.KBuckets      as KBuckets
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo
import           Network.Tox.Time              (TimeStamp)

class NodeList l where
  addNode :: TimeStamp -> NodeInfo -> l -> l

  removeNode :: PublicKey -> l -> l

  viable :: NodeInfo -> l -> Bool

  baseKey :: l -> PublicKey

  traverseClientLists ::
    Applicative f => (ClientList -> f ClientList) -> l -> f l

  -- | copied from Data.Traversable.foldMapDefault
  foldMapClientLists :: Monoid m => (ClientList -> m) -> l -> m
  foldMapClientLists f = getConst . traverseClientLists (Const . f)

  -- | copied from Data.Foldable.foldl
  foldlClientLists :: (a -> ClientList -> a) -> a -> l -> a
  foldlClientLists f z t =
    appEndo (getDual (foldMapClientLists (Dual . Endo . flip f) t)) z

  nodeListList :: l -> [NodeInfo]
  nodeListList = foldMapClientLists ClientList.nodeInfos

  foldNodes :: (a -> NodeInfo -> a) -> a -> l -> a
  foldNodes = foldlClientLists . ClientList.foldNodes


instance NodeList ClientList where
  addNode = ClientList.addNode
  removeNode = ClientList.removeNode
  viable = ClientList.viable
  baseKey = ClientList.baseKey
  traverseClientLists = id

instance NodeList KBuckets where
  addNode = KBuckets.addNode
  removeNode = KBuckets.removeNode
  viable = KBuckets.viable
  baseKey = KBuckets.baseKey
  traverseClientLists = KBuckets.traverseClientLists
\end{code}
