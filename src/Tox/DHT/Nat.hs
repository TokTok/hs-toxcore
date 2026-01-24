{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Reaching searched nodes through the nodes close to them, and punching
-- holes through their NATs, as toxcore's DHT does.
module Tox.DHT.Nat
    ( friendAddresses
    , routeToFriend
    , dhtRequest
    , handleNatPing
    , doNat
    , packetIdNatPing
    ) where

import           Control.Monad                      (forM_, unless, when)
import           Control.Monad.Logger               (logDebugN)
import qualified Data.Binary                        as Binary
import           Data.Binary.Put                    (putWord64le, runPut)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.List                          (sortOn)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust)
import           Data.Ord                           (Down (..))
import           Data.Text                          (pack)
import           Data.Word                          (Word16, Word32, Word8)

import qualified Tox.Core.Time                      as Time
import           Tox.Core.Time                      (TimeDiff, Timestamp)
import           Tox.Core.Timed                     (Timed (..))
import           Tox.Crypto.Core.Box                (PlainText (..))
import           Tox.Crypto.Core.Key                (PublicKey)
import qualified Tox.Crypto.Core.MonadRandomBytes   as MonadRandomBytes
import           Tox.Crypto.Core.MonadRandomBytes   (randomNonce, randomWord64)
import qualified Tox.DHT.ClientList                 as ClientList
import qualified Tox.DHT.DhtPacket                  as DhtPacket
import           Tox.DHT.DhtRequestPacket           (DhtRequestPacket (..))
import qualified Tox.DHT.DhtState                   as DhtState
import           Tox.DHT.DhtState                   (DhtSearchEntry (..),
                                                     NatState (..))
import           Tox.DHT.Operation                  (DhtNodeMonad, getDht,
                                                     modifyDht, sendPingRequest)
import           Tox.Network.Core.HostAddress       (HostAddress)
import           Tox.Network.Core.Networked         (Networked (..))
import           Tox.Network.Core.NodeInfo          (NodeInfo (..))
import           Tox.Network.Core.Packet            (Packet (..),
                                                     RawPayload (..))
import qualified Tox.Network.Core.PacketKind        as PacketKind
import           Tox.Network.Core.PortNumber        (PortNumber (..))
import           Tox.Network.Core.SocketAddress     (SocketAddress (..))
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))

packetIdNatPing, natPingRequest, natPingResponse :: Word8
packetIdNatPing = 0xfe
natPingRequest  = 0
natPingResponse = 1

maxFriendClients, maxPunchingPorts, maxNormalPunchingTries :: Int
maxFriendClients       = 8
maxPunchingPorts       = 48
maxNormalPunchingTries = 5

-- | How long a node's report of where a searched node is stays good.
badNodeTimeout :: TimeDiff
badNodeTimeout = Time.seconds 122

punchInterval, punchResetTime :: TimeDiff
punchInterval  = Time.seconds 3
punchResetTime = Time.seconds 40

within :: Timestamp -> TimeDiff -> Maybe Timestamp -> Bool
within now d = maybe False (\t -> now <= Time.addTime t d)

-- | Nodes close to the searched node and where they say it is.  'Nothing'
-- if we reach it ourselves.
reports :: Timestamp -> DhtSearchEntry -> Maybe [(NodeInfo, SocketAddress)]
reports now e
  | isJust (searchNode e) = Nothing
  | otherwise = Just
      [ (client, addr)
      | client <- ClientList.nodeInfos (searchClientList e)
      , Just (addr, t) <- [Map.lookup (publicKey client) (searchReturned e)]
      , within now badNodeTimeout (Just t)
      ]

friendAddresses :: Timestamp -> DhtSearchEntry -> Maybe [SocketAddress]
friendAddresses now e = map snd <$> reports now e

-- | Send a packet to the nodes that say they know the searched node, which
-- pass it on to it: how many we sent it to.
routeToFriend :: DhtNodeMonad m => PublicKey -> Packet RawPayload -> m Int
routeToFriend pk packet = do
  now <- askTime
  entry <- Map.lookup pk . DhtState.dhtSearchList <$> getDht
  case entry >>= reports now of
    Just rs | length rs >= maxFriendClients `div` 4 -> do
      mapM_ ((`sendPacket` packet) . fst) rs
      return $ length rs
    _ -> return 0

-- | Send a packet through one random node that knows the searched node.
routeOneToFriend :: DhtNodeMonad m => PublicKey -> Packet RawPayload -> m ()
routeOneToFriend pk packet = do
  now <- askTime
  entry <- Map.lookup pk . DhtState.dhtSearchList <$> getDht
  case maybe [] (map fst) (entry >>= reports now) of
    [] -> return ()
    rs -> do
      i <- MonadRandomBytes.randomInt (length rs)
      sendPacket (rs !! i) packet

-- | A DHT Request packet to a node, which nodes close to it pass on.
dhtRequest :: DhtNodeMonad m => PublicKey -> BS.ByteString -> m (Packet RawPayload)
dhtRequest to plain = do
  keyPair <- DhtState.dhtKeyPair <$> getDht
  nonce <- randomNonce
  pkt <- DhtPacket.encryptKeyed keyPair to nonce (PlainText plain)
  return . Packet PacketKind.Crypto . RawPayload . LBS.toStrict . Binary.encode $ DhtRequestPacket to pkt

natPing :: Word8 -> BS.ByteString -> BS.ByteString
natPing ty pingId = BS.pack [packetIdNatPing, ty] <> pingId

encodePingId :: NatState -> BS.ByteString
encodePingId = LBS.toStrict . runPut . putWord64le . natPingId

modifyNat :: DhtNodeMonad m => PublicKey -> (NatState -> NatState) -> m ()
modifyNat pk f = modifyDht $ \s -> s
  { DhtState.dhtSearchList = Map.adjust (\e -> e { searchNat = f (searchNat e) }) pk (DhtState.dhtSearchList s) }

-- | A NAT ping from a searched node: answer requests, and start punching
-- when it answers ours.
handleNatPing :: DhtNodeMonad m => PublicKey -> BS.ByteString -> m ()
handleNatPing from bytes = do
  entry <- Map.lookup from . DhtState.dhtSearchList <$> getDht
  forM_ entry $ \e -> case BS.unpack (BS.take 1 bytes) of
    [ty] | BS.length bytes == 9 -> do
      let pingId = BS.drop 1 bytes
      now <- askTime
      if | ty == natPingRequest -> do
             dhtRequest from (natPing natPingResponse pingId) >>= routeOneToFriend from
             modifyNat from $ \n -> n { natPingRecv = Just now }
         | ty == natPingResponse && pingId == encodePingId (searchNat e) -> do
             newId <- randomWord64
             logDebugN $ "NAT ping answered by " <> pack (show from)
             modifyNat from $ \n -> n { natPingId = newId, natPunching = True }
         | otherwise -> return ()
    _ -> return ()

-- | Periodic work: NAT ping searched nodes we only know of through others,
-- and punch holes towards the ones that answer.
doNat :: DhtNodeMonad m => m ()
doNat = do
  now <- askTime
  entries <- Map.toList . DhtState.dhtSearchList <$> getDht
  forM_ entries $ \(pk, e) -> case friendAddresses now e of
    Just addrs | length addrs >= maxFriendClients `div` 2 -> do
      let nat = searchNat e
      unless (within now punchInterval (natPingSent nat)) $ do
        sent <- dhtRequest pk (natPing natPingRequest (encodePingId nat)) >>= routeToFriend pk
        when (sent > 0) $ modifyNat pk $ \n -> n { natPingSent = Just now }
      when (natPunching nat && not (within now punchInterval (natPunched nat))
            && within now (2 * punchInterval) (natPingRecv nat)) $
        forM_ (commonHost addrs) $ \host -> do
          let reset = not $ within now punchResetTime (natPunched nat)
              nat' = if reset then nat { natTries = 0, natPortIndex = 0, natLowPortIndex = 0 } else nat
              ports = [ p | SocketAddress h (PortNumber p) <- addrs, h == host ]
              (targets, nat'') = punchTargets ports nat'
          logDebugN $ "Punching holes to " <> pack (show pk) <> " at " <> pack (show host) <> " ports " <> pack (show targets)
          mapM_ (\port -> sendPingRequest $ NodeInfo UDP (SocketAddress host (PortNumber port)) pk) targets
          modifyNat pk $ \n -> nat'' { natPingSent = natPingSent n, natPunched = Just now, natPunching = False }
    _ -> return ()

-- | The host most nodes report, if at least half of them do.
commonHost :: [SocketAddress] -> Maybe HostAddress
commonHost addrs = case sortOn (Down . snd) counts of
  (host, n) : _ | n >= maxFriendClients `div` 2 -> Just host
  _                                             -> Nothing
  where
    hosts = [ h | SocketAddress h _ <- addrs ]
    counts = Map.toList $ Map.fromListWith (+) [ (h, 1 :: Int) | h <- hosts ]

-- | Ports to ping: the one port if all reports agree, else guesses around the
-- reported ones, and after a few tries also a sweep from port 1024.
punchTargets :: [Word16] -> NatState -> ([Word16], NatState)
punchTargets [] nat = ([], nat)
punchTargets ports@(first : _) nat = (normal ++ low, nat'')
  where
    numPorts = fromIntegral (length ports) :: Word32
    (normal, nat')
      | all (== first) ports = ([first], nat)
      | otherwise =
          ( [ guess (fromIntegral i + natPortIndex nat) | i <- [0 .. maxPunchingPorts - 1] ]
          , nat { natPortIndex = natPortIndex nat + fromIntegral maxPunchingPorts } )
    guess it =
      let delta = (if odd it then negate else id) (it `div` (2 * numPorts))
      in (ports !! fromIntegral ((it `div` 2) `mod` numPorts)) + fromIntegral delta
    (low, nat'')
      | natTries nat' > maxNormalPunchingTries =
          ( [ 1024 + fromIntegral (fromIntegral i + natLowPortIndex nat') | i <- [0 .. maxPunchingPorts - 1] ]
          , nat' { natLowPortIndex = natLowPortIndex nat' + fromIntegral (maxPunchingPorts `div` 2), natTries = natTries nat' + 1 } )
      | otherwise = ([], nat' { natTries = natTries nat' + 1 })
