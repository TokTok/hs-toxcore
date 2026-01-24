module Tox.Application.ToxSpec (spec) where

import qualified Network.Socket      as Socket
import           Test.Hspec

import qualified Tox.Application.Tox as Tox

spec :: Spec
spec =
  describe "Tox" $ do
    it "fails calls once killed, rather than waiting for ever" $ do
      tox <- Tox.new Tox.defaultOptions { Tox.optLocalDiscovery = False } >>= either (fail . show) return
      _ <- Tox.selfGetName tox
      Tox.kill tox
      Tox.selfGetName tox `shouldThrow` (== Tox.ToxStopped)

    it "reports a taken relay port and lets go of its UDP port" $ do
      taken <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.bind taken (Socket.SockAddrInet 0 0)
      Socket.listen taken 1
      tcpPort <- fromIntegral <$> Socket.socketPort taken
      probe <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
      Socket.bind probe (Socket.SockAddrInet 0 0)
      udpPort <- fromIntegral <$> Socket.socketPort probe
      Socket.close probe
      let opts = Tox.defaultOptions { Tox.optLocalDiscovery = False, Tox.optUdpPorts = (udpPort, udpPort) }
      refused <- Tox.new opts { Tox.optTcpPort = Just tcpPort }
      Socket.close taken
      either Just (const Nothing) refused `shouldBe` Just Tox.PortInUse
      Tox.new opts >>= either (fail . show) Tox.kill
