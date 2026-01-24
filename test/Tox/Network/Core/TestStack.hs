-- | The monad stack of pure tests of networked code: keys, a fixed clock,
-- seeded randomness, no logging, a state, and the packets sent.
module Tox.Network.Core.TestStack
    ( TestStack
    , runTestStack
    ) where

import           Control.Monad.Logger       (NoLoggingT, runNoLoggingT)
import           Control.Monad.Random       (RandT, evalRandT)
import           Control.Monad.State        (StateT, runStateT)
import           Data.Functor.Identity      (Identity (..))
import qualified Data.Map                   as Map
import           System.Random              (StdGen, mkStdGen)

import           Tox.Core.Time              (Timestamp)
import           Tox.Crypto.Core.Keyed      (KeyedT, evalKeyedT)
import           Tox.Network.Core.Networked (NetworkAction, NetworkLogged,
                                             runNetworkLogged)
import           Tox.Network.Core.TimedT    (TimedT, runTimedT)

type TestStack s = KeyedT (TimedT (RandT StdGen (NoLoggingT (StateT s (NetworkLogged Identity)))))

-- | Run an action with a random seed, the time and a state: its result, the
-- new state and what it sent.
runTestStack :: Int -> Timestamp -> s -> TestStack s a -> (a, s, [NetworkAction])
runTestStack seed now s m =
  let ((a, s'), actions) = runIdentity . runNetworkLogged . (`runStateT` s) . runNoLoggingT
        . (`evalRandT` mkStdGen seed) . (`runTimedT` now) $ evalKeyedT m Map.empty
  in (a, s', actions)
