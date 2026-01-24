\begin{code}
{-# LANGUAGE StrictData #-}
module Tox.Transport.Stream
    ( Congestion (..)
    , initState
    , sampleInterval
    , congestionTimeout
    , minRate
    , minQueue
    , sample
    , refill
    , sentNew
    , resendBudget
    , resent
    ) where

import qualified Tox.Core.Time as Time
import           Tox.Core.Time (TimeDiff, Timestamp)

-- | How fast a connection may send lossless data that is subject to
-- congestion control, and what we measured to decide it.
data Congestion = Congestion
  { ccSendRate       :: Double -- ^ New packets a second.
  , ccResendRate     :: Double -- ^ Packets a second, resent ones included.
  , ccPacketsLeft    :: Int    -- ^ New packets we may send now.
  , ccResendsLeft    :: Int    -- ^ Packets we may resend now.
  , ccRefilled       :: Maybe Timestamp
  , ccRefillRest     :: Double
  , ccResendRefilled :: Maybe Timestamp
  , ccResendRest     :: Double
  , ccSent           :: Int    -- ^ New packets sent since the last sample.
  , ccResent         :: Int
  , ccQueueSizes     :: [Int]  -- ^ The send queue at the last samples, newest first.
  , ccSentHistory    :: [Int]  -- ^ 'ccSent' at the last samples, newest first.
  , ccResentHistory  :: [Int]
  , ccLastCongestion :: Maybe Timestamp
  } deriving (Eq, Show)

-- | toxcore's CRYPTO_PACKET_MIN_RATE and CRYPTO_MIN_QUEUE_LENGTH.
minRate :: Double
minRate = 4

minQueue :: Int
minQueue = 64

-- | How often 'sample' runs.
sampleInterval :: TimeDiff
sampleInterval = Time.milliseconds 50

-- | How many samples the rates are taken over, and how many we keep to look
-- a round trip back.
queueSamples, sentSamples :: Int
queueSamples = 12
sentSamples = 2 * queueSamples

-- | After a congestion event, the rate stops growing for this long.
congestionTimeout :: TimeDiff
congestionTimeout = Time.seconds 1

-- | A send queue this many seconds long at the measured rate slows us down.
sendQueueRatio :: Double
sendQueueRatio = 2

initState :: Congestion
initState = Congestion
  { ccSendRate       = minRate
  , ccResendRate     = minRate
  , ccPacketsLeft    = minQueue
  , ccResendsLeft    = minQueue
  , ccRefilled       = Nothing
  , ccRefillRest     = 0
  , ccResendRefilled = Nothing
  , ccResendRest     = 0
  , ccSent           = 0
  , ccResent         = 0
  , ccQueueSizes     = replicate queueSamples 0
  , ccSentHistory    = replicate sentSamples 0
  , ccResentHistory  = replicate sentSamples 0
  , ccLastCongestion = Nothing
  }
\end{code}

toxcore no longer uses the formula described at the end of this section, and
nor do we.  Every 50 milliseconds it takes how many packets it sent over 12
such intervals, one round trip ago, less what the send queue grew by in that
time.  It then sends 20\% faster than that, or 10\% slower within a second of
a congestion event, or slower still while the send queue holds more than two
seconds of packets.  New packets and resent ones each get a budget at their
rate, which 'refill' tops up.

\begin{code}
-- | Take a sample: the round trip time, the send queue length, and whether
-- the connection just moved from TCP to UDP, in which case the rates stay.
sample :: TimeDiff -> Int -> Bool -> Timestamp -> Congestion -> Congestion
sample rtt queue justDirect now cc
  | justDirect = recorded
  | otherwise = recorded
      { ccSendRate   = max minRate rate
      , ccResendRate = max (max minRate rate) (resendSpeed * 1.2)
      }
  where
    recorded = cc
      { ccSent          = 0
      , ccResent        = 0
      , ccQueueSizes    = take queueSamples (queue : ccQueueSizes cc)
      , ccSentHistory   = take sentSamples (ccSent cc : ccSentHistory cc)
      , ccResentHistory = take sentSamples (ccResent cc : ccResentHistory cc)
      }
    growth = queue - last (ccQueueSizes recorded)
    millis = fromIntegral . Time.toMilliseconds :: TimeDiff -> Double
    delay = min (sentSamples - queueSamples) . roundUp $ millis rtt / millis sampleInterval
    -- As toxcore counts round its circular arrays, so a long delay wraps
    -- round to the newest sample.
    window history = sum [ history !! ((delay + 1 + j) `mod` sentSamples) | j <- [0 .. queueSamples - 1] ]
    sent0 = window $ ccSentHistory recorded
    resent0 = window $ ccResentHistory recorded
    (sent', resent')
      | growth > 0 = (sent0 - growth, resent0)
      | otherwise = (sent0, min resent0 (negate growth))
    perSecond n = 1000 * fromIntegral n / (fromIntegral queueSamples * millis sampleInterval)
    speed = max minRate (perSecond sent')
    resendSpeed = perSecond (sent' + resent')
    ratio = fromIntegral queue / speed
    congested = not $ Time.due now congestionTimeout (ccLastCongestion cc)
    rate
      | ratio > sendQueueRatio && queue > minQueue = speed / (ratio / sendQueueRatio)
      | congested = speed * 0.9
      | otherwise = speed * 1.2

-- | Top the budgets up for the time that passed.  The budget for new packets
-- holds at most a little more than four refills.
refill :: Timestamp -> Congestion -> Congestion
refill now cc = case (ccRefilled cc, ccResendRefilled cc) of
  (Just since, Just resendSince) ->
    let cc' = case topUp (ccSendRate cc) since (ccRefillRest cc) of
          Just (n, rest) -> cc
            { ccPacketsLeft = if ccPacketsLeft cc > 4 * n + minQueue then 4 * n + minQueue else ccPacketsLeft cc + n
            , ccRefilled = Just now, ccRefillRest = rest }
          Nothing -> cc
        cc'' = case topUp (ccResendRate cc) resendSince (ccResendRest cc) of
          Just (n, rest) -> cc' { ccResendsLeft = n, ccResendRefilled = Just now, ccResendRest = rest }
          Nothing -> cc'
    in cc'' { ccResendsLeft = max (ccResendsLeft cc'') (ccPacketsLeft cc'') }
  _ -> cc { ccRefilled = Just now, ccResendRefilled = Just now, ccPacketsLeft = minQueue, ccResendsLeft = minQueue }
  where
    -- Once a packet's worth of time has passed, the whole packets earned
    -- and the fraction left over.
    topUp rate since rest
      | Time.addTime since (Time.milliseconds . roundUp $ 1000 / rate) <= now =
          let elapsed = fromIntegral (Time.timestampToMicroseconds now - Time.timestampToMicroseconds since) / 1000000
              earned = rate * elapsed + rest
              whole = floor earned :: Int
          in Just (whole, earned - fromIntegral whole)
      | otherwise = Nothing

-- | Halves round up, as toxcore's casts after adding 0.5 do.
roundUp :: Integral a => Double -> a
roundUp x = floor (x + 0.5)

-- | Packets that go out past the budget are owed, so the budget can go
-- below zero.
sentNew :: Congestion -> Congestion
sentNew cc = cc
  { ccPacketsLeft = ccPacketsLeft cc - 1
  , ccResendsLeft = max 0 (ccResendsLeft cc - 1)
  , ccSent = ccSent cc + 1
  }

resendBudget :: Congestion -> Int
resendBudget = ccResendsLeft

-- | Packets went out again.  Using up the budget for new packets is a
-- congestion event.
resent :: Timestamp -> Int -> Congestion -> Congestion
resent now n cc
  | n < ccPacketsLeft cc = counted { ccPacketsLeft = ccPacketsLeft cc - n }
  | otherwise = counted { ccPacketsLeft = 0, ccLastCongestion = Just now }
  where
    counted = cc { ccResendsLeft = max 0 (ccResendsLeft cc - n), ccResent = ccResent cc + n }
\end{code}

The ping or rtt (round trip time) between two peers can be calculated by saving
the time each packet was sent and taking the difference between the time the
latest packet confirmed received by a request packet was sent and the time the
request packet was received.  The rtt can be calculated for every request
packet.  The lowest one (for all packets) will be the closest to the real ping.

This ping or rtt can be used to know if a request packet that requests a packet
we just sent should be resent right away or we should wait or not for the next
one (to know if the other side actually had time to receive the packet).

The congestion control algorithm has the goal of guessing how many packets can
be sent through the link every second before none can be sent through anymore.
How it works is basically to send packets faster and faster until none can go
through the link and then stop sending them faster than that.

Currently the congestion control uses the following formula in toxcore however
that is probably not the best way to do it.

The current formula is to take the difference between the current size of the
send queue and the size of the send queue 1.2 seconds ago, take the total
number of packets sent in the last 1.2 seconds and subtract the previous number
from it.

Then divide this number by 1.2 to get a packet speed per second.  If this speed
is lower than the minimum send rate of 8 packets per second, set it to 8.

A congestion event can be defined as an event when the number of requested
packets exceeds the number of packets the congestion control says can be sent
during this frame.  If a congestion event occurred during the last 2 seconds,
the packet send rate of the connection is set to the send rate previously
calculated, if not it is set to that send rate times 1.25 in order to increase
the speed.

Like I said this isn't perfect and a better solution can likely be found or the
numbers tweaked.

To fix the possible issue where it would be impossible to send very low
bandwidth data like text messages when sending high bandwidth data like files
it is possible to make priority packets ignore the congestion control
completely by placing them into the send packet queue and sending them even if
the congestion control says not to.  This is used in toxcore for all non file
transfer packets to prevent file transfers from preventing normal message
packets from being sent.
