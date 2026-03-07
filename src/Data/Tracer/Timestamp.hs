{- |
Module      : Data.Tracer.Timestamp
Description : Timestamped event wrapper and tracer
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a wrapper to pair events with their timestamps,
and tracer transformers that automatically add timestamps
to events.

Two clock sources are supported:

* 'utcTimestampTracer' — wall-clock time via
  'getCurrentTime'
* 'monotonicTimestampTracer' — monotonic nanoseconds via
  'getMonotonicTimeNSec' (no NTP jumps, microsecond
  precision)
-}
module Data.Tracer.Timestamp
    ( Timestamped (..)
    , utcTimestampTracer
    , monotonicTimestampTracer
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Tracer.Internal (mkTracer)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

{- | A wrapper that pairs an event with its timestamp.

The time type @t@ is determined by the clock source:

* @'UTCTime'@ for wall-clock timestamps
* @'Word64'@ (nanoseconds) for monotonic timestamps

@
event <- Timestamped \<$\> getCurrentTime \<*\> pure myEvent
traceWith tracer event
@
-}
data Timestamped t a = Timestamped
    { timestampedTime :: t
    -- ^ when the event occurred
    , timestampedEvent :: a
    -- ^ the event payload
    }
    deriving (Show, Eq)

{- | Wrap events with wall-clock timestamps.

Uses 'getCurrentTime' which has microsecond precision
but may jump due to NTP adjustments.

@
let tracer = utcTimestampTracer downstream
traceWith tracer \"Hello\"
@
-}
utcTimestampTracer
    :: Tracer IO (Timestamped UTCTime a)
    -> Tracer IO a
utcTimestampTracer downstream = mkTracer $ \event -> do
    time <- getCurrentTime
    traceWith downstream $ Timestamped time event

{- | Wrap events with monotonic timestamps.

Uses 'getMonotonicTimeNSec' which returns nanoseconds
since an arbitrary epoch. Monotonically increasing,
unaffected by NTP. Ideal for measuring durations.

@
let tracer = monotonicTimestampTracer downstream
traceWith tracer \"Hello\"
@
-}
monotonicTimestampTracer
    :: Tracer IO (Timestamped Word64 a)
    -> Tracer IO a
monotonicTimestampTracer downstream = mkTracer $ \event -> do
    time <- getMonotonicTimeNSec
    traceWith downstream $ Timestamped time event
