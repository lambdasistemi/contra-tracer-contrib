{-# LANGUAGE GADTs #-}

{- |
Module      : Data.Tracer.Measure
Description : Duration measurement between paired events
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

A tracer transformer that intercepts paired start\/end events,
measures the elapsed time between them, and emits a single
composed event carrying the duration in seconds.

Non-matching events pass through unchanged. The start event
is swallowed, the end event is replaced by the composed
measurement. This keeps the producer free from 'MonadIO'
— all timing happens in the 'IO' tracer pipeline.

The 'Timing' singleton selects the clock source. Pattern
matching on it provides both the clock read and the diff
function, so users only choose the variant — everything
else follows.
-}
module Data.Tracer.Measure
    ( Timing (..)
    , measureDuration
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock
    ( UTCTime
    , diffUTCTime
    , getCurrentTime
    , nominalDiffTimeToSeconds
    )
import Data.Tracer.Internal (mkTracer)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

{- | Clock source for duration measurement.

Pattern matching on a 'Timing' value determines the
timestamp type @t@, the clock read action, and the diff
function that converts two timestamps to seconds.
-}
data Timing t where
    {- | Monotonic clock. Nanosecond precision, unaffected
    by NTP adjustments. Ideal for measuring durations.
    -}
    Monotonic :: Timing Word64
    {- | Wall clock via 'getCurrentTime'. Microsecond
    precision, subject to NTP jumps.
    -}
    WallClock :: Timing UTCTime

-- | Read the clock for a given 'Timing'.
readClock :: Timing t -> IO t
readClock Monotonic = getMonotonicTimeNSec
readClock WallClock = getCurrentTime

-- | Compute elapsed seconds between two timestamps.
diffSeconds :: Timing t -> t -> t -> Double
diffSeconds Monotonic t0 t1 =
    fromIntegral (t1 - t0) / 1e9
diffSeconds WallClock t0 t1 =
    realToFrac $
        nominalDiffTimeToSeconds $
            diffUTCTime t1 t0

{- | Create a tracer that measures duration between paired
events.

Given a clock source, two selectors, and a composer, this
transformer:

1. When the start selector matches: records the timestamp
   and the extracted context, swallows the event.
2. When the end selector matches: computes elapsed seconds
   since the start, emits the composed event, clears the
   pending state.
3. All other events pass through unchanged.

If an end event arrives without a preceding start, it is
passed through unchanged. If two start events arrive
without an end, the second overwrites the first.

@
data AppTrace
    = PhaseStart String
    | PhaseEnd String
    | PhaseDuration String String Double
    | OtherTrace String

tracer <- measureDuration Monotonic
    (\\case PhaseStart s -> Just s; _ -> Nothing)
    (\\case PhaseEnd s -> Just s; _ -> Nothing)
    (\\startCtx endCtx secs -> PhaseDuration startCtx endCtx secs)
    downstream
@
-}
measureDuration
    :: Timing t
    -- ^ clock source
    -> (a -> Maybe b)
    -- ^ select start event, extract context
    -> (a -> Maybe c)
    -- ^ select end event, extract context
    -> (b -> c -> Double -> a)
    -- ^ compose measurement (start, end, seconds)
    -> Tracer IO a
    -- ^ downstream tracer
    -> IO (Tracer IO a)
    -- ^ stateful tracer
measureDuration timing selectStart selectEnd compose downstream =
    do
        ref <- newIORef Nothing
        pure $ mkTracer $ \event ->
            case selectStart event of
                Just ctx -> do
                    t <- readClock timing
                    writeIORef ref (Just (t, ctx))
                Nothing -> case selectEnd event of
                    Just endCtx -> do
                        pending <- readIORef ref
                        case pending of
                            Nothing ->
                                traceWith downstream event
                            Just (startTime, startCtx) -> do
                                endTime <-
                                    readClock timing
                                writeIORef ref Nothing
                                traceWith downstream $
                                    compose
                                        startCtx
                                        endCtx
                                        ( diffSeconds
                                            timing
                                            startTime
                                            endTime
                                        )
                    Nothing ->
                        traceWith downstream event
