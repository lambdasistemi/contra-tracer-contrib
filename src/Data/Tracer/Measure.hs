{- |
Module      : Data.Tracer.Measure
Description : Duration measurement between paired events
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

A tracer transformer that intercepts paired start\/end events,
measures the elapsed time between them using a monotonic clock,
and emits a single composed event carrying the duration.

Non-matching events pass through unchanged. The start event
is swallowed, the end event is replaced by the composed
measurement. This keeps the producer free from 'MonadIO'
— all timing happens in the 'IO' tracer pipeline.
-}
module Data.Tracer.Measure
    ( measureDuration
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Tracer.Internal (mkTracer)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

{- | Create a tracer that measures duration between paired
events.

Given two selectors and a composer, this transformer:

1. When the start selector matches: records the monotonic
   timestamp and the extracted context, swallows the event.
2. When the end selector matches: computes elapsed
   nanoseconds since the start, emits the composed event,
   clears the pending state.
3. All other events pass through unchanged.

If an end event arrives without a preceding start, it is
passed through unchanged. If two start events arrive
without an end, the second overwrites the first.

@
data AppTrace
    = PhaseStart String
    | PhaseEnd String
    | PhaseDuration String String Word64
    | OtherTrace String

tracer <- measureDuration
    (\\case PhaseStart s -> Just s; _ -> Nothing)
    (\\case PhaseEnd s -> Just s; _ -> Nothing)
    (\\startCtx endCtx ns -> PhaseDuration startCtx endCtx ns)
    downstream
@
-}
measureDuration
    :: (a -> Maybe b)
    -- ^ select start event, extract context
    -> (a -> Maybe c)
    -- ^ select end event, extract context
    -> (b -> c -> Word64 -> a)
    -- ^ compose measurement (start, end, nanoseconds)
    -> Tracer IO a
    -- ^ downstream tracer
    -> IO (Tracer IO a)
    -- ^ stateful tracer
measureDuration selectStart selectEnd compose downstream =
    do
        ref <- newIORef Nothing
        pure $ mkTracer $ \event ->
            case selectStart event of
                Just ctx -> do
                    t <- getMonotonicTimeNSec
                    writeIORef ref (Just (t, ctx))
                Nothing -> case selectEnd event of
                    Just endCtx -> do
                        pending <- readIORef ref
                        case pending of
                            Nothing ->
                                traceWith downstream event
                            Just (startTime, startCtx) -> do
                                endTime <-
                                    getMonotonicTimeNSec
                                writeIORef ref Nothing
                                traceWith downstream $
                                    compose
                                        startCtx
                                        endCtx
                                        ( endTime
                                            - startTime
                                        )
                    Nothing ->
                        traceWith downstream event
