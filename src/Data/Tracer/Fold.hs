{-# LANGUAGE BangPatterns #-}

{- |
Module      : Data.Tracer.Fold
Description : Streaming fold accumulation over trace events
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

A tracer transformer that applies a 'Fold' from the @foldl@
library to a stream of trace events. The fold accumulator is
stepped strictly on every input, but the extracted output is
passed downstream lazily — if the downstream tracer does not
force it, 'extract' never runs.

This pairs naturally with 'Data.Tracer.Throttle' for
rate-limited sampling: the fold steps on every event (cheap),
while the throttle decides which extracted values actually
reach the output.

@
import qualified Control.Foldl as L

-- Average over last 100 values
avgFold :: Fold Double Double
avgFold = ...

tracer <- throttleByFrequency diffSec [const (Just 1.0)] logTracer
let pipeline = foldTracer avgFold tracer
@
-}
module Data.Tracer.Fold
    ( foldTracer
    ) where

import Control.Foldl (Fold (..))
import Control.Tracer (Tracer, traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Tracer.Internal (mkTracer)

{- | Create a tracer that accumulates events with a 'Fold'.

The accumulator state is updated strictly on every input
event. The extracted value @b@ is passed downstream lazily
— it is a thunk that calls @extract@ on the current state.
If the downstream tracer never forces it (e.g. because a
throttle drops the event), no extraction work is performed.

@
foldTracer myFold downstream
@

produces a @Tracer m a@ that:

1. Steps the fold state with each incoming @a@ (strict).
2. Passes @extract state@ to @downstream@ (lazy).
-}
foldTracer
    :: Fold a b
    -- ^ the fold to accumulate with
    -> Tracer IO b
    -- ^ downstream tracer receiving extracted values
    -> IO (Tracer IO a)
    -- ^ stateful tracer
foldTracer (Fold step initial extract) downstream = do
    ref <- newIORef initial
    pure $ mkTracer $ \a -> do
        -- Strict update of the accumulator
        s <- readIORef ref
        let !s' = step s a
        writeIORef ref s'
        -- Lazy extraction: only forced if downstream demands it
        let b = extract s'
        traceWith downstream b
