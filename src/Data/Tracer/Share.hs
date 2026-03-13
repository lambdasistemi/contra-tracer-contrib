{- |
Module      : Data.Tracer.Share
Description : Share traced values via a mutable cell
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

A tracer combinator that writes each traced value to a shared
mutable cell ('IORef') before passing it downstream. External
consumers (e.g. an HTTP handler) can read the cell at any time.

The value is written lazily — if neither the downstream tracer
nor the external reader forces it, 'extract' (from an upstream
'foldTracer') never runs.

@
metricsRef <- newIORef initialMetrics
let pipeline =
        foldTracer avgFold
        . shareTracer metricsRef  -- HTTP reads here
        . sampleTracer 1Hz
        $ logTracer

-- HTTP handler
handleMetrics = readIORef metricsRef >>= evaluate
@
-}
module Data.Tracer.Share
    ( shareTracer
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.IORef (IORef, writeIORef)
import Data.Tracer.Internal (mkTracer)

{- | Create a tracer that shares values via an 'IORef'.

Each traced value is written to the cell (without forcing
it), then passed to the downstream tracer. The 'IORef'
always holds the latest value — or its thunk if nobody
has forced it yet.

This preserves laziness: 'writeIORef' does not evaluate
the value it stores.
-}
shareTracer
    :: IORef b
    -- ^ shared cell for external readers
    -> Tracer IO b
    -- ^ downstream tracer
    -> Tracer IO b
shareTracer ref downstream = mkTracer $ \b -> do
    writeIORef ref b
    traceWith downstream b
