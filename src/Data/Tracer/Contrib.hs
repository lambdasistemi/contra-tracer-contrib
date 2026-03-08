{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Data.Tracer.Contrib
Description : Convenience re-exports for contra-tracer utilities
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

This module re-exports all utilities from contra-tracer-contrib along with
the core 'Tracer' type and 'traceWith' function from contra-tracer.

@
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \\tracer -> do
    safeTracer <- newThreadSafeTracer tracer
    throttled <- throttleByFrequency [\\_ -> Just 1.0] safeTracer
    let timestamped = timestampTracer throttled
    traceWith timestamped "Hello, world!"
@
-}
module Data.Tracer.Contrib
    ( -- * Re-exports from contra-tracer
      Tracer
    , traceWith

      -- * Intercept
    , intercept

      -- * Logging
    , logFileTracer
    , logTracer

      -- * Thread Safety
    , newThreadSafeTracer

      -- * Timestamps
    , Timestamped (..)
    , utcTimestampTracer
    , monotonicTimestampTracer

      -- * Duration Measurement
    , measureDuration

      -- * Throttling
    , Throttled (..)
    , throttleByFrequency

      -- * Pattern Synonym
    , pattern TraceWith
    , tracer
    , trace
    , contra
    ) where

import Control.Tracer (Tracer, traceWith)

import Data.Tracer.Intercept (intercept)
import Data.Tracer.LogFile (logFileTracer, logTracer)
import Data.Tracer.Measure (measureDuration)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Data.Tracer.Throttle (Throttled (..), throttleByFrequency)
import Data.Tracer.Timestamp
    ( Timestamped (..)
    , monotonicTimestampTracer
    , utcTimestampTracer
    )
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
