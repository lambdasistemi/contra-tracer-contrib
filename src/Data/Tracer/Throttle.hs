{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Data.Tracer.Throttle
Description : Frequency-based event throttling
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a tracer that throttles events based on frequency
limits. Uses event timestamps for deterministic, testable
behavior.

The throttle is parameterized on the time type @t@ — pass
a diff function to compute elapsed seconds between two
timestamps.
-}
module Data.Tracer.Throttle
    ( Throttled (..)
    , throttleByFrequency
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tracer.Internal (mkTracer)
import Data.Tracer.Timestamp (Timestamped (..))

{- | Result of throttling an event.

When an event passes through the throttle, it includes
a count of how many events were dropped since the last
emission.
-}
data Throttled t a = Throttled
    { throttledEvent :: Timestamped t a
    -- ^ the event that passed through
    , throttledDropped :: Int
    -- ^ number of events dropped since last emission
    }
    deriving (Show, Eq)

-- | State for a single throttle category.
data ThrottleState t = ThrottleState
    { lastEmitTime :: t
    -- ^ when we last emitted for this category
    , droppedCount :: Int
    -- ^ events dropped since last emission
    }

{- | Create a tracer that throttles events based on
frequency limits.

The first argument computes elapsed seconds between two
timestamps. For example:

* UTC: @\\t1 t2 -> realToFrac (diffUTCTime t2 t1)@
* Monotonic: @\\t1 t2 -> fromIntegral (t2 - t1) / 1e9@

Each matcher function defines a throttle category. When a
matcher returns @Just frequency@, events matching that
category are limited to that frequency (in Hz).

Events that don't match any category pass through
immediately. The first event in each category always
passes through. Subsequent events within the throttle
interval are dropped. When an event finally passes, it
reports how many were dropped.

@
let matchErrors msg
        | "error" \`isInfixOf\` msg = Just 1.0
        | otherwise = Nothing
    diffSeconds t1 t2 =
        realToFrac (diffUTCTime t2 t1)
tracer <- throttleByFrequency diffSeconds [matchErrors] downstream
@
-}
throttleByFrequency
    :: (t -> t -> Double)
    -- ^ elapsed seconds between two timestamps
    -> [a -> Maybe Double]
    -- ^ matchers returning frequency in Hz
    -> Tracer IO (Throttled t a)
    -- ^ downstream tracer
    -> IO (Tracer IO (Timestamped t a))
throttleByFrequency diffSeconds matchers downstream = do
    stateRef <- newIORef (Map.empty :: Map Int (ThrottleState t))
    return $ mkTracer $ \event -> do
        let ts = timestampedTime event
        case findMatch matchers (timestampedEvent event) of
            Nothing ->
                traceWith downstream $
                    Throttled
                        { throttledEvent = event
                        , throttledDropped = 0
                        }
            Just (categoryIdx, frequency) -> do
                let interval = 1.0 / frequency
                state <- readIORef stateRef
                case Map.lookup categoryIdx state of
                    Nothing -> do
                        modifyIORef' stateRef $
                            Map.insert
                                categoryIdx
                                ThrottleState
                                    { lastEmitTime = ts
                                    , droppedCount = 0
                                    }
                        traceWith downstream $
                            Throttled
                                { throttledEvent =
                                    event
                                , throttledDropped = 0
                                }
                    Just
                        ThrottleState
                            { lastEmitTime
                            , droppedCount
                            } -> do
                            let elapsed =
                                    diffSeconds
                                        lastEmitTime
                                        ts
                            if elapsed >= interval
                                then do
                                    modifyIORef' stateRef $
                                        Map.insert
                                            categoryIdx
                                            ThrottleState
                                                { lastEmitTime =
                                                    ts
                                                , droppedCount =
                                                    0
                                                }
                                    traceWith downstream $
                                        Throttled
                                            { throttledEvent =
                                                event
                                            , throttledDropped =
                                                droppedCount
                                            }
                                else
                                    modifyIORef' stateRef $
                                        Map.adjust
                                            ( \s ->
                                                s
                                                    { droppedCount =
                                                        droppedCount
                                                            + 1
                                                    }
                                            )
                                            categoryIdx

-- | Find the first matching category and its frequency.
findMatch
    :: [a -> Maybe Double]
    -> a
    -> Maybe (Int, Double)
findMatch matchers event = go 0 matchers
  where
    go _ [] = Nothing
    go idx (m : ms) = case m event of
        Just freq -> Just (idx, freq)
        Nothing -> go (idx + 1) ms
