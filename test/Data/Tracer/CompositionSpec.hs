{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tracer.CompositionSpec (spec) where

-- \|
-- Module      : Data.Tracer.CompositionSpec
-- Description : Tests for tracer compositions
-- Copyright   : (c) Paolo Veronelli, 2025
-- License     : Apache-2.0
--
-- Tests that verify tracer utilities work correctly when composed together,
-- especially under concurrent load.

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_, replicateM_)
import Control.Tracer (Tracer, traceWith)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Time.Clock
    ( UTCTime
    , addUTCTime
    , diffUTCTime
    )
import Data.Tracer.Intercept (intercept)
import Data.Tracer.Internal (mkTracer)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Data.Tracer.Throttle
    ( Throttled (..)
    , throttleByFrequency
    )
import Data.Tracer.Timestamp
    ( Timestamped (..)
    , utcTimestampTracer
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Create a tracer that collects events in a list (thread-safe append)
collectTracer :: IORef [a] -> Tracer IO a
collectTracer ref = mkTracer $ \a ->
    atomicModifyIORef' ref (\xs -> (a : xs, ()))

-- | Base time for tests
baseTime :: UTCTime
baseTime = read "2024-01-01 00:00:00 UTC"

-- | Add seconds to a time
addSeconds :: Double -> UTCTime -> UTCTime
addSeconds s = addUTCTime (realToFrac s)

-- | Diff function for UTCTime
diffSecs :: UTCTime -> UTCTime -> Double
diffSecs t1 t2 = realToFrac (diffUTCTime t2 t1)

-- | Create a Timestamped with a specific time
mkTimestamped :: UTCTime -> a -> Timestamped UTCTime a
mkTimestamped t a =
    Timestamped
        { timestampedTime = t
        , timestampedEvent = a
        }

-- | Match errors by checking for "error" in the string
matchError :: String -> Maybe String
matchError s = if "error" `isInfixOf` s then Just s else Nothing

-- | Extract events that had drops
extractDrops
    :: Throttled UTCTime String
    -> Maybe (Throttled UTCTime String)
extractDrops t@Throttled{throttledDropped}
    | throttledDropped > 0 = Just t
    | otherwise = Nothing

-- | Check if a Timestamped event is valid (non-empty event)
hasValidTimestamp :: Timestamped UTCTime String -> Bool
hasValidTimestamp ts = not (null (timestampedEvent ts))

spec :: Spec
spec = describe "Tracer Compositions" $ do
    describe "utcTimestampTracer + ThreadSafe" $ do
        it "adds timestamps correctly under concurrent access" $ do
            ref <- newIORef []
            let baseTracer = collectTracer ref
            throttled <- throttleByFrequency diffSecs [] baseTracer
            let timestamped = utcTimestampTracer throttled
            safe <- newThreadSafeTracer timestamped

            forConcurrently_ [1 :: Int .. 50] $ \i ->
                replicateM_ 10 $ traceWith safe ("msg-" ++ show i)

            messages <- readIORef ref
            length messages `shouldBe` 500
            -- All events should be Throttled with Timestamped inside
            all (hasValidTimestamp . throttledEvent) messages `shouldBe` True

    describe "Throttle + ThreadSafe" $ do
        it "throttles correctly under concurrent access" $ do
            ref <- newIORef []
            let matcher _ = Just 1.0 -- 1 Hz
            throttled <-
                throttleByFrequency diffSecs [matcher] (collectTracer ref)
            safe <- newThreadSafeTracer throttled

            -- All events have same timestamp, so all after first should drop
            forConcurrently_ [1 :: Int .. 10] $ \_ ->
                replicateM_ 10 $
                    traceWith safe (mkTimestamped baseTime "event")

            results <- readIORef ref
            -- First event passes, rest are dropped (same timestamp)
            length results `shouldBe` 1

        it "maintains correct drop counts with sequential timestamps" $ do
            ref <- newIORef []
            let matcher _ = Just 1.0 -- 1 Hz = 1 second interval
            throttled <-
                throttleByFrequency diffSecs [matcher] (collectTracer ref)
            safe <- newThreadSafeTracer throttled

            -- Send events: 0s, 0.1s, 0.2s, ..., 1.0s, 1.1s, ..., 2.0s
            forM_ [0 :: Int .. 20] $ \i -> do
                let ts = addSeconds (fromIntegral i * 0.1) baseTime
                traceWith safe (mkTimestamped ts ("event-" ++ show i))

            results <- reverse <$> readIORef ref
            -- Events at 0s, 1.0s, 2.0s should pass (indices 0, 10, 20)
            length results `shouldBe` 3
            -- Drop counts: 0 (first), 9 (dropped 1-9), 9 (dropped 11-19)
            map throttledDropped results `shouldBe` [0, 9, 9]

    describe "Intercept + ThreadSafe" $ do
        it "intercepts correctly under concurrent access" $ do
            primaryRef <- newIORef []
            secondaryRef <- newIORef []
            let primary = collectTracer primaryRef
                secondary = collectTracer secondaryRef
                intercepted = intercept secondary matchError primary
            safe <- newThreadSafeTracer intercepted

            forConcurrently_ [1 :: Int .. 50] $ \i -> do
                traceWith safe ("info-" ++ show i)
                traceWith safe ("error-" ++ show i)

            primaryCount <- length <$> readIORef primaryRef
            secondaryCount <- length <$> readIORef secondaryRef
            primaryCount `shouldBe` 100 -- all events
            secondaryCount `shouldBe` 50 -- only errors
        it "preserves event order within each tracer" $ do
            primaryRef <- newIORef []
            secondaryRef <- newIORef []
            let primary = collectTracer primaryRef
                secondary = collectTracer secondaryRef
                intercepted = intercept secondary matchError primary
            safe <- newThreadSafeTracer intercepted

            -- Sequential to verify ordering
            forM_ [1 :: Int .. 10] $ \i -> do
                traceWith safe ("info-" ++ show i)
                traceWith safe ("error-" ++ show i)

            primaryEvents <- reverse <$> readIORef primaryRef
            secondaryEvents <- reverse <$> readIORef secondaryRef

            -- Primary should have all 20 events in order
            length primaryEvents `shouldBe` 20
            -- Secondary should have only errors in order
            secondaryEvents
                `shouldBe` ["error-" ++ show i | i <- [1 :: Int .. 10]]

    describe "Full Pipeline: Throttle + Intercept + ThreadSafe" $ do
        it "works as a full pipeline" $ do
            logRef <- newIORef []
            errorRef <- newIORef []

            let logTracer' = collectTracer logRef
                errorTracer = collectTracer errorRef
                -- Intercept events with drops before they reach logTracer
                interceptedLog = intercept errorTracer extractDrops logTracer'

            -- Throttle at 10 Hz (0.1s interval)
            throttled <-
                throttleByFrequency diffSecs [\_ -> Just 10.0] interceptedLog
            safe <- newThreadSafeTracer throttled

            -- Send 100 events with 0.05s spacing (faster than throttle)
            forM_ [1 :: Int .. 100] $ \i -> do
                let ts = addSeconds (fromIntegral i * 0.05) baseTime
                traceWith safe (mkTimestamped ts ("event-" ++ show i))

            logs <- readIORef logRef
            errors <- readIORef errorRef

            -- Some events pass through throttle
            length logs `shouldSatisfy` (< 100)
            length logs `shouldSatisfy` (> 0)
            -- Some events had drops (reported to error tracer)
            length errors `shouldSatisfy` (> 0)
            -- All intercepted events should have drops > 0
            all ((> 0) . throttledDropped) errors `shouldBe` True

        it "handles high concurrency with full pipeline" $ do
            logRef <- newIORef []
            errorRef <- newIORef []

            let logTracer' = collectTracer logRef
                errorTracer = collectTracer errorRef
                -- Intercept events with drops before they reach logTracer
                interceptedLog = intercept errorTracer extractDrops logTracer'

            throttled <-
                throttleByFrequency diffSecs [\_ -> Just 100.0] interceptedLog
            safe <- newThreadSafeTracer throttled

            -- Concurrent access with varying timestamps
            forConcurrently_ [1 :: Int .. 20] $ \threadId ->
                forM_ [1 :: Int .. 50] $ \eventId -> do
                    let ts = addSeconds (fromIntegral eventId * 0.005) baseTime
                    traceWith safe $
                        mkTimestamped
                            ts
                            ("t" ++ show threadId ++ "-e" ++ show eventId)

            logs <- readIORef logRef
            -- Should have processed events without crashes or deadlocks
            length logs `shouldSatisfy` (> 0)
            length logs `shouldSatisfy` (< 1000) -- throttling worked
