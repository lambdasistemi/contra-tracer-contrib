module Data.Tracer.ThrottleSpec
    ( spec
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Tracer.Internal (mkTracer)
import Data.Tracer.Throttle
    ( Throttled (..)
    , throttleByFrequency
    )
import Data.Tracer.Timestamp (Timestamped (..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Create a tracer that collects events in a list
collectTracer :: IORef [a] -> Tracer IO a
collectTracer ref = mkTracer $ \a -> modifyIORef' ref (a :)

-- | Helper to create a Timestamped with a specific time
mkTimestamped :: t -> a -> Timestamped t a
mkTimestamped t a =
    Timestamped
        { timestampedTime = t
        , timestampedEvent = a
        }

-- | Diff function for UTCTime
diffSeconds :: UTCTime -> UTCTime -> Double
diffSeconds t1 t2 = realToFrac (diffUTCTime t2 t1)

-- | Base time for tests
baseTime :: UTCTime
baseTime = read "2024-01-01 00:00:00 UTC"

-- | Add seconds to base time
addSecs :: Double -> UTCTime -> UTCTime
addSecs s = addUTCTime (realToFrac s)

spec :: Spec
spec = do
    describe "throttleByFrequency" $ do
        it "passes through non-matching events immediately" $
            do
                ref <- newIORef []
                tracer <-
                    throttleByFrequency
                        diffSeconds
                        []
                        (collectTracer ref)
                traceWith
                    tracer
                    (mkTimestamped baseTime "event1")
                traceWith
                    tracer
                    (mkTimestamped baseTime "event2")
                results <- reverse <$> readIORef ref
                length results `shouldBe` 2
                map throttledDropped results
                    `shouldBe` [0, 0]

        it "emits first matching event immediately" $
            do
                ref <- newIORef []
                let matcher _ = Just 1.0
                tracer <-
                    throttleByFrequency
                        diffSeconds
                        [matcher]
                        (collectTracer ref)
                traceWith
                    tracer
                    (mkTimestamped baseTime "event1")
                results <- readIORef ref
                case results of
                    [r] ->
                        throttledDropped r `shouldBe` 0
                    _ ->
                        fail "expected exactly one result"

        it "drops events within throttle interval" $
            do
                ref <- newIORef []
                let matcher _ = Just 1.0
                tracer <-
                    throttleByFrequency
                        diffSeconds
                        [matcher]
                        (collectTracer ref)
                traceWith
                    tracer
                    (mkTimestamped baseTime "event1")
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.5 baseTime)
                        "event2"
                    )
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.9 baseTime)
                        "event3"
                    )
                results <- readIORef ref
                length results `shouldBe` 1

        it "reports dropped count when interval passes" $
            do
                ref <- newIORef []
                let matcher _ = Just 1.0
                tracer <-
                    throttleByFrequency
                        diffSeconds
                        [matcher]
                        (collectTracer ref)
                traceWith
                    tracer
                    (mkTimestamped baseTime "event1")
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.3 baseTime)
                        "event2"
                    )
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.6 baseTime)
                        "event3"
                    )
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 1.1 baseTime)
                        "event4"
                    )
                results <- reverse <$> readIORef ref
                length results `shouldBe` 2
                map throttledDropped results
                    `shouldBe` [0, 2]
                map
                    (timestampedEvent . throttledEvent)
                    results
                    `shouldBe` ["event1", "event4"]

        it
            "handles multiple matcher categories \
            \independently"
            $ do
                ref <- newIORef []
                let matcherA "A" = Just 1.0
                    matcherA _ = Nothing
                    matcherB "B" = Just 2.0
                    matcherB _ = Nothing
                tracer <-
                    throttleByFrequency
                        diffSeconds
                        [matcherA, matcherB]
                        (collectTracer ref)
                traceWith
                    tracer
                    (mkTimestamped baseTime "A")
                traceWith
                    tracer
                    (mkTimestamped baseTime "B")
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.3 baseTime)
                        "A"
                    )
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.3 baseTime)
                        "B"
                    )
                traceWith
                    tracer
                    ( mkTimestamped
                        (addSecs 0.6 baseTime)
                        "B"
                    )
                results <- reverse <$> readIORef ref
                length results `shouldBe` 3
                map
                    (timestampedEvent . throttledEvent)
                    results
                    `shouldBe` ["A", "B", "B"]
