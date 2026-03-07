module Data.Tracer.TimestampSpec (spec) where

import Control.Tracer (traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime)
import Data.Tracer.Internal (mkTracer)
import Data.Tracer.Timestamp
    ( Timestamped (..)
    , monotonicTimestampTracer
    , utcTimestampTracer
    )
import GHC.Clock (getMonotonicTimeNSec)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldSatisfy
    )

spec :: Spec
spec = do
    describe "Timestamped data type" $ do
        it "stores time and event" $ do
            now <- getCurrentTime
            let ts = Timestamped now "test"
            timestampedTime ts
                `shouldSatisfy` (== now)
            timestampedEvent ts `shouldBe` "test"

    describe "utcTimestampTracer" $ do
        it "wraps events with timestamps" $ do
            ref <- newIORef Nothing
            let downstream =
                    mkTracer $ writeIORef ref . Just
                tracer = utcTimestampTracer downstream
            traceWith tracer "test message"
            result <- readIORef ref
            case result of
                Nothing ->
                    expectationFailure
                        "No event received"
                Just (Timestamped _ event) ->
                    event `shouldBe` "test message"

        it "adds current time to events" $ do
            ref <- newIORef Nothing
            before <- getCurrentTime
            let downstream =
                    mkTracer $ writeIORef ref . Just
                tracer = utcTimestampTracer downstream
            traceWith tracer "test"
            after <- getCurrentTime
            result <- readIORef ref
            case result of
                Nothing ->
                    expectationFailure
                        "No event received"
                Just (Timestamped time _) -> do
                    time `shouldSatisfy` (>= before)
                    time `shouldSatisfy` (<= after)

        it "preserves event content unchanged" $ do
            ref <- newIORef Nothing
            let downstream =
                    mkTracer $ writeIORef ref . Just
                tracer = utcTimestampTracer downstream
                testData =
                    ("key", [1, 2, 3 :: Int], True)
            traceWith tracer testData
            result <- readIORef ref
            case result of
                Nothing ->
                    expectationFailure
                        "No event received"
                Just (Timestamped _ event) ->
                    event `shouldBe` testData

        it "works with different event types" $ do
            intRef <- newIORef Nothing
            let intDownstream =
                    mkTracer $ writeIORef intRef . Just
                intTracer =
                    utcTimestampTracer intDownstream
            traceWith intTracer (42 :: Int)
            intResult <- readIORef intRef
            fmap timestampedEvent intResult
                `shouldBe` Just 42

            listRef <- newIORef Nothing
            let listDownstream =
                    mkTracer $
                        writeIORef listRef . Just
                listTracer =
                    utcTimestampTracer listDownstream
            traceWith listTracer [1, 2, 3 :: Int]
            listResult <- readIORef listRef
            fmap timestampedEvent listResult
                `shouldBe` Just [1, 2, 3]

        it "assigns non-decreasing timestamps" $ do
            ref <- newIORef []
            let downstream = mkTracer $ \ts -> do
                    v <- readIORef ref
                    writeIORef ref (ts : v)
                tracer = utcTimestampTracer downstream
            traceWith tracer "first"
            traceWith tracer "second"
            traceWith tracer "third"
            results <- reverse <$> readIORef ref
            length results `shouldBe` 3
            let times = map timestampedTime results
            times `shouldSatisfy` isNonDecreasing

    describe "monotonicTimestampTracer" $ do
        it "wraps events with nanosecond timestamps" $
            do
                ref <- newIORef Nothing
                let downstream =
                        mkTracer $ writeIORef ref . Just
                    tracer =
                        monotonicTimestampTracer
                            downstream
                traceWith tracer "test message"
                result <- readIORef ref
                case result of
                    Nothing ->
                        expectationFailure
                            "No event received"
                    Just (Timestamped _ event) ->
                        event `shouldBe` "test message"

        it "produces timestamps near current time" $
            do
                ref <- newIORef Nothing
                before <- getMonotonicTimeNSec
                let downstream =
                        mkTracer $ writeIORef ref . Just
                    tracer =
                        monotonicTimestampTracer
                            downstream
                traceWith tracer "test"
                after <- getMonotonicTimeNSec
                result <- readIORef ref
                case result of
                    Nothing ->
                        expectationFailure
                            "No event received"
                    Just (Timestamped time _) -> do
                        time
                            `shouldSatisfy` (>= before)
                        time
                            `shouldSatisfy` (<= after)

        it "assigns non-decreasing timestamps" $ do
            ref <- newIORef []
            let downstream = mkTracer $ \ts -> do
                    v <- readIORef ref
                    writeIORef ref (ts : v)
                tracer =
                    monotonicTimestampTracer downstream
            traceWith tracer "first"
            traceWith tracer "second"
            traceWith tracer "third"
            results <- reverse <$> readIORef ref
            length results `shouldBe` 3
            let times = map timestampedTime results
            times `shouldSatisfy` isNonDecreasing
  where
    isNonDecreasing :: (Ord a) => [a] -> Bool
    isNonDecreasing xs =
        and $ zipWith (<=) xs (drop 1 xs)
