module Data.Tracer.FoldSpec (spec) where

import Control.Foldl (Fold (..))
import Control.Monad (when)
import Control.Tracer (Tracer, traceWith)
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Tracer.Fold (foldTracer)
import Data.Tracer.Internal (mkTracer)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | A simple sum fold.
sumFold :: Fold Int Int
sumFold = Fold (+) 0 id

-- | A fold that counts events.
countFold :: Fold a Int
countFold = Fold (\n _ -> n + 1) 0 id

-- | Collect traced values into an IORef list.
collectTracer :: IORef [a] -> IO (Tracer IO a)
collectTracer ref =
    pure $ mkTracer $ \a ->
        modifyIORef' ref (a :)

spec :: Spec
spec = describe "Data.Tracer.Fold" $ do
    it "accumulates a running sum" $ do
        ref <- newIORef ([] :: [Int])
        downstream <- collectTracer ref
        tracer <- foldTracer sumFold downstream
        mapM_ (traceWith tracer) [1, 2, 3, 4, 5 :: Int]
        result <- readIORef ref
        -- Reversed because we prepend
        reverse result `shouldBe` [1, 3, 6, 10, 15]

    it "counts events" $ do
        ref <- newIORef ([] :: [Int])
        downstream <- collectTracer ref
        tracer <- foldTracer countFold downstream
        mapM_ (traceWith tracer) ["a", "b", "c" :: String]
        result <- readIORef ref
        reverse result `shouldBe` [1, 2, 3]

    it "works with zero events" $ do
        ref <- newIORef ([] :: [Int])
        downstream <- collectTracer ref
        _ <- foldTracer sumFold downstream
        result <- readIORef ref
        result `shouldBe` []

    it "extract runs only when downstream forces" $ do
        extractCount <- newIORef (0 :: Int)
        eventCount <- newIORef (0 :: Int)
        -- A fold whose extract has an observable side
        -- effect: the counter increments only when the
        -- thunk is forced
        let countingExtract s = unsafePerformIO $ do
                modifyIORef' extractCount (+ 1)
                pure s
        -- Downstream forces only every 10th value
        let downstream = mkTracer $ \b -> do
                n <- readIORef eventCount
                modifyIORef' eventCount (+ 1)
                when (n `mod` 10 == 0) $ b `seq` pure ()
        tracer <-
            foldTracer
                (Fold (+) (0 :: Int) countingExtract)
                downstream
        -- Send 100 events
        mapM_ (traceWith tracer) [1 :: Int .. 100]
        -- Only 10 out of 100 should have been forced
        extractions <- readIORef extractCount
        extractions `shouldBe` 10
