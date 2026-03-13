module Data.Tracer.ShareSpec (spec) where

import Control.Tracer (traceWith)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Tracer.Internal (mkTracer)
import Data.Tracer.Share (shareTracer)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Data.Tracer.Share" $ do
    it "writes each value to the shared cell" $ do
        cell <- newIORef (0 :: Int)
        let downstream = mkTracer $ \_ -> pure ()
            tracer = shareTracer cell downstream
        mapM_ (traceWith tracer) [1, 2, 3 :: Int]
        -- Cell holds the last value
        result <- readIORef cell
        result `shouldBe` 3

    it "passes values to downstream" $ do
        cell <- newIORef (0 :: Int)
        collected <- newIORef ([] :: [Int])
        let downstream = mkTracer $ \a ->
                modifyIORef' collected (a :)
            tracer = shareTracer cell downstream
        mapM_ (traceWith tracer) [10, 20, 30 :: Int]
        result <- readIORef collected
        reverse result `shouldBe` [10, 20, 30]

    it "preserves laziness of values" $ do
        cell <- newIORef (undefined :: Int)
        let downstream = mkTracer $ \_ -> pure ()
            tracer = shareTracer cell downstream
        -- Tracing undefined should not crash because
        -- neither the cell write nor downstream forces it
        traceWith tracer (undefined :: Int)
        -- If we got here, laziness was preserved
        pure ()
