{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tracer.InterceptSpec (spec) where

import Control.Tracer (Tracer (Tracer), traceWith)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Tracer.Intercept (intercept)
import Data.Tracer.Internal (mkTracer)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "intercept" $ do
        it "forwards all events to primary tracer" $
            property $
                \(events :: [Int]) -> ioProperty $ do
                    primaryRef <- newIORef []
                    secondaryRef <- newIORef []
                    let primary = mkTracer $ \x ->
                            modifyIORef' primaryRef (x :)
                        secondary = mkTracer $ \x ->
                            modifyIORef' secondaryRef (x :)
                        intercepted = intercept secondary (const []) primary
                    mapM_ (traceWith intercepted) events
                    received <- readIORef primaryRef
                    return $ reverse received === events

        it "forwards matching events to secondary tracer" $
            property $
                \(events :: [Int]) -> ioProperty $ do
                    primaryRef <- newIORef []
                    secondaryRef <- newIORef []
                    let primary = mkTracer $ \x ->
                            modifyIORef' primaryRef (x :)
                        secondary = mkTracer $ \x ->
                            modifyIORef' secondaryRef (x :)
                        onlyEven x = [x | even x]
                        intercepted = intercept secondary onlyEven primary
                    mapM_ (traceWith intercepted) events
                    received <- readIORef secondaryRef
                    return $ reverse received === filter even events

        it "applies mapping function to forwarded events" $
            property $
                \(events :: [Int]) -> ioProperty $ do
                    primaryRef <- newIORef []
                    secondaryRef <- newIORef []
                    let primary = mkTracer $ \x ->
                            modifyIORef' primaryRef (x :)
                        secondary = mkTracer $ \x ->
                            modifyIORef' secondaryRef (x :)
                        doubleIfPositive x =
                            [x * 2 | x > 0]
                        intercepted =
                            intercept secondary doubleIfPositive primary
                    mapM_ (traceWith intercepted) events
                    received <- readIORef secondaryRef
                    let expected = map (* 2) (filter (> 0) events)
                    return $ reverse received === expected

        it "emits to secondary before primary" $ do
            ref <- newIORef []
            let primary = mkTracer $ \x ->
                    modifyIORef' ref ((x, "primary") :)
                secondary = mkTracer $ \x ->
                    modifyIORef' ref ((x, "secondary") :)
                intercepted = intercept secondary (\x -> [x]) primary
            traceWith intercepted "test"
            received <- readIORef ref
            received `shouldBe` [("test", "primary"), ("test", "secondary")]
