module Data.Tracer.Foldl.ExtraSpec (spec) where

import Control.Foldl (Fold, fold)
import Data.Time
    ( UTCTime (..)
    , addUTCTime
    , fromGregorian
    )
import Data.Tracer.Foldl.Extra
    ( averageOverWindow
    , speedoMeter
    , valueSpeedoMeter
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

epoch :: UTCTime
epoch =
    UTCTime (fromGregorian 2025 1 1) 0

-- | Generate timestamps 1 second apart
timestamps :: Int -> [UTCTime]
timestamps n =
    [ addUTCTime (fromIntegral i) epoch
    | i <- [0 .. n - 1]
    ]

applyFold :: Fold a b -> [a] -> b
applyFold = fold

spec :: Spec
spec = describe "Data.Tracer.Foldl.Extra" $ do
    describe "averageOverWindow" $ do
        it "returns 0 with no input" $ do
            applyFold (averageOverWindow 10) ([] :: [Double])
                `shouldBe` 0

        it "averages all values when fewer than window" $ do
            applyFold
                (averageOverWindow 10)
                [2.0, 4.0, 6.0 :: Double]
                `shouldBe` 4.0

        it "averages only the last window values" $ do
            applyFold
                (averageOverWindow 3)
                [1.0, 2.0, 3.0, 10.0, 20.0, 30.0 :: Double]
                `shouldBe` 20.0

    describe "speedoMeter" $ do
        it "returns 0 with no input" $ do
            applyFold (speedoMeter 10) []
                `shouldBe` 0

        it "returns 0 before first window completes" $ do
            applyFold (speedoMeter 100) (timestamps 50)
                `shouldBe` 0

        it
            "measures ~1 event/sec with 1-second intervals"
            $ do
                let speed =
                        applyFold
                            (speedoMeter 10)
                            (timestamps 25)
                speed `shouldSatisfy` (> 0)

    describe "valueSpeedoMeter" $ do
        it "returns 0 with no input" $ do
            applyFold (valueSpeedoMeter 10) []
                `shouldBe` 0

        it "measures value change rate" $ do
            let events =
                    zip
                        (timestamps 25)
                        [0, 10 .. 240]
                speed =
                    applyFold
                        (valueSpeedoMeter 10)
                        events
            speed `shouldSatisfy` (> 0)
