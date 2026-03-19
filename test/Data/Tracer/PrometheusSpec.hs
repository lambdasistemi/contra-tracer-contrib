{-# LANGUAGE OverloadedStrings #-}

module Data.Tracer.PrometheusSpec (spec) where

import qualified Data.Text as Text
import Data.Tracer.Prometheus
    ( counter
    , gauge
    , renderPrometheusLines
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Data.Tracer.Prometheus" $ do
    describe "gauge" $ do
        it "renders HELP, TYPE, and value lines" $ do
            gauge "myapp" "queue_len" "Queue length" 42.5
                `shouldBe` [ "# HELP myapp_queue_len"
                                <> " Queue length"
                           , "# TYPE myapp_queue_len"
                                <> " gauge"
                           , "myapp_queue_len 42.5"
                           ]

        it "renders integer-like doubles without .0" $ do
            let lines =
                    gauge
                        "myapp"
                        "count"
                        "Count"
                        3.0
            last lines `shouldBe` "myapp_count 3"

    describe "counter" $ do
        it "renders with counter type" $ do
            let lines =
                    counter
                        "myapp"
                        "total"
                        "Total"
                        100.0
            lines
                !! 1
                `shouldBe` "# TYPE myapp_total counter"

    describe "renderPrometheusLines" $ do
        it "concatenates metric blocks" $ do
            let result =
                    renderPrometheusLines
                        [ gauge
                            "app"
                            "a"
                            "Metric A"
                            1.0
                        , counter
                            "app"
                            "b"
                            "Metric B"
                            2.0
                        ]
            Text.count "\n" result `shouldBe` 6
            Text.isInfixOf "app_a 1" result
                `shouldBe` True
            Text.isInfixOf "app_b 2" result
                `shouldBe` True
