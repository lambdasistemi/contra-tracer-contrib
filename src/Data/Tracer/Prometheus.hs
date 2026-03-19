{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Tracer.Prometheus
Description : Prometheus text exposition format helpers
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Helpers for rendering metrics in Prometheus text exposition
format. Each metric is rendered as a HELP line, a TYPE line,
and a value line, following the
<https://prometheus.io/docs/instrumenting/exposition_formats/ Prometheus specification>.

@
renderMyMetrics :: Text -> MyMetrics -> Text
renderMyMetrics prefix m =
    renderPrometheusLines
        [ gauge prefix "queue_length"
            "Average queue length" (queueLen m)
        , counter prefix "blocks_total"
            "Total blocks processed"
            (fromIntegral $ blockCount m)
        ]
@
-}
module Data.Tracer.Prometheus
    ( gauge
    , counter
    , renderPrometheusLines
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

{- | Render a Prometheus gauge metric.

A gauge is a metric that can go up and down (e.g. queue
length, current speed).
-}
gauge
    :: Text
    -- ^ metric prefix (e.g. @\"myapp\"@)
    -> Text
    -- ^ metric name (e.g. @\"queue_length\"@)
    -> Text
    -- ^ help description
    -> Double
    -- ^ current value
    -> [Text]
gauge = metric "gauge"

{- | Render a Prometheus counter metric.

A counter is a metric that only goes up (e.g. total
requests, cumulative duration).
-}
counter
    :: Text
    -- ^ metric prefix
    -> Text
    -- ^ metric name
    -> Text
    -- ^ help description
    -> Double
    -- ^ current value
    -> [Text]
counter = metric "counter"

{- | Concatenate rendered metric blocks into a single
Prometheus exposition text.
-}
renderPrometheusLines :: [[Text]] -> Text
renderPrometheusLines = Text.unlines . concat

-- internal

metric :: Text -> Text -> Text -> Text -> Double -> [Text]
metric typ prefix name help val =
    [ "# HELP "
        <> prefix
        <> "_"
        <> name
        <> " "
        <> help
    , "# TYPE "
        <> prefix
        <> "_"
        <> name
        <> " "
        <> typ
    , prefix <> "_" <> name <> " " <> showDouble val
    ]

showDouble :: Double -> Text
showDouble v =
    let s = Text.pack $ show v
    in  if Text.isSuffixOf ".0" s
            then Text.dropEnd 2 s
            else s
