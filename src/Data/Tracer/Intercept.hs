{-# LANGUAGE RankNTypes #-}

{- |
Module      : Data.Tracer.Intercept
Description : Selective forwarding via partial mapping
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a combinator for intercepting trace events and selectively
forwarding them to a secondary tracer based on a partial mapping function.
-}
module Data.Tracer.Intercept
    ( intercept
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.Foldable (for_)
import Data.Tracer.Internal (mkTracer)

{- | Modify a 'Tracer' to emit to another 'Tracer' based on a mapping
function.

The intercepted tracer continues to emit all events to the primary tracer,
but also forwards events to a secondary tracer for each element returned
by the mapping function.

@
intercept secondary mapping primary
@

* @secondary@ - receives transformed events
* @mapping@ - function producing zero or more events to forward
* @primary@ - receives all events unchanged
-}
intercept
    :: Tracer IO b
    -- ^ secondary tracer for intercepted events
    -> (a -> [b])
    -- ^ mapping function (empty list = no interception)
    -> Tracer IO a
    -- ^ primary tracer
    -> Tracer IO a
intercept secondary g primary = mkTracer $ \a -> do
    for_ (g a) (traceWith secondary)
    traceWith primary a
