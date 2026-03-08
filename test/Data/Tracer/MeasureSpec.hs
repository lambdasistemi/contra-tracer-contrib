module Data.Tracer.MeasureSpec (spec) where

import Control.Tracer (traceWith)
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.Tracer.Internal (mkTracer)
import Data.Tracer.Measure (measureDuration)
import Data.Word (Word64)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

data Event
    = Start String
    | End String
    | Duration String String Word64
    | Other String
    deriving (Show, Eq)

selectStart :: Event -> Maybe String
selectStart (Start s) = Just s
selectStart _ = Nothing

selectEnd :: Event -> Maybe String
selectEnd (End s) = Just s
selectEnd _ = Nothing

compose :: String -> String -> Word64 -> Event
compose = Duration

collector :: IO (Event -> IO (), IO [Event])
collector = do
    ref <- newIORef []
    let push e = do
            v <- readIORef ref
            writeIORef ref (v ++ [e])
    pure (push, readIORef ref)

spec :: Spec
spec = do
    describe "measureDuration" $ do
        it "swallows start and replaces end with duration" $
            do
                (push, getEvents) <- collector
                let downstream = mkTracer push
                tracer <-
                    measureDuration
                        selectStart
                        selectEnd
                        compose
                        downstream
                traceWith tracer (Start "a")
                traceWith tracer (End "b")
                events <- getEvents
                length events `shouldBe` 1
                case events of
                    [Duration s e ns] -> do
                        s `shouldBe` "a"
                        e `shouldBe` "b"
                        ns `shouldSatisfy` (>= 0)
                    _ ->
                        error "unexpected events"

        it "passes non-matching events through" $ do
            (push, getEvents) <- collector
            let downstream = mkTracer push
            tracer <-
                measureDuration
                    selectStart
                    selectEnd
                    compose
                    downstream
            traceWith tracer (Other "hello")
            traceWith tracer (Other "world")
            events <- getEvents
            events
                `shouldBe` [ Other "hello"
                           , Other "world"
                           ]

        it "passes end through if no start" $ do
            (push, getEvents) <- collector
            let downstream = mkTracer push
            tracer <-
                measureDuration
                    selectStart
                    selectEnd
                    compose
                    downstream
            traceWith tracer (End "orphan")
            events <- getEvents
            events `shouldBe` [End "orphan"]

        it "measures positive duration" $ do
            (push, getEvents) <- collector
            let downstream = mkTracer push
            tracer <-
                measureDuration
                    selectStart
                    selectEnd
                    compose
                    downstream
            traceWith tracer (Start "x")
            _ <- pure $! sum [1 .. 1000 :: Int]
            traceWith tracer (End "y")
            events <- getEvents
            case events of
                [Duration _ _ ns] ->
                    ns `shouldSatisfy` (>= 0)
                _ -> error "unexpected events"

        it "interleaves with other events" $ do
            (push, getEvents) <- collector
            let downstream = mkTracer push
            tracer <-
                measureDuration
                    selectStart
                    selectEnd
                    compose
                    downstream
            traceWith tracer (Other "before")
            traceWith tracer (Start "s")
            traceWith tracer (Other "during")
            traceWith tracer (End "e")
            traceWith tracer (Other "after")
            events <- getEvents
            length events `shouldBe` 4
            case events of
                [ Other "before"
                    , Other "during"
                    , Duration "s" "e" _
                    , Other "after"
                    ] -> pure ()
                _ ->
                    error $
                        "unexpected: " ++ show events

        it "second start overwrites first" $ do
            (push, getEvents) <- collector
            let downstream = mkTracer push
            tracer <-
                measureDuration
                    selectStart
                    selectEnd
                    compose
                    downstream
            traceWith tracer (Start "first")
            traceWith tracer (Start "second")
            traceWith tracer (End "done")
            events <- getEvents
            case events of
                [Duration s _ _] ->
                    s `shouldBe` "second"
                _ -> error "unexpected events"
