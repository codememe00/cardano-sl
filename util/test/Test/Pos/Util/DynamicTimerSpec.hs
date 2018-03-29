module Test.Pos.Util.DynamicTimerSpec
    ( spec) where

import           Universum
import           Control.Concurrent.Async (async)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds, toMicroseconds)
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (Property, Gen, property, ioProperty, choose)
import           Test.QuickCheck.Monadic (forAllM, monadicIO, run, assert)

import           Pos.Util.DynamicTimer

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

chooseMsc :: (Microsecond, Microsecond) -> Gen Microsecond
chooseMsc (a, b) = fromMicroseconds <$> choose (toMicroseconds a, toMicroseconds b)

-- | A dynamic timer should wait at least given interval.
prop_wait :: Property
prop_wait = 
    let tMIN = 50 :: Microsecond
        tMAX = 100 :: Microsecond
    in monadicIO
    $ forAllM (chooseMsc (tMIN, tMAX))
    $ \t -> do
        timer <- newDynamicTimer (return t)
        startTime <- run getPOSIXTime
        _ <- startDynamicTimer timer
        _ <- atomically $ waitDynamicTimer timer
        endTime <- run getPOSIXTime
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        assert $ t <= diffTime 

-- | A dynamic timer should be additive: (re)-starting a timer @t1@ after @t0@
-- microseconds should wait @t0 + t1@ microseconds.
prop_additive :: Property
prop_additive = 
    let tMIN = 50 :: Microsecond
        tMAX = 100 :: Microsecond
    in monadicIO
    $ forAllM (chooseMsc (tMIN, tMAX))
    $ \tmax -> forAllM (chooseMsc (tMIN `div` 2 , tmax `div` 2))
    $ \tmin -> do
        timerMin <- newDynamicTimer (return tmin)
        timerMax <- newDynamicTimer (return tmax)
        startTime <- run $
               startDynamicTimer timerMin
            >> startDynamicTimer timerMax
            >> getPOSIXTime
        _ <- run $ async $
               atomically (waitDynamicTimer timerMin)
            >> startDynamicTimer timerMax
        endTime <- run $
               atomically (waitDynamicTimer timerMax)
            >> getPOSIXTime
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        assert (tmax + tmin <= diffTime)

spec :: Spec
spec = describe "DynamicTimer" $ do
    it "should wait" $ property prop_wait
    it "should be additive" $ property prop_additive
