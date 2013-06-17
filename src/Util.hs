module Util (
      printTiming
    , printTiming'
    , now
    , dbg
    ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf           (printf)
import Debug.Trace (trace)

dbg :: (Show a) => String -> a -> a
dbg = dbgsh show

dbgsh :: (a -> String) -> String -> a -> a
dbgsh sh msg x = trace (msg++sh x) x


now :: IO Double
now = realToFrac `fmap` getPOSIXTime

printTiming :: Int -> Double -> Double -> IO ()
printTiming iters start end = do
    let diff = end - start
    putStrLn $ printf "done in %s (%s tps)" (time diff) (tps diff iters)

    where
        tps :: Double -> Int -> String
        tps d i = printf "%.0f" (realToFrac i / d)

        time :: Double -> String
        time = printf "%.4f sec"


printTiming' :: Double -> Double -> IO ()
printTiming' start end = do
    let diff = end - start
    putStrLn $ printf "done in %s" (time diff)

    where
        time :: Double -> String
        time = printf "%.4f sec"
