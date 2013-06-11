module Util
  ( printTiming
  , printTiming'
  , now
  ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf           (printf)

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


-- vim: set ts=4 sw=4 et:
