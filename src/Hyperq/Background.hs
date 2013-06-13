module Hyperq.Background (
    Work
  , SendWork
  , spawnWorkers
  , testRun
  ) where

import Control.Monad
    ( replicateM, replicateM_, when )
import Control.Concurrent
    ( ThreadId, forkIO, myThreadId, threadDelay )
import Control.Exception as E
import Control.Concurrent.STM

type Work = IO ()

type SendWork = Work -> STM ()

spawnWorkers :: Int -> IO (SendWork,IO (), [ThreadId])
spawnWorkers i | i <= 0 = error "Need positive number of workers"
               | otherwise = do
      workChan <- atomically newTChan
      runCount <- atomically (newTVar i)
      let stop = atomically (writeTVar runCount . pred =<< readTVar runCount)
          die e = do tId <- myThreadId
                     print ("Thread "++show tId++" died with exception "++show (e :: ErrorCall))
                     stop
          work = do mJob <- atomically (readTChan workChan)
                    case mJob of Nothing -> stop
                                 Just job -> E.catch job die >> work
      threads <- replicateM i (forkIO work)
      let stopCommand = do atomically (replicateM_ i (writeTChan workChan Nothing))
                           atomically (do running <- readTVar runCount
                                          when (running>0) retry)
      return (writeTChan workChan . Just, stopCommand, threads)

printJob :: Int -> IO ()
printJob i = do threadDelay (i*1000)
                tId <- myThreadId
                print ("printJob took "++show i++" ms in thread "++show tId)

testRun :: IO ()
testRun = do
  (submit,stop,ids) <- spawnWorkers 10
  mapM_ (\x -> print $ ' ' : show x) ids
  mapM_ (atomically . submit . printJob) (take 100 (cycle [100,200,300,400]))
  atomically $ submit (error "Boom")
  stop
