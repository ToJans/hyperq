-- [[file:~/projects/hyperq/hyperq.org::*threading%20example][threading\ example:1]]

import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Control.Concurrent.STM

type Work = IO ()

type SendWork = Work -> STM ()

spawnWorkers :: Int -> IO (SendWork,IO ())
spawnWorkers i | i <= 0 = error "Need positive number of workers"
               | otherwise = do
    workChan <- atomically newTChan
    runCount <- atomically (newTVar i)
    let stop = atomically (writeTVar runCount . pred =<< readTVar runCount)
        die e = do id <- myThreadId
                   print ("Thread "++show id++" died with exception "++show (e :: ErrorCall))
                   stop
        work = do mJob <- atomically (readTChan workChan)
                  case mJob of Nothing -> stop
                               Just job -> E.catch job die >> work
    replicateM_ i (forkIO work)
    let stopCommand = do atomically (replicateM_ i (writeTChan workChan Nothing))
                         atomically (do running <- readTVar runCount
                                        when (running>0) retry)
    return (writeTChan workChan . Just,stopCommand)

printJob :: Int -> IO ()
printJob i = do threadDelay (i*1000)
                id <- myThreadId
                print ("printJob took "++show i++" ms in thread "++show id)

main :: IO ()
main = do
  (submit,stop) <- spawnWorkers 10
  mapM_ (atomically . submit . printJob) (take 40 (cycle [100,200,300,400]))
  atomically $ submit (error "Boom")
  stop

-- threading\ example:1 ends here
