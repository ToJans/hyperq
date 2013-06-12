{-# LANGUAGE DeriveDataTypeable #-}
module EchoBackground (run
                     , runString
                     , concBatchPublishTo') where

import           Control.Applicative    ((*>))
import           Control.Concurrent     ( newEmptyMVar
                                        , putMVar
                                        , takeMVar
                                        , readMVar
                                        , MVar
                                        , killThread
                                        )
import           Control.Monad
-- import Control.DeepSeq
import           Background
import           Control.Concurrent.STM
import           Control.Exception.Base
import           Data.Char
import           Data.RingBuffer
import           Data.RingBuffer.Internal
import           Data.RingBuffer.Types
import           Data.RingBuffer.Vector
import           Data.Typeable
import           Util

-- import Data.Time
-- import System.Random
-- import Debug.Trace

import qualified Data.Vector.Mutable               as MV
import           Data.Bits
import qualified Data.Vector.Fusion.Stream.Monadic as S
import           Data.Vector.Generic.Mutable       (mstream)

{-
Replication of echo using the ringbuffer.

Sequencer: an stdin listener
Consumer: an stdout writer

writer just echos till it gets a q, then shuts everything down.

also incorporate pooling concepts

-}

concPublishTo' :: MVector Int -> Int -> Sequencer -> Int -> IO ()
concPublishTo' (MVector mvec) modm seqr@(Sequencer sq _) v = do
    nextm <- nextSeq seqr sq (MV.length mvec)
    -- _ <- traceIO ("\npub slot, value " ++ (show nextm)  ++ " " ++ (show (toEnum v :: Char)))
                  
    MV.unsafeWrite mvec (nextm .&. modm) v
    writeSeq sq nextm
{-# INLINE concPublishTo' #-}

concBatchPublishTo' :: MVector a -> Int -> Sequencer -> [a] -> IO ()
concBatchPublishTo' (MVector mvec) modm seqr@(Sequencer sq _) vs = do
    next <- nextBatch seqr sq len (MV.length mvec)

    mapM_ update $ zip [next - len + 1..next] vs
    publish seqr next len

    where
        len = length vs
        update (n,x) = MV.unsafeWrite mvec (n .&. modm) x
{-# INLINE concBatchPublishTo' #-}

consumeFrom' :: MVector a -> Int -> Barrier -> Consumer a -> IO ()
consumeFrom' (MVector mvec) modm barr (Consumer fn sq) = do
    upto  <- readSeq sq
    avail <- waitFor barr upto

    let start = upto .&. modm
        len   = avail - upto
        (_,t) = MV.splitAt (start+1) mvec
        tlen  = MV.length t

    -- when (len > 0) $ do
      -- traceIO ("\ncon cursor, len, tlen " ++ (show upto)  ++ " " ++ (show len) ++ " " ++ (show tlen))
    S.mapM_ fn . mstream . MV.take len $ t
    unless (tlen >= len) $
        S.mapM_ fn . mstream . MV.take (len - tlen) $ mvec

    writeSeq sq avail
{-# INLINE consumeFrom' #-}


printOrDone :: MVar Int -> Int -> IO ()
printOrDone done x
  | toEnum x == 'q' = putMVar done 1
  | otherwise = putChar $ toEnum x
                -- $ trace ("\nwriting a char " ++ (show (toEnum x :: Int))) x

printOrDone' :: MVar Int -> MVector Int -> MVar Int -> Int -> IO ()
printOrDone' done (MVector ans) mvc x
  | toEnum x == 'q' = putMVar done 1
  | otherwise = do
                c <- readMVar mvc
                _ <- MV.unsafeWrite ans c x                
                _ <- putMVar mvc (c + 1)
                return()

data MyException = ThisException | ThatException
                 deriving (Show, Typeable)
instance Exception MyException

run :: IO ()
run = do
  done  <- newEmptyMVar
  con   <- newConsumer (printOrDone done)
  seqr  <- newSequencer [con]
  buf   <- newRingBuffer bufferSize (0 :: Int)
  start <- now
  (submit,_,ids) <- spawnWorkers 2
  mapM_ (\x -> print $ ' ' : show x) ids
  atomically . submit . forever
    $ do
      c <- getChar
      concPublishTo' buf modmask seqr $ fromEnum c

  atomically . submit . forever
    $ consumeFrom' buf modmask (newBarrier seqr []) con
  -- atomically $ submit (error "Boom")
  -- takeMVar done *> now >>= printTiming' start >> stop >> mapM_ (flip throwTo ThisException) ids
  -- stop
  takeMVar done *> now >>= printTiming' start >> mapM_ killThread ids
  -- takeMVar done *> mapM killThread ids
    
  -- replicateM_ 2 (atomically $ submit (error "Boom"))
  -- stop

  where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

runString :: IO String
runString = do
  answer <- do
      m <- MV.replicate 100 0
      return(MVector m)
  count <- newEmptyMVar
  done  <- newEmptyMVar
  con   <- newConsumer (printOrDone' done answer count)
  seqr  <- newSequencer [con]
  buf   <- newRingBuffer bufferSize (0 :: Int)
  start <- now
  (submit,_,ids) <- spawnWorkers 2
  mapM_ (\x -> print $ ' ' : show x) ids
  atomically . submit . forever $ do
              mapM_ (concPublishTo' buf modmask seqr) $ map fromEnum "This is a test string that will be published one character at a time until a q is reached"
              return()

  atomically . submit . forever
    $ consumeFrom' buf modmask (newBarrier seqr []) con

  takeMVar done *> now >>= printTiming' start >> mapM_ killThread ids
  c <- readMVar count
  let (MVector mvec) = answer
      (t,_) = MV.splitAt (c+1) mvec
  s <- MV.unsafeRead t 1
  return [chr s]

  where
        bufferSize = 1024*8
        modmask    = bufferSize - 1
