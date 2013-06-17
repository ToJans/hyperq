{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module: EchoBackground
-- Experimentation with data-ringbuffer
--
-- This run replicates a simple echo using:
--
-- - Data.RingBuffer
-- - Background: a worker pool of background threads

module Hyperq.EchoBackground (
   -- * testing imperatives
     run
   , runString

   -- * unused functions
   , concBatchPublishTo'
   ) where

import Control.Applicative
    ( (*>) )
import Control.Concurrent
    ( newEmptyMVar, putMVar , takeMVar, MVar, killThread )
import Control.Concurrent.STM
    ( atomically )
import Control.Monad
    ( unless, forever, when )
import Data.Bits
    ( (.&.) )
import Data.Char
    ( chr )
import qualified Data.Vector.Fusion.Stream.Monadic as S
import           Data.Vector.Generic.Mutable
    (mstream)
import qualified Data.Vector.Mutable               as MV

-- Data.RingBuffer (https://github.com/hyperq/data-ringbuffer) is a fork of
-- https://github.com/kim/data-ringbuffer to expose all modules
import           Data.RingBuffer
    ( nextBatch, publish, waitFor, newConsumer, newSequencer, newBarrier
    , nextSeq )
import           Data.RingBuffer.Internal
    ( writeSeq, readSeq, addAndGet, mkSeq )
import           Data.RingBuffer.Types
    ( Barrier(..), Consumer(..), Sequencer(..), Sequence )
import           Data.RingBuffer.Vector
    ( MVector(..), newRingBuffer)

-- local helper modules
import           Hyperq.Background
    ( spawnWorkers )
import           Util
    ( now, printTiming' )

-- | Publish Wrapper for Data.RingBuffer.Vector function,
-- to extract the Sequence from Sequencer.
concPublishTo' :: MVector Int -- ^ publishing buffer
               -> Int         -- ^ mod mask
               -> Sequencer   -- ^ contains the cursor
               -> Int         -- ^ value to be published
               -> IO ()
concPublishTo' mv modm seqr@(Sequencer sq _) =
    concPublishTo mv modm seqr sq

concPublishTo :: MVector Int -> Int -> Sequencer -> Sequence -> Int -> IO ()
concPublishTo (MVector mvec) modm seqr sq v = do
    next <- nextSeq seqr sq (MV.length mvec)
    MV.unsafeWrite mvec (next .&. modm) v
    publish seqr next 1


-- not sure why the original function uses an explicit length
concBatchPublishTo' :: MVector a -- ^ buffer
                    -> Int       -- ^ mod mask
                    -> Sequencer -- ^ publisher and cursor
                    -> [a]       -- ^ values to be published
                    -> IO ()
concBatchPublishTo' (MVector mvec) modm seqr@(Sequencer sq _) vs = do
    next <- nextBatch seqr sq len (MV.length mvec)
    mapM_ update $ zip [next - len + 1..next] vs
    publish seqr (next+1) len
  where
    len = length vs
    update (n,x) = MV.unsafeWrite mvec (n .&. modm) x

-- | Consume from buffer. Altered consumeFrom to reference
-- last consumed (upto) rather
-- than next to be consumed (next)
consumeFrom' :: MVector Int  -- ^ buffer
             -> Int        -- ^ mod mask
             -> Barrier    -- ^ tracking cursors
             -> Consumer Int -- ^ a stream consumer (with cursor)
             -> IO ()
consumeFrom' (MVector mvec) modm barr (Consumer fn sq) = do
    next  <- addAndGet sq 1
    avail <- waitFor barr next
    let start = next .&. modm
        len   = avail - next + 1
        (_,t) = MV.splitAt start mvec
        tlen  = MV.length t
    S.mapM_ fn $ mstream $ MV.take len t
    unless (tlen >= len) $
        S.mapM_ fn . mstream . MV.take (len - tlen) $ mvec

    writeSeq sq avail

-- | Prints to stdout unless it encounters a 'q', in which case 
-- it triggers shutdown
printOrDone :: MVar Int -- ^ shutdown flag
            -> Int      -- ^ value
            -> IO ()
printOrDone done x
    | toEnum x == 'q' = putMVar done 1
    | otherwise = putChar $ toEnum x

-- | Print to an MVector unless 'q' happens
printOrDone' :: MVector Int -- ^ vector being printed to
             -> Sequence    -- ^ vector size
             -> MVar Int    -- ^ shutdown flag
             -> Int         -- ^ value
             -> IO ()
printOrDone' (MVector ans) mvc done x = do
    -- print "in printOrDone'"
    c <- readSeq mvc
    _ <- MV.unsafeWrite ans (c+1) x
    _ <- writeSeq mvc (c + 1)
    when (x == 113) $
        putMVar done 1
    return()

-- | test run using stdin and stdout
-- answer and count associate with con and with final IO cleanup
run :: IO ()
run = do
    done  <- newEmptyMVar
    (con, seqr, buf, start) <- makeRb bufferSize (printOrDone done)
    (submit,_,ids) <- spawnWorkers 2
    atomically . submit . forever $ do
        c <- getChar
        concPublishTo' buf modmask seqr $ fromEnum c
    atomically . submit . forever
        $ consumeFrom' buf modmask (newBarrier seqr []) con
    takeMVar done *>
        now >>= printTiming' start >>
        mapM_ killThread ids

  where
    bufferSize = 1024*8
    modmask    = bufferSize - 1

-- | test run using MVector
runString :: String -> IO String
runString s = do
    answer <- do
        m <- MV.replicate 100 0
        return(MVector m)
    count <- mkSeq
    done  <- newEmptyMVar
    (con, seqr, buf, start) <- makeRb bufferSize
                               (printOrDone' answer count done)
    (submit,_,ids) <- spawnWorkers 3
    atomically . submit $ mapM_
        (concPublishTo' buf modmask seqr . fromEnum) s
    atomically . submit . forever
        $ consumeFrom' buf modmask (newBarrier seqr []) con
    takeMVar done *>
        now >>= printTiming' start >>
        mapM_ killThread ids
    c <- readSeq count
    let (MVector mvec) = answer
        (t,_) = MV.splitAt (c+1) mvec
    sOut <- mapM (MV.read t) [0..c]
    return $ map chr sOut
  where
    bufferSize = 1024*8
    modmask    = bufferSize - 1

makeRb :: Int -> (a -> IO ()) ->
          IO (Consumer a, Sequencer, MVector Int, Double)
makeRb bufferSize conFn = do
    con   <- newConsumer conFn
    seqr  <- newSequencer [con]
    buf   <- newRingBuffer bufferSize (0 :: Int)
    start <- now
    return(con, seqr, buf, start)
