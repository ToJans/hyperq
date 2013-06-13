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
    ( newEmptyMVar, putMVar , takeMVar, readMVar, MVar, killThread )
import Control.Concurrent.STM
    ( atomically )
import Control.Monad
    ( unless, forever )
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
    ( nextBatch, publish, waitFor, newConsumer, newSequencer, newBarrier ) 
import           Data.RingBuffer.Internal
    ( readSeq, writeSeq )
import           Data.RingBuffer.Types
    ( Barrier(..), Consumer(..), Sequencer(..) )
import           Data.RingBuffer.Vector
    ( MVector(..), concPublishTo , newRingBuffer)

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
{-# INLINE concPublishTo' #-}

-- not sure why the original function uses an explicit length
concBatchPublishTo' :: MVector a -- ^ buffer
                    -> Int       -- ^ mod mask
                    -> Sequencer -- ^ publisher and cursor
                    -> [a]       -- ^ values to be published
                    -> IO ()
concBatchPublishTo' (MVector mvec) modm seqr@(Sequencer sq _) vs = do
    next <- nextBatch seqr sq len (MV.length mvec)
    mapM_ update $ zip [next - len + 1..next] vs
    publish seqr next len
  where
    len = length vs
    update (n,x) = MV.unsafeWrite mvec (n .&. modm) x
{-# INLINE concBatchPublishTo' #-}

-- | Consume from buffer. Altered consumeFrom to reference 
-- last consumed (upto) rather
-- than next to be consumed (next)
consumeFrom' :: MVector a  -- ^ buffer 
             -> Int        -- ^ mod mask
             -> Barrier    -- ^ tracking cursors
             -> Consumer a -- ^ a stream consumer (with cursor) 
             -> IO ()
consumeFrom' (MVector mvec) modm barr (Consumer fn sq) = do
    upto  <- readSeq sq
    avail <- waitFor barr upto

    let start = upto .&. modm
        len   = avail - upto
        (_,t) = MV.splitAt (start+1) mvec
        tlen  = MV.length t

    -- traceIO ("\ncon cursor, len, tlen " ++ (show upto)  ++ " " ++ 
    --     (show len) ++ " " ++ (show tlen))
    S.mapM_ fn . mstream . MV.take len $ t
    unless (tlen >= len) $
        S.mapM_ fn . mstream . MV.take (len - tlen) $ mvec

    writeSeq sq avail
{-# INLINE consumeFrom' #-}

-- | Prints to stdout unless it encounters a 'q', in which case 
-- it triggers shutdown
printOrDone :: MVar Int -- ^ shutdown flag 
            -> Int      -- ^ value
            -> IO ()
printOrDone done x
    | toEnum x == 'q' = putMVar done 1
    | otherwise = putChar $ toEnum x
                -- trace ("\nwriting a char " ++ (show (toEnum x :: Int))) x

-- | Print to an MVector unless 'q' happens
printOrDone' :: MVar Int    -- ^ shutdown flag 
             -> MVector Int -- ^ vector being printed to
             -> MVar Int    -- ^ vector size
             -> Int         -- ^ value
             -> IO ()
printOrDone' done (MVector ans) mvc x
    | toEnum x == 'q' = putMVar done 1
    | otherwise = do
                c <- readMVar mvc
                _ <- MV.unsafeWrite ans c x                
                _ <- putMVar mvc (c + 1)
                return()

-- | test run using stdin and stdout
-- todo: refactor run and runString
-- - answer and count associate with con and with final IO cleanup
run :: IO ()
run = do
    done  <- newEmptyMVar
    con   <- newConsumer (printOrDone done)
    seqr  <- newSequencer [con]
    buf   <- newRingBuffer bufferSize (0 :: Int)
    start <- now
    (submit,_,ids) <- spawnWorkers 2
    mapM_ (\x -> print $ ' ' : show x) ids
    atomically . submit . forever $ do
        c <- getChar
        concPublishTo' buf modmask seqr $ fromEnum c
    atomically . submit . forever
        $ consumeFrom' buf modmask (newBarrier seqr []) con
    takeMVar done *> now >>= printTiming' start >> mapM_ killThread ids
  where
    bufferSize = 1024*8
    modmask    = bufferSize - 1

-- | test run using MVector
-- todo: refactor run and runString
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
    -- mapM_ (\x -> print $ ' ' : show x) ids
    atomically . submit . forever $ do
        mapM_ (concPublishTo' buf modmask seqr . fromEnum) 
            "This is a test string"
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
