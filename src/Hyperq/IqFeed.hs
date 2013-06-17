module Hyperq.IqFeed (
    con
  , conFileTime
  , conAdmin
  , conStream
  , conLookup
  , logon
  , getCurrentTimeString
  , demo
  ) where

import Control.Concurrent
    ( newEmptyMVar, forkIO, tryPutMVar, takeMVar, threadDelay )
import Control.Exception
    ( finally )
import Control.Monad 
    ( forever )
import Data.Time.Calendar
    ( Day(..) )
import Data.Time.Clock
    ( getCurrentTime, diffUTCTime, addUTCTime, UTCTime(..), secondsToDiffTime )
import Data.Time.Format
    ( formatTime )
import Network as Net
import System.Environment
    ( getArgs )
import System.Exit
    ( ExitCode(..) )
import System.IO as IO
import System.Locale
    ( defaultTimeLocale )
import System.Process
    ( rawSystem )

con :: String -> String -> IO ()
con host port = do
    h <- Net.connectTo host $ Net.PortNumber $ toEnum $ read port
    hSetBuffering stdout LineBuffering
    hSetBuffering h      LineBuffering
    done <- newEmptyMVar

    _ <- forkIO $ (hGetContents h >>= putStr)
                `finally` tryPutMVar done ()

    _ <- forkIO $ (getContents >>= hPutStr h)
                `finally` tryPutMVar done ()

                -- Wait for at least one of the above threads to complete
    takeMVar done

conFileTime :: String -> String -> String -> IO ()
conFileTime host port file = do
    h <- Net.connectTo host $ Net.PortNumber $ toEnum $ read port
    f <- openFile file WriteMode
    hSetBuffering stdout LineBuffering
    hSetBuffering h      LineBuffering
    hSetBuffering f      LineBuffering
    done <- newEmptyMVar

    _ <- forkIO $ forever (do
        t <- getCurrentTimeString
        st <- hGetLine h
        hPutStrLn f $ t ++ "," ++ st)
            `finally` tryPutMVar done ()

    _ <- forkIO $ (getContents >>= hPutStr h)
            `finally` tryPutMVar done ()

                -- Wait for at least one of the above threads to complete
    takeMVar done

conAdmin :: String -> IO ()
conAdmin cmds = do
    con "localhost" "9300"
    putStr cmds

conStream :: String -> IO ()
conStream cmds = do
    con "localhost" "5009"
    putStr cmds

conLookup :: String -> IO ()
conLookup cmds = do
    con "localhost" "9100"
    putStr cmds

logon :: IO ()
logon = do
    let cmd = "wine"
        args = ["Z:\\Users\\tonyday\\wine\\iqfeed\\iqconnect.exe", "-product IQFEED_DEMO -version 1"]
    _ <- rawSystem cmd args
    return()


getCurrentTimeString :: IO String
getCurrentTimeString = do
     now <- getCurrentTime
     let offset = diffUTCTime  (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)) (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime (4 * 60 * 60)))
     return (formatTime defaultTimeLocale "%H:%M:%S%Q" $ addUTCTime offset now)


demo :: IO ExitCode
demo = do
    [file] <- getArgs
    _ <- forkIO logon
    threadDelay $ 1000000 * 10
    putStr "\ndelay finished\n"
    conFileTime "localhost" "5009" file
    return ExitSuccess
