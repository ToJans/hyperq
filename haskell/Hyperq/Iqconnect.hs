-- [[file:~/projects/hyperq/hyperq.org::*Hyperq.Iqconnect][Hyperq\.Iqconnect:1]]

module Hyperq.Iqconnect where 

import Control.Concurrent
import Network
import System.Process
import System.IO
import Control.Exception
import Control.Monad (forever)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import System.Locale

con :: String -> String -> IO ()
con host port = do
    h <- connectTo host $ PortNumber $ toEnum $ read port
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
    h <- connectTo host $ PortNumber $ toEnum $ read port
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

-- Hyperq\.Iqconnect:1 ends here
