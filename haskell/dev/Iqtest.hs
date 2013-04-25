-- [[file:~/projects/hft/hft.org::*Iqtest%20development][Iqtest\ development:1]]

import Control.Concurrent
import Network
import System.Environment
import System.Process
import System.IO
import Control.Exception
import System.Exit
import Control.Monad (forever)
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Text.Regex.TDFA

logon :: IO ()
logon = do
  let cmd = "wine"
      args = ["Z:\\Users\\tonyday\\wine\\iqfeed\\iqconnect.exe", "-product IQFEED_DEMO -version 1"]
  _ <- rawSystem cmd args
  return()







start :: String -> String -> IO ()
start host port = do
    h <- try (connectTo host $ PortNumber $ toEnum $ read port) :: IO (Either SomeException Handle)
    case h of
      Left ex -> case () of _ 
                              | "connect: does not exist" =~ show ex  -> logon
                              | otherwise -> putStrLn $ "Caught Exception: " ++ show ex
 
      Right val -> hGetContents val >>= putStr
    return ()



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



getCurrentTimeString :: IO String
getCurrentTimeString = do
   now <- getCurrentTime
   return (formatTime defaultTimeLocale "%H:%M:%S%Q" now)


main :: IO ExitCode
main = do
  [file] <- getArgs
  -- _ <- forkIO (logon)
  -- threadDelay $ 1000000 * 6
  -- putStr "\ndelay finished\n"
  conFileTime "localhost" "5009" file
  return(ExitSuccess)

-- Iqtest\ development:1 ends here
