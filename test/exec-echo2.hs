-- executable for echo testing
module Main where

import           System.Environment
    ( getArgs )

import qualified Hyperq.EchoBackground as E

main :: IO ()
main = do
    args <- getArgs
    s <- E.runString $ head args
    putStrLn s
    return ()
