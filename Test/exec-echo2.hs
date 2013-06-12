module Main where

import qualified EchoBackground

main :: IO ()
main = do
       s <- EchoBackground.runString
       putStrLn s
       return ()
