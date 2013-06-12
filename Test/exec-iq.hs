import Control.Concurrent
import System.Environment
import System.Exit
import Hyperq.Iqconnect

-- $ ./cabal-dev/bin/iq "test.txt"

main :: IO ExitCode
main = do
  [file] <- getArgs
  _ <- forkIO logon
  threadDelay $ 1000000 * 10
  putStr "\ndelay finished\n"
  conFileTime "localhost" "5009" file
  return ExitSuccess
