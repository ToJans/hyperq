-- [[file:~/projects/hyperq/hyperq.org::*Iqtest][Iqtest:1]]

import Control.Concurrent
import System.Environment
import System.Exit
import Hyperq.Iqconnect

main :: IO ExitCode
main = do
  [file] <- getArgs
  _ <- forkIO (logon)
  threadDelay $ 1000000 * 10
  putStr "\ndelay finished\n"
  conFileTime "localhost" "5009" file
  return(ExitSuccess)

-- Iqtest:1 ends here
