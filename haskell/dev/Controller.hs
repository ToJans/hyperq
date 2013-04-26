-- [[file:~/projects/hyperq/hyperq.org::*Controller.hs][Controller\.hs:1]]

-- Example
--
-- $ ghc --make Controller.hs
-- $ ./Controller
import ControllerTest
import System.Environment
import Data.Maybe

main :: IO ()
main = do
     a <- getArgs
     let f = fromMaybe "../dot/candidate.dot" $ listToMaybe a 
     dotGraph <- importDotFile f 
     putStrLn "nodes:"
     putStrLn $ show $ nodeList dotGraph
     putStrLn "connections:"
     putStrLn $ show $ edgeList dotGraph
     return ()

-- Controller\.hs:1 ends here
