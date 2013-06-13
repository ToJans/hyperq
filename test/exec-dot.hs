import System.Environment
    ( getArgs )
import qualified Hyperq.Dot as Dot
-- | usage 
-- $ ./cabal-dev/bin/dot dot/iqcontroller.dot 

main :: IO ()
main = do
  (file:_) <- getArgs
  g <- Dot.importDotFile file
  print $ map Dot.commChan $ Dot.comm g
  return()
