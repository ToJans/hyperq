-- import System.Environment
--     ( getArgs )
import Hyperq.Dot
import Hyperq.EchoBackground

-- | usage 
-- new experiemnt combining iq rb and dot ideas

main :: IO ()
main = do
  -- (file:_) <- getArgs
  g <- importDotFile "dot/iqfeed.dot"
  let rbs = map commRB $ comm g
      
  return()

