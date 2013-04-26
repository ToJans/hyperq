-- [[file:~/projects/hyperq/hyperq.org::*ControllerTest.hs][ControllerTest\.hs:1]]

module ControllerTest 
( importDotFile
, importDot
, printGraph
, nodeList
, edgeList
) where

import Data.GraphViz
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as I
import qualified Data.GraphViz.Types.Generalised as G
import Data.Graph.Inductive.Graph

importDotFile :: FilePath -> IO (G.DotGraph String)
importDotFile f = do
        dotText <- I.readFile f 
        return $ parseDotGraph dotText

importDot :: L.Text -> G.DotGraph Node
importDot s = parseDotGraph s

printGraph :: G.DotGraph String -> IO ()
printGraph d = do
        putStrLn $ L.unpack $ printDotGraph d
        return()

nodeList :: G.DotGraph String -> [String]
nodeList g = map nodeID $ graphNodes g

edgeList :: G.DotGraph String -> [(String,String)]
edgeList g =  map (\x -> (fromNode x, toNode x)) $ graphEdges g

-- ControllerTest\.hs:1 ends here
