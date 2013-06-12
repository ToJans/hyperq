module Dot
( importDot
, importDotFile
, printGraph
, nodeList
, edgeList
, comp
, dir
, comm
, commChan
, commRB
, comb
, buffs
) where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
-- import Data.GraphViz.Types
-- import qualified Data.Text.Lazy.Internal as Int
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as I
-- import qualified Data.GraphViz.Types.Generalised as G
-- import Data.Graph.Inductive.Graph
import Data.List (union)
import qualified Data.Map as Map
import Control.Arrow

importDot :: String -> DotGraph String
importDot = parseDotGraph . L.pack

importDotFile :: FilePath -> IO (DotGraph String)
importDotFile f = do
        dotText <- I.readFile f 
        return $ parseDotGraph dotText

printGraph :: DotGraph String -> IO ()
printGraph d = do
        putStrLn $ L.unpack $ printDotGraph d
        return()

nodeList :: DotGraph String -> [String]
nodeList g = map nodeID $ graphNodes g

edgeList :: DotGraph String -> [(String,String)]
edgeList g =  map (fromNode &&& toNode) $ graphEdges g

data CompType = Internal 
              | External
              deriving Show

comp :: DotGraph String -> [(String, CompType)]
comp g = zip name s where
    name = map nodeID n
    s =  map (compType . nodeAttributes) n
    n = toDotNodes $ nodeInformationClean True g

toDotNodes :: (Ord n) => NodeLookup n -> [DotNode n]
toDotNodes = map (\(n,(_,as)) -> DotNode n as) . Map.assocs

dir :: [Attribute] -> DirType
dir [] = Forward
dir x = 
    let d = [a | Dir a <- x]
    in case d of
       [] -> Forward
       _ -> head d

compType :: [Attribute] -> CompType
compType [] = Internal
compType x = 
    let d = [a | Shape a <- x]
    in case d of
       [] -> Internal
       [Egg] -> External
       _ -> Internal

comm :: DotGraph String -> [(String, String, DirType)]
comm g = zip3 from to d where 
     e = edgeInformationClean True g
     d = map (dir.edgeAttributes) e
     from = map fromNode e
     to = map toNode e

commChan :: (String, String, DirType) -> [(String, String, String)]
commChan (f,t,Both)          = [(f,t,"Write")
                               ,(f,t,"Read")]
commChan (f,t,Forward)       = [(f,t,"Write")]
commChan (f,t,Back)          = [(f,t,"Read")]
commChan _                   = []

commRB :: (String, String, DirType) -> [(String, String)]
commRB (f,t,Both)          = [(f,t)
                             ,(t,f)]
commRB (f,t,Forward)       = [(f,t)]
commRB (f,t,Back)          = [(t,f)]
commRB _                   = []

comb :: [(String, String)] -> Map.Map String [String]
comb [] = Map.empty
comb (x:xs) = Map.insertWith union (fst x) [snd x] (comb xs)

buffs :: DotGraph String -> Map.Map String [String]
buffs g = comb $ concatMap commRB $ comm g
