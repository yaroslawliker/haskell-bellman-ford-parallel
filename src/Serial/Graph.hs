module Serial.Graph (Node, Arc(..), Graph(..), arcs, getNodes, addArc, removeArc) where

import Data.List

-- This modules define oriented graphs, and operations on them.
-- Negative values for nodes are forbidden.
-- Nodes with no arcs from/to them are firbidden.

----------
-- Data --
----------
type Node = Int

data Arc = Arc Node Int Node

newtype Graph = Graph [Arc]

---------------
-- Functions --
---------------

-- Getters
arcs :: Graph -> [Arc]
arcs (Graph as) = as

--
-- Instances
--
instance Show Arc where
  show (Arc from w to) = show from ++ " --(" ++ show w ++ ")--> " ++ show to

instance Show Graph where
  show (Graph as) =
    "Nodes:" ++ show (getNodes (Graph as)) ++ "\n" ++
    "Arcs:\n" ++ unlines (map show as)

--
-- Operations
--
arcsToNodes :: [Arc] -> [Node]
arcsToNodes [] = []
arcsToNodes ((Arc from _ to ):as) = from : to : arcsToNodes as

-- Gets unique nodes from the graph in ascending order
getNodes :: Graph -> [Node]
getNodes (Graph as) = sort . nub $ arcsToNodes as

addArc :: Arc -> Graph -> Graph
addArc newArc (Graph as) = Graph (newArc:as)

removeArc :: Int -> Graph -> Graph
removeArc i (Graph as) = Graph (take i as ++ drop (i+1) as)
