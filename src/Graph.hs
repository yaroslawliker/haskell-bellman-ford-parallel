module Graph (Node, Arc(..), Graph(..), arcs, getNodes) where

import Data.List

-- This modules define oriented graphs, and operations on them.
-- Negative values for nodes are forbidden.
-- Nodes with no arcs from/to them are firbidden.

-- Data
type Node = Int

data Arc = Arc Node Int Node

data Graph = Graph [Arc]

-- Getters
arcs :: Graph -> [Arc]
arcs (Graph as) = as

-- Instances
instance Show Arc where
  show (Arc from w to) = show from ++ " --(" ++ show w ++ ")--> " ++ show to

instance Show Graph where
  show (Graph as) =
    "Nodes:" ++ show (getNodes (Graph as)) ++ "\n" ++
    "Arcs:\n" ++ unlines (map show as)

-- Functions

arcsToNodes :: [Arc] -> [Node]
arcsToNodes [] = []
arcsToNodes ((Arc from w to ):as) = from : to : arcsToNodes as


getNodes :: Graph -> [Node]
getNodes (Graph as) = nub $ arcsToNodes as