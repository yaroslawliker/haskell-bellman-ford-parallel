module Parallel.ParallelBellmanFord (relaxAllNodesParallel, bellmanFordParralel) where

import Control.Parallel.Strategies;

import Serial.Graph (Graph(..), Node, getNodes)

import Serial.BellmanFord (
    CostMap(..),
    relaxAllNodes, removeDuplicates, initCosts);

-- Takes sub-costmap, a full graph and returns an update costmap
relaxAllNodesWrapper :: (CostMap, Graph) -> CostMap
relaxAllNodesWrapper (costMap, graph) = relaxAllNodes costMap graph


-- Relaxes all nodes, splitting the work amoung N processes
-- Takes numbe of processes, a costmap, a graph
-- Returns the updated costmap
relaxAllNodesParallel :: Int -> CostMap -> Graph -> CostMap
relaxAllNodesParallel procN costMap (Graph arcs) =
    removeDuplicates $ concat $ parMap rdeepseq relaxAllNodesWrapper subTasks
    where
        arcsPerProc = length arcs `div` procN
        subGraphs = [
                Graph subTask |
                let lastP = procN-1,
                p <- [0..lastP],
                let start = p*arcsPerProc,
                let dropped = drop start arcs,
                let subTask =
                        -- Check is needed to give the last process remaining arcs
                        if p == lastP
                            then dropped
                            else take arcsPerProc dropped
            ]
        costMaps = replicate procN costMap

        subTasks = zip costMaps subGraphs

