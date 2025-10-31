module Parallel.ParallelBellmanFord (relaxAllNodesParallel) where

import Control.Parallel.Strategies;

import Serial.Graph (Graph(..))

import Serial.BellmanFord (
    CostMap(..),
    relaxCost, relaxAllNodes, removeDuplicates);

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
        arcsPerProc = length costMap `div` procN
        -- Example
        -- 1 2 3 4 5 6 7 8 9 arcs
        -- nodesN = 4;
        --
        -- arcsPerProc = 9 / 4 = 2
        --
        -- p = 0, start = 0*2 = 0
        -- drop 0 -> 1 2 3 4 5 6 7 8 9
        -- take 2 -> 1 2
        --
        -- p = 1, start = 1*2 = 2
        -- drop 2 -> 3 4 5 6 7 8 9
        -- take 2 -> 3 4
        --
        -- p = 2, start = 2*2 = 4
        -- drop 4 -> 5 6 7 8 9
        -- take 2 -> 5 6
        -- 
        -- p = 3, start = 3*2 = 6
        -- drop 6 -> 7 8 9
        -- -> 7 8 9

        subGraphs = [
                Graph subTask |
                p <- [0..(procN-1)],
                let start = p*arcsPerProc,
                let dropped = drop start arcs,
                let subTask =
                        -- Check is needed to give the last process remaining arcs
                        if length dropped < arcsPerProc*2
                            then dropped
                            else take arcsPerProc dropped
            ]
        costMaps = replicate procN costMap

        subTasks = zip costMaps subGraphs

