module Parallel.ParallelBellmanFord (bellmanFordParralel) where

import Control.Parallel.Strategies ( parMap, rdeepseq );

import Serial.Graph (Graph(..), Node, getNodes)
import Serial.BellmanFord (CostMap, relaxAllNodes, removeDuplicates, initCosts);



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


-- Paralelly relaxes the costmap n times splitting the work amoung procN processes
-- Takes the processors number, a costmap, a graph and relaxing iterations amount
-- Returns the relaxed costmap
relaxNTimesParallel :: Int -> CostMap -> Graph -> Int -> CostMap
relaxNTimesParallel _ costMap _ 0 = costMap
relaxNTimesParallel procN costMap graph n = relaxNTimesParallel 
    procN 
    (relaxAllNodesParallel procN costMap graph)
    graph 
    (n-1)

-- Parallel Bellmand-Ford shortest-path-finidng algorythm.
-- Takes the number of processos, a Graph and the starting Node
-- Returns a CostMap, consisting of NodeCosts (node, cost, parent) where
--     - node is the current node (id or number)
--     - cost is the weight of the path from the starting node to current
--     - parent is the previous node on the path from the statring node to the current
-- The algorythm does not account negative cycles.
bellmanFordParralel :: Int -> Graph -> Node -> CostMap 
bellmanFordParralel procN graph node =

    relaxNTimesParallel procN inits graph (n-1)

    where
        inits = initCosts graph node
        n = length $ getNodes graph
