module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), CostMap, NodeCost (NodeCost), initCosts, relaxNTimes, bellmanFord, relaxAllNodes);
import Parallel.ParallelBellmanFord (relaxAllNodesParallel)


main :: IO ()
main = do
    putStrLn "Graph"

    -- 1 --(2)--> 2
    -- 1 --(5)--> 4
    -- 1 --(10)--> 6
    -- 2 --(4)--> 3
    -- 3 --(3)--> 5
    -- 4 --(8)--> 5
    -- 5 --(-4)--> 6
    let graph = Graph [Arc 1 2 2, Arc 1 5 4, Arc 1 10 6, Arc 2 4 3, Arc 3 3 5, Arc 4 8 5, Arc 5 (-4) 6]

    print "Inits:"
    let inits = initCosts graph 1
    print inits

    print "Relaxed (Serial):"
    let relaxedSerial = relaxAllNodes inits graph
    print relaxedSerial   

    print "Relaxed (Parallel):"
    let relaxedParallel = relaxAllNodesParallel 3 inits graph
    print relaxedParallel
    
    print "Relaxed #2 (Serial):"
    let relaxedSerial1 = relaxAllNodes relaxedSerial graph
    print relaxedSerial1

    print "Relaxed #2 (Parallel):"
    let relaxedParallel1 = relaxAllNodesParallel 3 relaxedParallel graph
    print relaxedParallel1

    print "Relaxed #3 (Serial):"
    let relaxedSerial2 = relaxAllNodes relaxedSerial1 graph
    print relaxedSerial2

    print "Relaxed #3 (Parallel):"
    let relaxedParallel2 = relaxAllNodesParallel 3 relaxedParallel1 graph
    print relaxedParallel2

    return ()
