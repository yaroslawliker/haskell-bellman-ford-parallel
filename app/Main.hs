module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), CostMap, NodeCost (NodeCost), initCosts, relaxAllNodes);


main :: IO ()
main = do
    putStrLn "Graph"

    -- 1 --(5)--> 2
    -- 1 --(7)--> 3
    -- 2 --(10)--> 4
    -- 3 --(1)--> 4
    -- 3 --(2)--> 5
    let graph = Graph [Arc 1 5 2, Arc 1 7 3, Arc 2 10 4, Arc 3 1 4, Arc 3 2 5]
    let inits = initCosts graph 1
    print inits  

    print "Relaxed #1:"
    let relaxed = relaxAllNodes inits graph
    print relaxed

    print "Relaxed #2:"
    let relaxed1 = relaxAllNodes relaxed graph
    print relaxed1

    return ()
