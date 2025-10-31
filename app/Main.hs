module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), CostMap, findCostsOfArcNodes, NodeCost (NodeCost), relaxCost, initCosts, relaxAllNodes, findDublicatesOfNode);


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
    let relaxed = relaxAllNodes inits graph
    print "Relaxed:"
    print relaxed
    print $ findDublicatesOfNode relaxed 4
    -- let relaxed2 = relaxAllNodes relaxed graph
    -- print "Relaxed:"
    -- print relaxed2
    

    return ()
