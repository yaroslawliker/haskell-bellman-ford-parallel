module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), CostMap, NodeCost (NodeCost), initCosts, relaxNTimes, bellmanFord);


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

    print "Solved:"
    let solved = bellmanFord graph 1
    print solved
    
    return ()
