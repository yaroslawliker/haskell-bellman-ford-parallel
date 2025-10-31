module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), CostMap, NodeCost (NodeCost), initCosts, relaxNTimes, bellmanFord);


main :: IO ()
main = do
    putStrLn "Graph"

    -- 1 --(5)--> 2
    -- 1 --(7)--> 3
    -- 2 --(10)--> 4
    -- 3 --(1)--> 4
    -- 3 --(2)--> 5
    -- 5 --(4)--> 6
    let graph = Graph [Arc 1 5 2, Arc 1 7 3, Arc 2 10 4, Arc 3 1 4, Arc 3 2 5, Arc 5 4 6]

    print "Solved:"
    let solved = bellmanFord graph 3
    print solved
    
    return ()
