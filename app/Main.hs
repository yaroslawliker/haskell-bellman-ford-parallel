module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), CostMap, findCostsOfArcNodes, NodeCost (NodeCost), relaxCost);


main :: IO ()
main = do
    putStrLn "Graph"
    -- 1 --10--> 2
    -- 1 -- 7--> 3
    -- 2 -- 5--> 3
    let graph = Graph [Arc 1 10 2, Arc 1 7 3, Arc 2 5 3]
    print graph

    let costMap = [NodeCost 1 (Cost 25) Nothing, NodeCost 2 (Cost 50) Nothing, NodeCost 3 Infinity Nothing ]

    -- Arc u->v 1 --(5)--> 2, cost(u) = 10, cost(v) = inf    => Should be 10+5, parent - 1
    print $ relaxCost (Arc 1 5 2) (Cost 10) (NodeCost 2 Infinity Nothing)
    -- Arc u->v 1 --(5)--> 2, cost(u) = 10, cost(v) = 11     => Should stay 11, parent - stay 33
    print $ relaxCost (Arc 11 5 22) (Cost 10) (NodeCost 22 (Cost 11) (Just 33))
    -- Arc u->v 1 --(5)--> 2, cost(u) = 10, cost(v) = 17     => Should be 10+5, parent - 111
    print $ relaxCost (Arc 111 5 222) (Cost 10) (NodeCost 222 (Cost 17) (Just 333))

    -- print (findCostsOfArcNodes (Arc 2 10 3) costMap )

    


    

    return ()
