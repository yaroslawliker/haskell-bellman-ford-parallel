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

    let costMap = [NodeCost 1 (Cost 25) Nothing, NodeCost 2 (Cost 50) Nothing, NodeCost 3 Infinity (Just 1) ]

    print (findCostsOfArcNodes (Arc 2 10 3) costMap )

    


    

    return ()
