module Main where

import Serial.Graph (Node, Arc(..), Graph(..));
import Serial.BellmanFord (Cost(..), initCosts, relaxCost);


main :: IO ()
main = do
    putStrLn "Graph"
    -- 1 --10--> 2
    -- 1 -- 7--> 3
    -- 2 -- 5--> 3
    let graph = Graph [Arc 1 10 2, Arc 1 7 3, Arc 2 5 3]
    print graph

    print $ initCosts graph 1

    -- Arc u->v 1 --(5)--> 2, cost(u) = 10, cost(v) = inf    => Should be 10+5
    print $ relaxCost (Arc 1 5 2) (Cost 10) Infinity
    -- Arc u->v 1 --(5)--> 2, cost(u) = 10, cost(v) = 11     => Should stay 11
    print $ relaxCost (Arc 1 5 2) (Cost 10) (Cost 11)
    -- Arc u->v 1 --(5)--> 2, cost(u) = 10, cost(v) = 17     => Should be 10+5
    print $ relaxCost (Arc 1 5 2) (Cost 10) (Cost 17)


    

    return ()
