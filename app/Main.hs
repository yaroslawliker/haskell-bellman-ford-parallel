module Main where

import Serial.Graph (Arc(..), Graph(..), addArc, removeArc);


main :: IO ()
main = do
    putStrLn "Graph"
    -- 1 --10--> 2
    -- 1 -- 7--> 3
    -- 2 -- 5--> 3
    let graph = Graph [Arc 1 10 2, Arc 1 7 3, Arc 2 5 3]

     -- 2 -- 5--> 4
    let graph1 = addArc (Arc 2 11 4) graph;
    let graph2 = removeArc 1 graph1;
    print graph2
    return ()
