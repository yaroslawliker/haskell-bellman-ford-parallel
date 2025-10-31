module Main where

import Graph (Arc(..), Graph(..));
import BellmanFord (bellmanFord);
import ParallelBellmanFord (bellmanFordParralel)


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

    let serialSol = bellmanFord graph 1
    print serialSol

    let parallelSol = bellmanFordParralel 4 graph 1
    print parallelSol


    return ()
