module Main where

import Graph (Arc(..), Graph(..));
import BellmanFord (bellmanFord);
import ParallelBellmanFord (bellmanFordParralel)

import RandomGraph (generateRandomGraph)


main :: IO ()
main = do
    putStrLn "Graph"

    graph <- generateRandomGraph 100 500 0 20 True
    
    print graph

    print (bellmanFord graph 1)


    return ()
