module Main where

import Data.Time.Clock

import Graph (Arc(..), Graph(..));
import BellmanFord (bellmanFord);
import ParallelBellmanFord (bellmanFordParralel)

import RandomGraph (generateRandomGraph)
import System.Random (mkStdGen)


main :: IO ()
main = do
    putStrLn "Graph"

    -- Graph generation
    genStartTime <- getCurrentTime
    let nodeN = 50000
    let randomGenerator = mkStdGen 42
    let graph = generateRandomGraph nodeN (nodeN*nodeN `div` 4) 0 20 True randomGenerator -- 1000*1000 = 1 000 000
    genEndTime <- getCurrentTime

    let genDuration = diffUTCTime genEndTime genStartTime
    print ("Graph generation time: " ++ show genDuration)


    -- Serial Bellman-Ford algorythm
    serialStartTime <- getCurrentTime
    let serialSol = bellmanFord graph 1
    serialEndTime <- getCurrentTime
    
    let serialDuration = diffUTCTime serialStartTime serialEndTime
    print ("Sirial Bellman-Ford time: " ++ show serialDuration)

    
    -- Parallel Bellman-Ford algorythm
    parallelStartTime <- getCurrentTime
    let parallelSol = bellmanFordParralel 4 graph 1
    parallelEndTime <- getCurrentTime
    
    let parallelDuration = diffUTCTime parallelStartTime parallelEndTime
    print ("Sirial Bellman-Ford time: " ++ show parallelDuration)

    return ()
