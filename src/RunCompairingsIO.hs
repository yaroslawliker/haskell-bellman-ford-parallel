module RunCompairingsIO (runCompairing) where

import Data.Time.Clock
import System.Random (StdGen)

import BellmanFord (bellmanFord);
import ParallelBellmanFord (bellmanFordParralel)

import RandomGraph (generateRandomGraph)
import Control.DeepSeq (deepseq)

runCompairing :: Int -> StdGen -> IO (Double, Double)
runCompairing nodeN g = do

    -- Graph generation
    let arcN = nodeN*nodeN `div` 4

    print "------------"
    print "Running for:"
    print $ "Nodes: " ++ show nodeN
    print $ "Arcs: " ++ show arcN

    
    genStartTime <- getCurrentTime
    let graph = generateRandomGraph nodeN arcN (-1000) 1000 True g -- 1000*1000 = 1 000 000
    genEndTime <- deepseq graph getCurrentTime

    let genDuration = diffUTCTime genEndTime genStartTime
    print ("Graph generation time: " ++ show genDuration)

    -- Serial Bellman-Ford algorythm
    serialStartTime <- getCurrentTime
    let serialSol = bellmanFord graph 1
    serialEndTime <- deepseq serialSol getCurrentTime
    
    let serialDuration = diffUTCTime serialEndTime serialStartTime 
    print ("Serial Bellman-Ford time: " ++ show serialDuration)
    
    -- Parallel Bellman-Ford algorythm
    parallelStartTime <- getCurrentTime
    let parallelSol = bellmanFordParralel 4 graph 1
    parallelEndTime <- deepseq parallelSol getCurrentTime
    
    let parallelDuration = diffUTCTime parallelEndTime parallelStartTime 
    print ("Parallel Bellman-Ford time: " ++ show parallelDuration)

    return (realToFrac serialDuration :: Double, realToFrac parallelDuration :: Double)