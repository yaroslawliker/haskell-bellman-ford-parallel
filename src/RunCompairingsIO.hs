module RunCompairingsIO (runCompairing) where

import Data.Time.Clock
import System.Random (StdGen)

import BellmanFord (bellmanFord);
import ParallelBellmanFord (bellmanFordParralel)

import RandomGraph (generateRandomGraph)
import Control.DeepSeq (deepseq)
import Text.Printf (printf)

runCompairing :: Int -> StdGen -> IO (Double, Double)
runCompairing nodeN g = do

    -- Graph generation
    let arcN = nodeN*nodeN `div` 4

    printf $ replicate 50 '-' ++ "\n"
    printf $ "Running for:    " ++ show nodeN ++ " nodes    and    " ++ show arcN  ++ " arcs: " ++ "\n"

    
    -- Graph generation
    genStartTime <- getCurrentTime
    let graph = generateRandomGraph nodeN arcN (-1000) 1000 True g -- 1000*1000 = 1 000 000
    genEndTime <- deepseq graph getCurrentTime
    let genDuration = diffUTCTime genEndTime genStartTime


    -- Serial Bellman-Ford algorythm
    serialStartTime <- getCurrentTime
    let serialSol = bellmanFord graph 1
    serialEndTime <- deepseq serialSol getCurrentTime
    let serialDuration = diffUTCTime serialEndTime serialStartTime   


    -- Parallel Bellman-Ford algorythm
    parallelStartTime <- getCurrentTime
    let parallelSol = bellmanFordParralel 4 graph 1
    parallelEndTime <- deepseq parallelSol getCurrentTime
    
    let parallelDuration = diffUTCTime parallelEndTime parallelStartTime 

    -- Outputs
    printf $ "Graph generation: " ++ show genDuration ++ "\n"
    printf $ "Serial " ++ show serialDuration ++ "    Parallel " ++ show parallelDuration ++ "\n"

    return (realToFrac serialDuration :: Double, realToFrac parallelDuration :: Double)