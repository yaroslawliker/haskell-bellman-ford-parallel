module RandomGraph (generateRandomGraph) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (nubBy)

import Graph(Graph(..), Arc(..), Node)



getRandomNode :: [Node] -> IO Node
getRandomNode nodes = do
    let nodeN = length nodes
    randI <- randomRIO (0, nodeN-1)

    let randNode = nodes !! randI
    return randNode


generateRandomArc :: [Node] -> Int -> Int -> IO Arc
generateRandomArc nodes minWeight maxWeight = do
    randU <- getRandomNode nodes
    randV <- getRandomNode nodes
    randW <- randomRIO (minWeight, maxWeight)

    if randU /= randV
        then return (Arc randU randW randV)
        else generateRandomArc nodes minWeight maxWeight


generateRandomNArcs :: [Node] -> Int -> Int -> Int -> IO [Arc]
generateRandomNArcs _ 0 _ _ = return []
generateRandomNArcs nodes arcN minWeight maxWeight =
    replicateM arcN (generateRandomArc nodes minWeight maxWeight)
-- more straigth forward solution:
--  do
    -- arc <-generateRandomArc nodes minWeight maxWeight
    -- arcs <- generateRandomNArcs nodes (arcN-1) minWeight maxWeight
    -- return (arc:arcs)

removeDuplicativeArcs :: [Arc] -> [Arc]
removeDuplicativeArcs = nubBy areEndNodesEqual
  where
    areEndNodesEqual :: Arc -> Arc -> Bool
    areEndNodesEqual (Arc u1 _ v1) (Arc u2 _ v2) =
      u1 == u2 && v1 == v2

generateRandomGraph :: Int -> Int -> Int -> Int -> Bool -> IO Graph
generateRandomGraph nodeN arcN minWeight maxWeight removeDublicates = do
    let nodes = [1..nodeN]

    arcs <- generateRandomNArcs nodes arcN minWeight maxWeight

    if removeDublicates
        then return (Graph (removeDuplicativeArcs arcs))
        else return (Graph arcs) 
    
