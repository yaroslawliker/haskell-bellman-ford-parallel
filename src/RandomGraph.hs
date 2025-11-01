module RandomGraph (generateRandomGraph) where


import System.Random (StdGen,randomR)
import Data.List (nubBy, sortBy)
import Data.Ord (comparing)

import Graph(Graph(..), Arc(..), Node)


-- Generates random node from the given list and the generator
getRandomNode :: [Node] -> StdGen -> (Node, StdGen)
getRandomNode nodes g = (nodes !! randI, newG)
    where
        nodeN = length nodes
        (randI, newG) = randomR (0, nodeN-1) g


-- Generates v different from u
generateV :: [Node] -> Node -> StdGen -> (Node, StdGen)
generateV nodes u g =
    if vCandidate /= u
        then (vCandidate, newG)
        else generateV nodes u newG
    where
        nodeN = length nodes
        (vCandidate, newG) = randomR (1, nodeN) g

-- Generates a random Arc (u, w, v), where u /= v, and w within weightRange
generateRandomArc :: [Node] -> (Int, Int) -> StdGen -> (Arc, StdGen)
generateRandomArc nodes weightRange g =  (Arc randU randW randV, newG3)
    where
        (randU, newG1) = getRandomNode nodes g
        (randV, newG2) = generateV nodes randU newG1
        (randW, newG3) = randomR weightRange newG2   

-- Generates N random arcs from given nodes and weight between weightRange
generateRandomNArcs :: [Node] -> Int -> (Int, Int) -> StdGen -> ([Arc], StdGen)
generateRandomNArcs _ 0 _ g = ([], g)
generateRandomNArcs nodes arcN weightRange g =
    (arc : arcs, newG2)

    where
        (arc, newG) = generateRandomArc nodes weightRange g
        (arcs, newG2) = generateRandomNArcs nodes (arcN - 1) weightRange newG


-- Removes dublicative arcs.
-- Two arcs Arc1(u, w, v) and Arc2(u', w, v') are dublicative if
-- u == u', v == v'
removeDuplicativeArcs :: [Arc] -> [Arc]
removeDuplicativeArcs = nubBy areEndNodesEqual
  where
    areEndNodesEqual :: Arc -> Arc -> Bool
    areEndNodesEqual (Arc u1 _ v1) (Arc u2 _ v2) =
      u1 == u2 && v1 == v2

sortArcsByNode :: [Arc] -> [Arc]
sortArcsByNode = sortBy (comparing getSortKey)
    where
        getSortKey :: Arc -> (Node, Node)
        getSortKey (Arc u _ v) = (u, v)

-- Generates random graph with given parameters
-- Takes nodes amount, arcs amount, minimal weight, maximum weight,
-- whether remove duplicative arcs and the random generator
-- Returns a random graph
generateRandomGraph :: Int -> Int -> Int -> Int -> Bool -> StdGen -> Graph
generateRandomGraph nodeN arcN minWeight maxWeight removeDublicates g =
    if removeDublicates
        then Graph (removeDuplicativeArcs sorted)
        else Graph sorted
    
    where
        nodes = [1..nodeN]
        (arcs, newG) = generateRandomNArcs nodes arcN (minWeight, maxWeight) g
        sorted = sortArcsByNode arcs
