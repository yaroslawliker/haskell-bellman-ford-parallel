module Serial.BellmanFord (Cost(..), CostMap, NodeCost(..), initCosts, relaxNTimes, bellmanFord ) where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Serial.Graph (Graph, Node, Arc(..), Graph(..), getNodes);

data Cost = Cost Int | Infinity deriving (Show, Eq, Ord)

-- NodeCost, fields for Current node, cost to reach from s, parent node
data NodeCost = NodeCost Node Cost (Maybe Node) deriving(Show, Eq)

instance Ord NodeCost where
  (NodeCost _ cost1 _) <= (NodeCost _ cost2 _) = cost1 <= cost2

type CostMap = [NodeCost]


-- Initialize costs for Node
initCosts :: Graph -> Node -> CostMap
initCosts (Graph as) n =
    [NodeCost x (
        if x /= n
            then Infinity
            else Cost 0
        )
        Nothing | x <- getNodes (Graph as)]

-- Getter for NodeCost
getNode :: NodeCost -> Node
getNode (NodeCost n _ _) = n

--------
-- relaxCost
--------
-- Relax the v vertex with given (u,v) and currect cost of v given
-- Arguments: Arc, Cost of u, Cost of v
-- Return: updated (or not) cost of v
relaxCost :: Arc -> Cost -> NodeCost -> NodeCost

-- Case: to fill the non-exhaustivesness
relaxCost (Arc _ w _) Infinity vCost = vCost

-- Case: vCost is infinity
relaxCost (Arc u w _) (Cost uCost) (NodeCost v Infinity _) = NodeCost v (Cost (w + uCost)) (Just u)
-- Case: vCost is present
relaxCost (Arc u w v) (Cost uCost) (NodeCost _ (Cost vCost) parent)
    | newPath < vCost = NodeCost v (Cost newPath) (Just u)
    | otherwise = NodeCost v (Cost vCost) parent
    where newPath = uCost + w


-- Finds Costs of nodes of given Arc in the given CostMap
findCostsOfArcNodes :: Arc -> CostMap -> (NodeCost, NodeCost)
findCostsOfArcNodes (Arc u _ v) costMap =
    (
    head (filter isFirst costMap),
    head (filter isSecond costMap)
    )
    where
        isFirst  (NodeCost node _ _ ) = u == node
        isSecond (NodeCost node _ _ ) = v == node

-- Find dublicates of the given node in a CostMap
findDublicatesOfNode :: CostMap -> Node -> CostMap
findDublicatesOfNode costMap node = filter isOfGivenNode costMap
    where isOfGivenNode (NodeCost n _ _) = n == node

removeDuplicates :: CostMap -> CostMap
removeDuplicates costMap =
    map minimum groupedByNode
    where
        sortedCostMap = sortBy (comparing getNode) costMap
        groupedByNode = groupBy ((==) `on` getNode) sortedCostMap


relaxAllNodes :: CostMap -> Graph -> CostMap
relaxAllNodes nodes (Graph arcs) = 
    removeDuplicates
        (nodes 
        ++
        [
            relaxCost (Arc u w v) uCost vCostNode
            | 
            (Arc u w v) <- arcs,

            let costNodes = findCostsOfArcNodes (Arc u w v) nodes, 
            let vCostNode = snd costNodes,

            let uCostNode = fst costNodes,
            let uCost = (\(NodeCost _ cost _) -> cost) uCostNode
        ])

relaxNTimes :: CostMap -> Graph -> Int -> CostMap
relaxNTimes costMap _ 0 = costMap
relaxNTimes costMap graph n = relaxNTimes (relaxAllNodes costMap graph) graph (n-1)


bellmanFord :: Graph -> Node -> Maybe(CostMap)
bellmanFord graph node =
    Just $ relaxNTimes inits graph (n-1)

    where
        inits = initCosts graph node
        n = length $ getNodes graph


