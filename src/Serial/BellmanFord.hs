module Serial.BellmanFord (Cost(..), CostMap, NodeCost(..), initCosts, findCostsOfArcNodes, relaxCost, relaxAllNodes) where

import Serial.Graph (Graph, Node, Arc(..), Graph(..), getNodes);

data Cost = Cost Int | Infinity deriving(Show)

-- NodeCost, fields for Current node, cost to reach from s, parent node
data NodeCost = NodeCost Node Cost (Maybe Node) deriving(Show)

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

-- removeDublicats :: CostMap -> CostMap
-- removeDublicats costMap = 



-- relaxAllNodes :: [NodeCost] -> [Arc] -> [NodeCost]
-- relaxAllNodes costs arcs = 



-- bellmanFord :: Graph -> Node -> Maybe(CostMap)
-- bellmanFord (Graph as) n =
--      let ics = initCosts (Graph as) n
--      in 
