module Serial.BellmanFord (Cost(..), initCosts, relaxCost) where

import Serial.Graph (Graph, Node, Arc(..), Graph(..), getNodes);

data Cost = Cost Int | Infinity deriving(Show)

-- NodeCost, fields for Current node, cost to reach from s, parent node
data NodeCost = NodeCost Node Cost (Maybe Node) deriving(Show)

data CostMap = CostMap [NodeCost] deriving(Show)


-- Initialize costs for Node
initCosts :: Graph -> Node -> CostMap
initCosts (Graph as) n = CostMap (
    [NodeCost x (
        if x /= n 
            then Infinity 
            else Cost 0)
        (Just 0) | x <- getNodes (Graph as)]
    )

--------
-- relaxCost
--------
-- Relax the v vertex with given (u,v) and currect cost of v given
-- Arguments: Arc, Cost of u, Cost of v
-- Return: updated (or not) cost of v
relaxCost :: Arc -> Cost -> Cost -> Cost

-- Case: to fill the non-exhaustivesness
relaxCost _ Infinity Infinity = Infinity 
relaxCost (Arc _ w _) Infinity vCost = vCost

-- Case: vCost is infinity
relaxCost (Arc _ w _) (Cost uCost) Infinity = Cost (w + uCost)
-- Case: vCost is present
relaxCost (Arc v w u) (Cost uCost) (Cost vCost)
    | newPath < vCost = Cost newPath
    | otherwise = Cost vCost
    where newPath = uCost + w

-- relaxAllNodes :: [NodeCost] -> [Arc] -> [NodeCost]
-- relaxAllNodes costs arcs = 



-- bellmanFord :: Graph -> Node -> Maybe(CostMap)
-- bellmanFord (Graph as) n =
--      let ics = initCosts (Graph as) n
--      in 
