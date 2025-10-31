module BellmanFord (CostMap, bellmanFord, relaxAllNodes, removeDuplicates, initCosts) where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Control.DeepSeq (NFData (rnf))

import Graph (Graph, Node, Arc(..), Graph(..), getNodes);



--------------------------
--- Data and instances ---
--------------------------

-- Stores data for the cost to reach a nodes from the starting node
data Cost = Cost Int | Infinity deriving (Show, Eq, Ord)
instance NFData Cost where
  rnf (Cost x) = rnf x
  rnf Infinity = ()


-- Stores data of a node, it's cost and the parent node (if exists)
data NodeCost = NodeCost Node Cost (Maybe Node) deriving(Show, Eq)
instance NFData NodeCost where
  rnf (NodeCost node cost maybeNode) = rnf node `seq` rnf cost `seq` rnf maybeNode
-- Interface: may be sorted by the cost
instance Ord NodeCost where
  (NodeCost _ cost1 _) <= (NodeCost _ cost2 _) = cost1 <= cost2


-- Represents the cost-nodes of the graph.
-- May also represent sub-cost-nodes of a graph
type CostMap = [NodeCost]



-----------------
--- Functions ---
-----------------

-- Gets node component from the NodeCost
getNode :: NodeCost -> Node
getNode (NodeCost n _ _) = n

-- Initializes costs for the node
-- Takes a graph, a starting node
-- Returns a costmap with all costs except the first's being Infinity,
-- the first's cost being 0, all parents being Nothing.
initCosts :: Graph -> Node -> CostMap
initCosts (Graph as) n =
    [NodeCost x (
        if x /= n
            then Infinity
            else Cost 0
        )
        Nothing | x <- getNodes (Graph as)]

-- Relax the v vertex with given (u,v) and currect cost of v given
-- Arguments: Arc, Cost of u, Cost of v
-- Return: updated (or not) cost of v
relaxCost :: Arc -> Cost -> NodeCost -> NodeCost

-- Case: to fill the non-exhaustivesness
relaxCost (Arc {}) Infinity vCost = vCost

-- Case: vCost is infinity
relaxCost (Arc u w _) (Cost uCost) (NodeCost v Infinity _) = NodeCost v (Cost (w + uCost)) (Just u)
-- Case: vCost is present
relaxCost (Arc u w v) (Cost uCost) (NodeCost _ (Cost vCost) parent)
    | newPath < vCost = NodeCost v (Cost newPath) (Just u)
    | otherwise = NodeCost v (Cost vCost) parent
    where newPath = uCost + w


-- Finds Costs of nodes of given Arc in the given costmap
-- Takes Arc (from u to v), the costmap
-- Returns (NodeCost of u, NodeCost of v)
findCostsOfArcNodes :: Arc -> CostMap -> (NodeCost, NodeCost)
findCostsOfArcNodes (Arc u _ v) costMap =
    (
    head (filter isFirst costMap),
    head (filter isSecond costMap)
    )
    where
        isFirst  (NodeCost node _ _ ) = u == node
        isSecond (NodeCost node _ _ ) = v == node


-- Removes NodeCosts with the same Node component, leaving the one's with smaller cost
removeDuplicates :: CostMap -> CostMap
removeDuplicates costMap =
    map minimum groupedByNode
    where
        sortedCostMap = sortBy (comparing getNode) costMap
        groupedByNode = groupBy ((==) `on` getNode) sortedCostMap


-- Relaxes all nodes of the graph, using it's arcs weights and the current costmap
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

-- Applies relaxation to the given costmap and graph n times
-- Takes a current costmap, a graph, number of relaxations needed
-- Returnes the relaxed n times costmap
relaxNTimes :: CostMap -> Graph -> Int -> CostMap
relaxNTimes costMap _ 0 = costMap
relaxNTimes costMap graph n = relaxNTimes (relaxAllNodes costMap graph) graph (n-1)

-- Bellmand-Ford shortest-path-finidng algorythm.
-- Takes a Graph and the starting Node
-- Returns a CostMap, consisting of NodeCosts (node, cost, parent) where
--     - node is the current node (id or number)
--     - cost is the weight of the path from the starting node to current
--     - parent is the previous node on the path from the statring node to the current
-- The algorythm does not account negative cycles.
bellmanFord :: Graph -> Node -> CostMap
bellmanFord graph node =
    relaxNTimes inits graph (n-1)

    where
        inits = initCosts graph node
        n = length $ getNodes graph


