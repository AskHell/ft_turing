module BigO
  ( createLoopMap
  , reduce
  , bigO
  )
where

import           Data.Map                      as Map
import           Data.List                     as List
import           Debug.Trace

import           Polynomial                     ( Polynomial
                                                , addP
                                                , multP
                                                , degree
                                                )

import           Machine                        ( Transition(..)
                                                , State
                                                )

type Cost = Polynomial
type Relation = String
type Node = (Cost, [Relation])

type BaseMap = Map String Node
type LoopMap = Map String [[Relation]]
type KeyMap = Map [Relation] Bool

getLoopKeys
  :: String
  -> (String, Node)
  -> Map Relation Bool
  -> BaseMap
  -> [Relation]
  -> [[Relation]]
  -> [[Relation]]
getLoopKeys baseNode (nodeName, _) explored _ path acc
  | member nodeName explored && nodeName == baseNode = path : acc
getLoopKeys baseNode (nodeName, _) explored _ _ acc
  | member nodeName explored = acc
getLoopKeys baseNode (nodeName, node) explored baseMap path acc = List.foldl
  (\acc relation ->
    let nodeToExploreMaybe = Map.lookup relation baseMap
    in  case nodeToExploreMaybe of
          Nothing            -> acc
          Just nodeToExplore -> getLoopKeys baseNode
                                            (relation, nodeToExplore)
                                            updatedExplored
                                            baseMap
                                            sortedPath
                                            acc
  )
  acc
  relations
 where
  updatedExplored = Map.insert nodeName True explored
  relations       = snd node
  sortedPath      = sort updatedPath
  updatedPath     = nodeName : path

createLoopMap :: BaseMap -> LoopMap
createLoopMap baseMap = loopMap where
  (loopMap, _) = List.foldl
    (\(loopMap, loopIdsMap) (key, node) ->
      let explored = Map.empty
      in  let loopKeys = getLoopKeys key (key, node) explored baseMap [] []
          in  let filteredKeys = List.filter
                    (\loopKey -> not $ Map.member loopKey loopIdsMap)
                    loopKeys
              in  let updatedLoopsMap = Map.insert key filteredKeys loopMap
                  in  let updatedLoopIdsMap = List.foldl
                            (\acc k -> Map.insert k True acc)
                            loopIdsMap
                            filteredKeys
                      in  (updatedLoopsMap, updatedLoopIdsMap)
    )
    (loopMap, loopIdsMap)
    nodeList   where
    acc        = (loopMap, loopIdsMap)
    loopMap    = Map.empty
    loopIdsMap = Map.empty
    nodeList   = Map.toList baseMap


addRelation :: BaseMap -> Polynomial -> Relation -> Polynomial
addRelation baseMap acc relation = addP acc cost where
  cost = case Map.lookup relation baseMap of
    Nothing        -> []
    Just (cost, _) -> cost

reduceLoop :: BaseMap -> [Relation] -> Polynomial
reduceLoop baseMap relations = multP loopCost loopCost
  where loopCost = List.foldl (addRelation baseMap) [] relations

multLoops :: BaseMap -> Polynomial -> [[Relation]] -> Polynomial
multLoops baseMap final loops = multP final loopCosts
 where
  loopCosts = List.foldl
    (\acc loop -> let loopCost = reduceLoop baseMap loop in multP acc loopCost)
    []
    loops

reduce :: BaseMap -> LoopMap -> Polynomial
reduce baseMap loopMap = List.foldl (multLoops baseMap) [] loopsList
  where loopsList = List.map snd (Map.toList loopMap)

updateBaseMap :: BaseMap -> (State, [Transition]) -> BaseMap
updateBaseMap baseMap (state, transitions) = Map.insert state
                                                        (cost, relations)
                                                        baseMap where
  cost      = if length relations < length to_states then [0, 1] else [1]
  relations = List.filter (/= state) to_states
  to_states = List.map to_state transitions

createBaseMap :: [(State, [Transition])] -> BaseMap
createBaseMap = List.foldl updateBaseMap Map.empty

toString :: Polynomial -> String
toString poly = case degree poly of
  0      -> "O(1)"
  1      -> "O(n)"
  degree -> "O(n^" ++ show degree ++ ")"

bigO :: Map State [Transition] -> String
bigO transitionsMap = toString reduced' where
  reduced' = Debug.Trace.trace ("reduced: " ++ show reduced) reduced
  reduced  = reduce baseMap' loopMap'
  loopMap' = Debug.Trace.trace ("loopMap: " ++ show loopMap) loopMap
  baseMap' = Debug.Trace.trace ("baseMap: " ++ show baseMap) baseMap
  loopMap  = createLoopMap baseMap
  baseMap  = createBaseMap $ Map.toList transitionsMap