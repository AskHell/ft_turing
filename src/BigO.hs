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
                                                , maxP
                                                )

import           Machine                        ( Transition(..)
                                                , State
                                                )

type Log = Int
type Cost = (Polynomial, Int)
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
      in  let loopIdsList = List.map fst $ Map.toList loopIdsMap
          in  let loopKeys = getLoopKeys key (key, node) explored baseMap [] []
              in  let
                    filteredKeys = List.filter
                      (\loopKey ->
                        not $ List.any (List.isInfixOf loopKey) loopIdsList
                      )
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

addRelation :: BaseMap -> Cost -> Relation -> Cost
addRelation baseMap (accPoly, accLog) relation = (newPoly, newLog) where
  newLog              = costLog + accLog
  newPoly             = addP accPoly costPoly
  (costPoly, costLog) = case Map.lookup relation baseMap of
    Nothing        -> ([], 0)
    Just (cost, _) -> cost

reduceLoop :: BaseMap -> [Relation] -> Cost
reduceLoop baseMap relations = (newPoly, log)
 where
  newPoly     = if log == 0 then multP poly poly else poly
  (poly, log) = List.foldl (addRelation baseMap) ([], 0) relations

multCost :: Cost -> Cost -> Cost
multCost (polyA, logA) (polyB, logB) = (multP polyA polyB, logA + logB)

maxCost :: [Cost] -> Cost
maxCost costs = (maxPoly, maxLog) where
  maxPoly = maxP $ List.map fst costs'
  costs'  = trace ("costs: " ++ show costs) costs
  maxLog  = maximum $ List.map snd costs

multLoops :: BaseMap -> Cost -> [[Relation]] -> Cost
multLoops baseMap final loops = multCost final loopCosts
 where
  loopCosts = List.foldl
    (\acc loop ->
      let acc' = trace ("acc: " ++ show acc) acc
      in  let loopCost = reduceLoop baseMap loop in maxCost [loopCost, acc']
    )
    ([], 0)
    loops

reduce :: BaseMap -> LoopMap -> Cost
reduce baseMap loopMap = List.foldl (multLoops baseMap) ([], 0) loopsList
  where loopsList = List.map snd (Map.toList loopMap)

-- TODO: find and update log
updateBaseMap :: BaseMap -> (State, [Transition]) -> BaseMap
updateBaseMap baseMap (state, transitions) = Map.insert state
                                                        (cost, relations)
                                                        baseMap where
  cost      = (stepCost, if hasLog then 1 else 0)
  hasLog    = List.all (\rel -> Machine.read rel /= Machine.write rel) to_states
  stepCost  = if not $ List.null to_states then [0, 1] else [1]
  relations = List.filter (/= state) $ List.map Machine.to_state transitions
  to_states = List.filter ((== state) . to_state) transitions

createBaseMap :: [(State, [Transition])] -> BaseMap
createBaseMap = List.foldl updateBaseMap Map.empty

toString :: Cost -> String
toString (poly, log) = case degree poly of
  0 -> "O(1)"
  1 -> case log of
    0 -> "O(n)"
    _ -> "O(nlogn)"
  degree -> "O(n^" ++ show degree ++ ")"

bigO :: Map State [Transition] -> String
bigO transitionsMap = toString reduced' where
  reduced' = trace ("reduced: " ++ show reduced) reduced
  reduced  = reduce baseMap loopMap
  loopMap  = createLoopMap baseMap
  baseMap  = createBaseMap $ Map.toList transitionsMap
