-- | Main module
module AllocationGraph
( module AllocationGraph
, module AllocationGraph.Type
)
where

import BasePrelude hiding((.), id)
import Control.Lens hiding((&))

import Data.Map (Map)
import qualified Data.Map as Map

import AllocationGraph.Type

-- | Build a graph . Return an error
-- if referece are not found or allocation are not the wrong way
-- (allocating to a source or from a target)
buildGraph :: Ord k =>  [Resource k]  -- ^ all the resources
                    -> [Allocation k] -- ^ allocations referencing the resources
                    -> Either String (Graph k)

buildGraph resources allocs = 
  let -- resourceMap :: Map k (Resource k)
      resourceMap = Map.fromList $ zip (map _resKey resources) resources

      allocations = forM allocs $ \al -> do
        source <- Map.lookup (_allocSource al) resourceMap 
        target <- Map.lookup (_allocTarget al) resourceMap
        if isSource source && isTarget target
           then Just $ Allocation (al ^. allocAmount) source target
           else Nothing

      groupAllocByResource resource als = Map.fromListWith (flip (<>)) (concatMap link als)
      link alloc = [ (_allocSource alloc, [alloc])
                   , (_allocTarget alloc, [alloc])
                   ]
                                       

  in if length resources /= Map.size resourceMap
        then Left "Some resources have the same key"
        else case allocations of
              Nothing -> Left "Allocations couldn't find references"
              Just als -> Right $ Graph als
                                        resources
                                        (groupAllocByResource _allocSource als)

allocated:: Ord k => Graph k -> Resource k -> Double
allocated graph res = let allocs = allocsFor graph res
  in sum (map _allocAmount allocs)

unallocated :: Ord k => Graph k -> Resource k -> Double
unallocated graph res = abs (_resAmount res) - abs ( allocated graph res)


allocsFor :: Ord k => Graph k -> Resource k -> [Allocation (Resource k)]
allocsFor graph resource = fromMaybe [] $ Map.lookup resource (_graphResourceMap graph)


-- | Keep allocations only related to the given resources 
-- both side of the allocation needs to be present so we can draw it properly
filterAllocations :: Ord k =>  [Resource k] -> [Allocation k] -> [Allocation k]
filterAllocations resources allocs = let
  resourceMap = Map.fromList $ zip (map _resKey resources) [1..] -- we shoudl use a set
  findAs alloc field = isJust $ Map.lookup (field alloc) resourceMap
  ok alloc = and $ map (findAs alloc) [_allocSource, _allocTarget]
  in filter ok allocs

-- | Reorder target so that they are in the same order
-- as the source. This should minimize arrows crossing.
orderTargets graph = let
  (sources, targets) = partition isSource (_graphResources graph)
  targets'  = (concatMap (\s -> map _allocTarget (allocsFor graph s)) sources) ++ targets
  -- we need then to remove duplicates but still keeping the initial order
  -- we add targets again to put add the end all the target which don't have allocations
  in graph { _graphResources = sources ++ removeDuplicatesWith _resKey targets' }


-- | Remove duplicates even if they are not grouped. As opposed to nub
removeDuplicatesWith :: (Ord k, Eq k) => (a -> k) -> [a] -> [a]
removeDuplicatesWith fkey as = go as mempty
  where
    go [] _ = []
    go (t:ts) done = case Map.lookup key done of
                        Nothing -> t : go ts (Map.insert key 1 done)
                        Just _ -> go ts done
                   where key = fkey t



-- | Compact a graph by grouping resources belonging to the same group as one
-- All linked allocation are also grouped together.
-- The group function should return a resource with a key. This key
-- will be used to group things together. It will then be replaced
-- by the key of the first resource from the group.
groupResources :: (Ord k, Eq k, Ord g) => Graph k -> (Resource k -> Either (Resource k) (Resource g)) -> Graph k
groupResources graph fgroup = let
  resources = _graphResources graph
  -- we need a map resource -> group
  -- Sources and Targets must be in in different groups which is why
  -- we have to add the resource type in the map key
  key r = (_resType r, fgroup r)
  groups = Map.fromListWith (<>) $ [(key r, [r]) | r <- resources ]
  resources' = removeDuplicatesWith key resources 
  -- now we need to replace each resource belonging to a group by the group
  grouped = Map.map (mkGroup. head) groups
  newResources = map findNewResource resources'
  findNewResource res = Map.findWithDefault (error "Ouch!") (key res) grouped
  mkGroup resource = case key resource  of
                             (_, Left rk) -> rk
                             k@(_, Right rg) ->  rg & resAmount .~ (sum $ map _resAmount (fromJust $ Map.lookup k groups))
                                                    & resKey .~ resource ^. resKey


  -- group allocation 
  transformAlloc alloc = alloc & allocSource %~ findNewResource
                               & allocTarget %~ findNewResource
  

  in graph & graphResources .~ (map mkGroup resources')
           & graphAllocations %~ groupAllocations . map transformAlloc

groupAllocations :: Ord k =>  [Allocation k] -> [Allocation k]
groupAllocations allocs =  let
  groups = Map.fromListWith  aggregate (zip allocs allocs)
  aggregate a a' = a & allocAmount +~ (a' ^. allocAmount)
  replace a = Map.findWithDefault a a groups
  in removeDuplicatesWith id (map replace allocs)
                                                    
                        

-- todo
--
-- remove all *connected graphs*
-- find allocation with not fully allocated resources
-- | Keep resources related to all the allocations
-- filterResources :: Ord 
