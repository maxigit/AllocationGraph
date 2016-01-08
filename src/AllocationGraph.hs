-- | Main module
module AllocationGraph
( module AllocationGraph
, module AllocationGraph.Type
)
where

import BasePrelude
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

      groupAllocByResource resource als = Map.fromListWith (<>) (concatMap link als)
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
  targets'  = concatMap (\s -> map _allocTarget (allocsFor graph s)) sources ++ targets
  -- we need then to remove duplicates but still keeping the initial order
  -- we add targets again to put add the end all the target which don't have allocations
  go [] _ = []
  go (t:ts) done = case Map.lookup key done of
                      Nothing -> t : go ts (Map.insert key 1 done)
                      Just _ -> go ts done
                    where key = _resKey t
  in graph { _graphResources = sources ++ go targets' mempty }
