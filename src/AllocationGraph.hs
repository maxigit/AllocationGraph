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
unallocated graph res = _resAmount res - allocated graph res


allocsFor :: Ord k => Graph k -> Resource k -> [Allocation (Resource k)]
allocsFor graph resource = fromMaybe [] $ Map.lookup resource (_graphResourceMap graph)

