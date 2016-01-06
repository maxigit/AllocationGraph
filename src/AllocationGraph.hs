-- | Main module
module AllocationGraph
( module AllocationGraph
, module AllocationGraph.Type
)
where

import BasePrelude
import AllocationGraph.Type

-- | Build a graph . Return an error
-- if referece are not found or allocation are not the wrong way
-- (allocating to a source or from a target)
buildGraph :: [Resource k]  -- ^ all the resources
           -> [Allocation k] -- ^ allocations referencing the resources
           -> Either String (Graph k)

buildGraph = error "buildGraph not implemented"

allocated:: Graph k -> Resource k -> Double
allocated graph res = let allocs = allocsForSource graph res
  in sum (map _allocAmount allocs)

unallocated :: Graph k -> Resource k -> Double
unallocated graph res = _resAmount res - allocated graph res

allocsForSource :: Graph k -> Resource k -> [Allocation (Resource k)]
allocsForSource = error "allocsForSource not implemented"
