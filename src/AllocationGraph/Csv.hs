module AllocationGraph.Csv (
  renderCsv
) where

import AllocationGraph.Type

import BasePrelude
       
import AllocationGraph

renderCsv :: Ord k => Graph k e -> [String]
renderCsv graph = let
  in "source,amount,target":map renderAllocation (_graphAllocations graph)
     ++ mapMaybe (renderUnallocatedBox graph) (_graphResources graph)


format :: String -> Double -> String -> String
format source amount target  = printf "\"%s\",%0.2f,\"%s\"" source amount target
renderUnallocatedBox :: Ord k => Graph k e -> Resource k e -> Maybe String
renderUnallocatedBox graph resource =  do
  let ua = abs (unallocated graph resource)
      name = _resName resource
      uname = printf "<%s>" name ::String
  guard (ua > 0.01)
  return $ if isSource resource
              then format name ua uname 
              else format uname ua name
  



renderAllocation :: Allocation (Resource k e) -> String
renderAllocation alloc = format
  (_resName ( _allocSource alloc))
  (abs (_allocAmount alloc))
  (_resName ( _allocTarget alloc))




