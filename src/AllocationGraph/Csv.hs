module AllocationGraph.Csv (
  renderCsv
) where

import AllocationGraph.Type

import BasePrelude
       
import AllocationGraph

renderCsv :: Ord k => Graph k e -> [String]
renderCsv graph = let
  in map renderAllocation (_graphAllocations graph)
     ++ mapMaybe (renderUnallocatedBox graph) (_graphResources graph)


renderUnallocatedBox :: Ord k => Graph k e -> Resource k e -> Maybe String
renderUnallocatedBox graph resource =  do
  let ua = abs (unallocated graph resource)
  guard (ua > 0.01)
  return $ if isSource resource
              then (printf "%s,%0.2f,") (_resName resource) ua
              else (printf ",%0.2f,%s") ua (_resName resource)
  



renderAllocation alloc = printf "%s,%0.2f,%s"
  (_resName ( _allocSource alloc))
  (abs (_allocAmount alloc))
  (_resName ( _allocTarget alloc))




