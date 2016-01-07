-- | Render an allocation graph using Diagrams.
module AllocationGraph.Diagrams 
( renderAllocation
, RenderParameter(..)
) where


import BasePrelude hiding((.), (<>))
-- import Control.Lens

import Diagrams.Prelude as D
import Diagrams.Backend.SVG
import Data.Map (Map)
import qualified Data.Map as Map

import AllocationGraph

type Diag = Diagram B

data RenderParameter k = RenderParameter 
  { paramWidth :: Double -> Double
  , paramHeight :: Double -> Double
  , paramSep :: Double
  }
-- | Generate a allocation graph using diagrams.
-- Resources are vertical displayed in columns
renderAllocation :: (Ord k, Show k, Ord g)
                        => RenderParameter k
                        -> (Resource k -> g)      -- ^ function to group resource
                        -> Graph k      -- ^ the graph to render
                        -> Diag


renderAllocation param group graph = let
  resources = _graphResources graph
  groups = Map.fromListWith (flip (<>)) $ zip (map group resources) (map (:[]) resources)
  columns = hsep (500 {-paramSep param-}) (map (renderColumn param graph) (Map.elems groups))
  arrows = map (joinAllocBox param columns) (_graphAllocations graph)
  in (mconcat (arrows)) -- `atop` columns


-- | Render a group of resources in a column
renderColumn :: (Ord k, Show k) => RenderParameter k -> Graph k -> [Resource k ] -> Diag
renderColumn param graph resources = vsep (paramSep param) $ map (renderResource param graph) resources


-- | Render a Resource as box with a split for each allocation
renderResource :: (Ord k, Show k)
               => RenderParameter k
               -> Graph k
               -> Resource k
               -> Diag
renderResource param graph resource = hcat (revIf rType (map alignT [tag , allocs]))
  where
    rType = _resType resource
    tag = label <> rect w h 
    label = renderLabel param (printf "%s: %.2f" (_resName resource) (_resAmount resource) )
    amount = _resAmount resource
    w = paramWidth param amount
    h = paramHeight param amount

    allocs = mconcat (map alignT  [allocBoxes, rect w h # bg red])
    revIf Source l = l
    revIf Target l = reverse l
    allocBoxes = vcat (map (allocBox param rType) (allocsFor graph resource))

renderLabel :: RenderParameter k -> String -> Diag
renderLabel param s = text s # rotateBy (1/4) # fontSize (local ((paramWidth param (error "please change this function to not take any parameters"))/4))

allocBox :: (Ord k, Show k)
         => RenderParameter k
         -> ResourceType
         -> Allocation (Resource k)
         -> Diag
allocBox param rType alloc = label  <> rect w h # bg green # named (nameAllocBox rType alloc)
  where w = paramWidth param  undefined
        h = paramHeight param (_allocAmount alloc)
        label = renderLabel param (printf "%.2f" (_allocAmount alloc))


-- | Give a unique name to the edge of an allocation.
-- This name will be used to draw arrows between boxes.
nameAllocBox :: Show k => ResourceType -> Allocation (Resource k) -> String
nameAllocBox t al = show t ++ key (_allocSource al) ++ "-" ++ key (_allocTarget al)
  where
    key resource = show ( _resKey  resource)


joinAllocBox :: Show k => RenderParameter k -> Diag -> Allocation (Resource k) -> Diag
joinAllocBox param diag alloc = diag # connect (nameAllocBox Source alloc)
                                               (nameAllocBox Target alloc)
