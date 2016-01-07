-- | Render an allocation graph using Diagrams.
module AllocationGraph.Diagrams 
( renderAllocation
, RenderParameter(..)
) where


import BasePrelude hiding((.), (<>))
-- import Control.Lens

import Diagrams.Prelude as D hiding((&))
import Diagrams.Backend.SVG
import Data.Map (Map)
import qualified Data.Map as Map

import AllocationGraph

type Diag = Diagram B

data RenderParameter k = RenderParameter 
  { paramWidth :: Double 
  , paramBoxHeight :: Double -> Double -> Double
                   -- ^                            resource amount
                   --           ^                  allocation amount
                   --                     ^        display size
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
  in case arrows of -- `atop` columns
      [] -> columns -- 
      as -> mconcat as


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
    tag = label <> rect (w*20) h  # lwL 1
    labelString = printf "%s -- %.2f" (_resName resource) amount
    label = renderTag param labelString
    amount = abs (_resAmount resource)
    w = paramWidth param 
    -- the height is the sum of the heigh of the boxes
    -- we can't just use the total amount because paramBoxHeight might not be linear
    -- (ex log)
    amounts = ua : map (abs . _allocAmount) (allocsFor graph resource)
    h = sum (map (paramBoxHeight param amount) amounts)

    allocs = vcat ([allocBoxes, unallocatedBox ])
    revIf Source l = l
    revIf Target l = reverse l
    allocBoxes = vcat (map (allocBox param resource) (allocsFor graph resource))  
    ua = abs (unallocated graph resource)
    unallocatedBox = if  ua == 0 then mempty
                                 else renderLabel param (printf "%.2f" ua)
                                      <> rect w (paramBoxHeight param amount ua) # bg red #lwL 1

renderLabel, renderTag :: RenderParameter k -> String -> Diag
renderLabel param s = text s # rotateBy (1/4) # fontSize (local ((paramWidth param)/4))
renderTag param s = text s # fontSize (local ((paramWidth param)/1))

allocBox :: (Ord k, Show k)
         => RenderParameter k
         -> Resource k
         -> Allocation (Resource k)
         -> Diag
allocBox param resource alloc = label  <> rect w h # bg green # named (nameAllocBox rType alloc) # lwL 1
  where w = paramWidth param
        h = abs $ paramBoxHeight param amount (_allocAmount alloc)
        label = renderLabel param (printf "%.2f" (abs (_allocAmount alloc)))
        amount = abs $ _resAmount resource
        rType = _resType resource


-- | Give a unique name to the edge of an allocation.
-- This name will be used to draw arrows between boxes.
nameAllocBox :: Show k => ResourceType -> Allocation (Resource k) -> String
nameAllocBox t al = show t ++ key (_allocSource al) ++ "-" ++ key (_allocTarget al)
  where
    key resource = show ( _resKey  resource)


joinAllocBox :: Show k => RenderParameter k -> Diag -> Allocation (Resource k) -> Diag
joinAllocBox param diag alloc = diag # connect' (with & shaftStyle  %~ lwL 1
                                                      & lengths .~ 1
                                                )
                                                (nameAllocBox Source alloc)
                                               (nameAllocBox Target alloc)
