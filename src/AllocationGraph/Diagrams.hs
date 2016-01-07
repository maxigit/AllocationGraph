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
renderAllocation :: (Ord k, Ord g)
                        => RenderParameter k
                        -> (Resource k -> g)      -- ^ function to group resource
                        -> Graph k      -- ^ the graph to render
                        -> Diag


renderAllocation param group graph = let
  resources = _graphResources graph
  groups = Map.fromListWith (flip (<>)) $ zip (map group resources) (map (:[]) resources)
  columns = hsep (paramSep param) (map (renderColumn param graph) (Map.elems groups))
  in columns


-- | Render a group of resources in a column
renderColumn :: Ord k => RenderParameter k -> Graph k -> [Resource k ] -> Diag
renderColumn param graph resources = vsep (paramSep param) $ map (renderResource param graph) resources


-- | Render a Resource as box with a split for each allocation
renderResource :: Ord k
               => RenderParameter k
               -> Graph k
               -> Resource k
               -> Diag
renderResource param graph resource = hcat (revIf (_resType resource) (map alignT [tag , allocs]))
  where
    tag = rect w h
    amount = _resAmount resource
    w = paramWidth param amount
    h = paramHeight param amount

    allocs = mconcat (map alignT  [allocBoxes, rect w h # bg red])
    revIf Source l = l
    revIf Target l = reverse l
    allocBoxes = vcat (map (allocBox param) (allocsFor graph resource))


allocBox :: Ord k
         => RenderParameter k
         -> Allocation (Resource k)
         -> Diag
allocBox param alloc = rect w h # bg green
  where w = paramWidth param  undefined
        h = paramHeight param (_allocAmount alloc)

    



