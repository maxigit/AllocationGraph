-- | Render an allocation graph using Diagrams.
module AllocationGraph.Diagrams 
( renderAllocation
, RenderParameter(..)
) where


import BasePrelude hiding((.), (<>))
import Control.Lens

import Diagrams.Prelude as D
import Diagrams.Backend.SVG
import Data.Map (Map)
import qualified Data.Map as Map

import AllocationGraph

type Diag = Diagram B

data RenderParameter k = RenderParameter 
  { paramWidth :: Double -> Double
  , paramHeight :: Double -> Double
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
  columns = hcat (map (renderColumn param) (Map.elems groups))
  in columns


-- | Render a group of resources in a column
renderColumn :: RenderParameter k -> [Resource k ] -> Diag
renderColumn param resources = vcat $ map (renderResource param) resources


-- | Render a Resource as box with a split for each allocation
renderResource :: RenderParameter k
               -> Resource k
               -> Diag
renderResource param resource = rect w h
  where
    amount = _resAmount resource
    w = paramWidth param amount
    h = paramHeight param amount
    



