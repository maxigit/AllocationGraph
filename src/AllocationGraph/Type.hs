{-# LANGUAGE TemplateHaskell #-}
module AllocationGraph.Type 
{-( Resource(..)
, Allocation(..)
, Graph
, ResourceKey
)-} where

import BasePrelude
import Control.Lens

type ResourceKey = Int
data Resource k = Resource 
  { _resName :: !String
  , _resKey :: k
  , _resAmount :: !Double
  } deriving (Show, Read, Eq)

makeLenses ''Resource

-- | Allocation. k is the type of resource
-- can be either ResourceKey or Resource 
data Allocation k = Allocation 
  { _allocAmount :: !Double
  , _allocSource :: !k
  , _allocTarget :: !k
  } deriving (Show, Read, Eq)

makeLenses ''Allocation

-- | Denormalized graph.
-- Should be build once but not updated.
data Graph k = Graph 
  { _graphAllocations :: [Allocation (Resource k)]
  } deriving (Show, Read, Eq)

makeLenses ''Graph

-- | Resources will be 
data Group = Group
