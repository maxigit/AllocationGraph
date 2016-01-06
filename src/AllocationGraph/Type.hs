{-# LANGUAGE TemplateHaskell #-}
module AllocationGraph.Type 
{-( Resource(..)
, Allocation(..)
, Graph
, ResourceKey
)-} where


import BasePrelude hiding((.))
import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map


data ResourceType = Source | Target {- | SourceAndTarget -} deriving (Show, Read, Eq, Ord)

_isSource :: ResourceType -> Bool
_isSource Target = False
_isSource _ = True

_isTarget :: ResourceType -> Bool
_isTarget Source = False
_isTarget _ = True

type ResourceKey = Int
data Resource k = Resource 
  { _resName :: !String
  , _resKey :: k
  , _resType :: ResourceType
  , _resAmount :: !Double
  } deriving (Show, Read, Eq)

makeLenses ''Resource

isSource, isTarget :: Resource k -> Bool
isSource = _isSource . _resType
isTarget = _isTarget . _resType

instance Ord k => Ord (Resource k) where
  compare a b = compare (_resKey a) (_resKey b)

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
  , _graphResources :: [Resource k] -- to keep initial order
  , _graphResourceMap :: Map (Resource k) [Allocation (Resource k)]
  } deriving (Show, Read, Eq)

makeLenses ''Graph

_graphSources, _graphTargets :: Graph k -> [Resource k]
_graphSources graph = filter isSource (_graphResources graph)
_graphTargets graph = filter isTarget (_graphResources graph)

-- | Resources will be 
data Group = Group
