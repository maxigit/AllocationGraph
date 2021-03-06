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
data Resource k e = Resource 
  { _resName :: !String
  , _resKey :: !k
  , _resType :: !ResourceType
  , _resAmount :: !Double
  , _resExtra :: !e
  } deriving (Show, Read)

makeLenses ''Resource

isSource, isTarget :: Resource k e -> Bool
isSource = _isSource . _resType
isTarget = _isTarget . _resType

instance Eq k => Eq (Resource k e) where
  (==) a b =  _resKey a == _resKey b
instance Ord k => Ord (Resource k e) where
  compare a b = compare (_resKey a) (_resKey b)

-- | Allocation. k is the type of resource
-- can be either ResourceKey or Resource 
data Allocation k = Allocation 
  { _allocAmount :: !Double
  , _allocSource :: !k
  , _allocTarget :: !k
  } deriving (Show, Read, Eq)

instance Ord k => Ord (Allocation k) where
  compare = (<>) <$> comparing _allocSource  
                 <*> comparing _allocTarget

makeLenses ''Allocation

-- | Denormalized graph.
-- Should be build once but not updated.
data Graph k e = Graph 
  { _graphAllocations :: [Allocation (Resource k e)]
  , _graphResources :: [Resource k e] -- to keep initial order
  , _graphResourceMap :: Map (Resource k e) [Allocation (Resource k e)]
  } deriving (Show, Read, Eq)

makeLenses ''Graph

_graphSources, _graphTargets :: Graph k e -> [Resource k e]
_graphSources graph = filter isSource (_graphResources graph)
_graphTargets graph = filter isTarget (_graphResources graph)

-- | Resources will be 
data Group = Group
