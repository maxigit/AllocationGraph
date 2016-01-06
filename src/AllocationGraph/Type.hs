{-# LANGUAGE TemplateHaskell #-}
module AllocationGraph.Type 
(
) where

import BasePrelude
import Control.Lens
data Resource = Resource 
  { _resName :: String
  , _resAmount :: Double
  } deriving (Show, Read, Eq)

makeLenses ''Resource

data Allocation = Alloction 
  { _allocAmount:: Double
  , _allocSource :: Resource
  , _allocTarget :: Resource
  } deriving (Show, Read, Eq)

makeLenses ''Allocation

data Graph = Graph 
  { allocations :: [Allocation]
  } deriving (Show, Read, Eq)

makeLenses ''Graph
