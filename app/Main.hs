module Main where

import AllocationGraph
import AllocationGraph.Diagrams

import Diagrams.Backend.SVG.CmdLine (mainWith)


-- * 
testGraph = let
  a,b :: Resource Int
  a = Resource "A" 1 Source 50
  b = Resource "B" 2 Source 50
  c = Resource "C" 3 Target 70
  d = Resource "D" 4 Target 30
  resources = [a,b,c,d]
  allocations = [ Allocation 45 1 3
                , Allocation 20 2 3
                , Allocation 30 2 4
                ]
  Right graph = buildGraph resources allocations
  in graph

main :: IO ()
main = mainWith diag 
  where
    diag = renderAllocation param _resType testGraph
    param = RenderParameter (const 50) (*5) 10
