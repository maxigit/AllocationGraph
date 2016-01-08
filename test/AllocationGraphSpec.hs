module AllocationGraphSpec (main, spec) where

import AllocationGraph

import BasePrelude
import Control.Lens hiding ((&))

import Test.Hspec
import Test.QuickCheck


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

a,b :: Resource Int
a = Resource "A" 1 Source 100
b = Resource "B" 2 Target 20
c = Resource "C" 3 Source  50
d = Resource "D" 4 Target 70
resources = [a,b]
allocations = [Allocation  15 1 2]
Right graph = buildGraph resources allocations


spec :: Spec
spec = do
  describe "Building graph" $ do
    it "can't allocate in the wrong direction" $ do
        isLeft (buildGraph resources [Allocation 15 2 1]) `shouldBe` True
    it "can't allocate more that allowed" $ do
        pendingWith "not implemented"
        isLeft (buildGraph resources [Allocation 25 1 2]) `shouldBe` True
    it "detect resources with same keys" $ do
        isLeft (buildGraph ((a&resName .~ "a'") :resources) allocations) `shouldBe` True
    context "keeps allocations in the original order" $ do
      let allocs = [Allocation 15 1 2, Allocation 10 1 4]
          allocs' = [Allocation 15 a b, Allocation 10 a d]
          allocsForFirst g = allocsFor g ( head  $ _graphResources g)
      it "(straight)" $ do
        let Right graph = buildGraph [a,b,c,d] allocs
        allocsForFirst graph `shouldBe` allocs'
      it "(reverse)" $ do
        let Right graph = buildGraph [a,b,c,d] (reverse allocs)
        allocsForFirst graph `shouldBe` (reverse allocs')

  describe "A graph" $ do
    context "should have the right allocations" $ do
      it "for a source" $ do
        allocated graph a `shouldBe` 15
      it "for a target" $ do
        allocated graph b `shouldBe` 15
    context "should have the right left allocations" $ do
      it "for a source" $ do
        unallocated graph a `shouldBe` 85
      it "for a target" $ do
        unallocated graph b `shouldBe` 5
  describe "Modifying graph" $ do
    context "sorting target by source" $ do
      it "rearranges target in source order (reverse)" $ do
         -- A -> D
         -- C -> B
        let allocs = [Allocation 15 1 4] -- [Allocation 70 1 4, Allocation 20 3 2]
            Right graph = buildGraph [a,b,c,d] allocs
        _graphResources (orderTargets graph) `shouldBe` [a,c,d,b]
      it "rearranges target in source order (straight)" $ do
         -- A -> B
         -- C -> D
        let allocs = [Allocation 50 3 4, Allocation 20 1 2]
            Right graph = buildGraph [a,b,c,d] allocs
        _graphResources (orderTargets graph) `shouldBe` [a,c,b,d]
      it "rearranges target in allocation order" $ do
         -- A -> D
         -- + -> B
        let a = Resource "a" 1 Source 100
            b = Resource "b" 2 Target 20
            c = Resource "c" 3 Source  50
            d = Resource "d" 4 Target 70
        let allocs = [Allocation 70 1 4, Allocation 20 1 2]
            Right graph = buildGraph [a,b,c,d] allocs
        _graphResources (orderTargets graph) `shouldBe` [a,c,d,b]
      it "puts unusued target at the end" $ do
        let allocs = [Allocation 15 1 4]
            Right graph = buildGraph [a,b,c,d] allocs
        _graphResources (orderTargets graph) `shouldBe` [a,c,d,b]
  describe "remove duplicates" $ do
    it "keep things in order" $ do
      let l = [a,b,c,d]
          l' = reverse l
      removeDuplicatesWith _resKey l `shouldBe` l
      removeDuplicatesWith _resKey l' `shouldBe` l'
    it "removes duplicates" $ do
      removeDuplicatesWith _resKey [d,b,d] `shouldBe` [d,b]

      
