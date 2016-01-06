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
a = Resource "A" 1 100
b = Resource "B" 2 20
resources = [a,b]
allocations = [Allocation  15 1 2]
Right graph = buildGraph resources allocations


spec :: Spec
spec = do
  describe "Building graph" $ do
    it "can't allocate in the wrong direction" $ do
        isLeft (buildGraph resources [Allocation 15 2 1]) `shouldBe` True
    it "can't allocate more that allowed" $ do
        isLeft (buildGraph resources [Allocation 25 1 2]) `shouldBe` True
    it "detect resources with same keys" $ do
        isLeft (buildGraph ((a&resName .~ "a'") :resources) allocations) `shouldBe` True
  describe "A graph" $ do
    it "should have the right allocations" $ do
        allocated graph a `shouldBe` 15
        allocated graph b `shouldBe` 15
