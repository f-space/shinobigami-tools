module Test.Graph (spec) where

import Prelude

import App.Model (Skill(..), SkillCategoryGap, SkillTable, categories, indices)
import App.Model.Graph (SkillGraph, build, route, cost)
import App.Model.Graph.Option (Option(..), toTransform)
import App.Model.Table (update)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Enum (toEnum)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set (Set, empty, fromFoldable, singleton)
import Data.Traversable (for_)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: forall m g. Monad m => MonadThrow Error g => SpecT g Unit m Unit
spec = describe "Graph" do
  it "empty-graph" do
    let result = build (pure false) $ mempty
    result `shouldSatisfy` isNothing
  it "full-graph" do
    let result = build (pure true) $ mempty
    result `shouldSatisfy` isJust

    let graph = unsafePartial fromJust result
    for_ (Skill <$> categories <*> indices) \skill -> do
      cost graph skill `shouldEqual` 0
  it "cost-without-options" do
    let graph = testGraph empty
    cost graph (skill_ 0 0) `shouldEqual` 4
    cost graph (skill_ 0 8) `shouldEqual` 3
    cost graph (skill_ 1 3) `shouldEqual` 3
    cost graph (skill_ 2 2) `shouldEqual` 2
    cost graph (skill_ 2 9) `shouldEqual` 0
    cost graph (skill_ 3 1) `shouldEqual` 3
    cost graph (skill_ 4 8) `shouldEqual` 2
    cost graph (skill_ 5 0) `shouldEqual` 6
    cost graph (skill_ 5 4) `shouldEqual` 4
  it "cost-with-fillgap" do
    let graph = testGraph $ fromFoldable [fillgap1, fillgap2]
    cost graph (skill_ 0 0) `shouldEqual` 3
    cost graph (skill_ 0 8) `shouldEqual` 3
    cost graph (skill_ 1 3) `shouldEqual` 2
    cost graph (skill_ 2 2) `shouldEqual` 2
    cost graph (skill_ 2 9) `shouldEqual` 0
    cost graph (skill_ 3 1) `shouldEqual` 2
    cost graph (skill_ 4 8) `shouldEqual` 2
    cost graph (skill_ 5 0) `shouldEqual` 5
    cost graph (skill_ 5 4) `shouldEqual` 4
  it "cost-with-wrapcat" do
    let graph = testGraph $ singleton wrapcat
    cost graph (skill_ 0 0) `shouldEqual` 4
    cost graph (skill_ 0 8) `shouldEqual` 2
    cost graph (skill_ 1 3) `shouldEqual` 3
    cost graph (skill_ 2 2) `shouldEqual` 2
    cost graph (skill_ 2 9) `shouldEqual` 0
    cost graph (skill_ 3 1) `shouldEqual` 3
    cost graph (skill_ 4 8) `shouldEqual` 2
    cost graph (skill_ 5 0) `shouldEqual` 6
    cost graph (skill_ 5 4) `shouldEqual` 3
  it "cost-with-wrapidx" do
    let graph = testGraph $ singleton wrapidx
    cost graph (skill_ 0 0) `shouldEqual` 4
    cost graph (skill_ 0 8) `shouldEqual` 3
    cost graph (skill_ 1 3) `shouldEqual` 3
    cost graph (skill_ 2 2) `shouldEqual` 2
    cost graph (skill_ 2 9) `shouldEqual` 0
    cost graph (skill_ 3 1) `shouldEqual` 3
    cost graph (skill_ 4 8) `shouldEqual` 2
    cost graph (skill_ 5 0) `shouldEqual` 3
    cost graph (skill_ 5 4) `shouldEqual` 4
  it "cost-with-diag" do
    let graph = testGraph $ singleton diag
    cost graph (skill_ 0 0) `shouldEqual` 4
    cost graph (skill_ 0 8) `shouldEqual` 3
    cost graph (skill_ 1 3) `shouldEqual` 3
    cost graph (skill_ 2 2) `shouldEqual` 2
    cost graph (skill_ 2 9) `shouldEqual` 0
    cost graph (skill_ 3 1) `shouldEqual` 2
    cost graph (skill_ 4 8) `shouldEqual` 2
    cost graph (skill_ 5 0) `shouldEqual` 6
    cost graph (skill_ 5 4) `shouldEqual` 4
  it "cost-with-all" do
    let graph = testGraph $ fromFoldable [fillgap1, fillgap2, wrapcat, wrapidx, diag]
    cost graph (skill_ 0 0) `shouldEqual` 3
    cost graph (skill_ 0 8) `shouldEqual` 2
    cost graph (skill_ 1 3) `shouldEqual` 2
    cost graph (skill_ 2 2) `shouldEqual` 2
    cost graph (skill_ 2 9) `shouldEqual` 0
    cost graph (skill_ 3 1) `shouldEqual` 1
    cost graph (skill_ 4 8) `shouldEqual` 2
    cost graph (skill_ 5 0) `shouldEqual` 3
    cost graph (skill_ 5 4) `shouldEqual` 3
  it "route" do
    let graph = testGraph $ fromFoldable [fillgap1, fillgap2, wrapcat, wrapidx, diag]
    route graph (skill_ 0 10) `shouldEqual`
      [ skill_ 2 0
      , skill_ 1 10
      , skill_ 0 10
      ]

testGraph :: Set Option -> SkillGraph
testGraph options = unsafePartial fromJust $ build skills $ toTransform options
  where
    skills :: SkillTable Boolean
    skills = pure false
      # update (skill_ 0 5) true
      # update (skill_ 2 0) true
      # update (skill_ 2 4) true
      # update (skill_ 2 9) true
      # update (skill_ 5 8) true

fillgap1 :: Option
fillgap1 = StretchGap (-1) $ gap_ 2

fillgap2 :: Option
fillgap2 = StretchGap (-1) $ gap_ 3

wrapcat :: Option
wrapcat = WrapCategory

wrapidx :: Option
wrapidx = WrapIndex

diag :: Option
diag = DiagonalFrom $ skill_ 2 0

gap_ :: Int -> SkillCategoryGap
gap_ value =
  unsafePartial fromJust $ toEnum value

skill_ :: Int -> Int -> Skill
skill_ category index =
  unsafePartial fromJust $ Skill <$> toEnum category <*> toEnum index
