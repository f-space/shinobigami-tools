module Test.Graph (spec) where

import Prelude

import App.Model (Skill(..), SkillCategoryGap, SkillTable, categories, indices)
import App.Model.Graph (SkillGraph, build, cost, paths, routes)
import App.Model.Graph.Option (Option(..), toTransform)
import App.Model.Table (update)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (sort)
import Data.Enum (toEnum)
import Data.Maybe (Maybe, fromJust, isJust, isNothing)
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
    graph <- fromJustGraph $ build (pure true) $ mempty
    for_ (Skill <$> categories <*> indices) \skill -> do
      cost skill graph `shouldEqual` 0
  it "cost-without-options" do
    graph <- fromJustGraph $ testGraph empty
    cost (skill_ 0 0) graph `shouldEqual` 4
    cost (skill_ 0 8) graph `shouldEqual` 3
    cost (skill_ 1 3) graph `shouldEqual` 3
    cost (skill_ 2 2) graph `shouldEqual` 2
    cost (skill_ 2 9) graph `shouldEqual` 0
    cost (skill_ 3 1) graph `shouldEqual` 3
    cost (skill_ 4 8) graph `shouldEqual` 2
    cost (skill_ 5 0) graph `shouldEqual` 6
    cost (skill_ 5 4) graph `shouldEqual` 4
  it "cost-with-fillgap" do
    graph <- fromJustGraph $ testGraph $ fromFoldable [fillgap1, fillgap2]
    cost (skill_ 0 0) graph `shouldEqual` 3
    cost (skill_ 0 8) graph `shouldEqual` 3
    cost (skill_ 1 3) graph `shouldEqual` 2
    cost (skill_ 2 2) graph `shouldEqual` 2
    cost (skill_ 2 9) graph `shouldEqual` 0
    cost (skill_ 3 1) graph `shouldEqual` 2
    cost (skill_ 4 8) graph `shouldEqual` 2
    cost (skill_ 5 0) graph `shouldEqual` 5
    cost (skill_ 5 4) graph `shouldEqual` 4
  it "cost-with-wrapcat" do
    graph <- fromJustGraph $ testGraph $ singleton wrapcat
    cost (skill_ 0 0) graph `shouldEqual` 4
    cost (skill_ 0 8) graph `shouldEqual` 2
    cost (skill_ 1 3) graph `shouldEqual` 3
    cost (skill_ 2 2) graph `shouldEqual` 2
    cost (skill_ 2 9) graph `shouldEqual` 0
    cost (skill_ 3 1) graph `shouldEqual` 3
    cost (skill_ 4 8) graph `shouldEqual` 2
    cost (skill_ 5 0) graph `shouldEqual` 6
    cost (skill_ 5 4) graph `shouldEqual` 3
  it "cost-with-wrapidx" do
    graph <- fromJustGraph $ testGraph $ singleton wrapidx
    cost (skill_ 0 0) graph `shouldEqual` 4
    cost (skill_ 0 8) graph `shouldEqual` 3
    cost (skill_ 1 3) graph `shouldEqual` 3
    cost (skill_ 2 2) graph `shouldEqual` 2
    cost (skill_ 2 9) graph `shouldEqual` 0
    cost (skill_ 3 1) graph `shouldEqual` 3
    cost (skill_ 4 8) graph `shouldEqual` 2
    cost (skill_ 5 0) graph `shouldEqual` 3
    cost (skill_ 5 4) graph `shouldEqual` 4
  it "cost-with-diag" do
    graph <- fromJustGraph $ testGraph $ singleton diag
    cost (skill_ 0 0) graph `shouldEqual` 4
    cost (skill_ 0 8) graph `shouldEqual` 3
    cost (skill_ 1 3) graph `shouldEqual` 3
    cost (skill_ 2 2) graph `shouldEqual` 2
    cost (skill_ 2 9) graph `shouldEqual` 0
    cost (skill_ 3 1) graph `shouldEqual` 2
    cost (skill_ 4 8) graph `shouldEqual` 2
    cost (skill_ 5 0) graph `shouldEqual` 6
    cost (skill_ 5 4) graph `shouldEqual` 4
  it "cost-with-all" do
    graph <- fromJustGraph $ testGraph $ fromFoldable [fillgap1, fillgap2, wrapcat, wrapidx, diag]
    cost (skill_ 0 0) graph `shouldEqual` 3
    cost (skill_ 0 8) graph `shouldEqual` 2
    cost (skill_ 1 3) graph `shouldEqual` 2
    cost (skill_ 2 2) graph `shouldEqual` 2
    cost (skill_ 2 9) graph `shouldEqual` 0
    cost (skill_ 3 1) graph `shouldEqual` 1
    cost (skill_ 4 8) graph `shouldEqual` 2
    cost (skill_ 5 0) graph `shouldEqual` 3
    cost (skill_ 5 4) graph `shouldEqual` 3
  it "unique-path" do
    graph <- fromJustGraph $ testGraph $ fromFoldable [fillgap1, fillgap2, wrapcat, wrapidx, diag]
    paths (routes (skill_ 0 10) graph) `shouldEqual`
      [ [ skill_ 2 0
        , skill_ 1 10
        , skill_ 0 10
        ]
      ]
  it "single-path" do
    graph <- fromJustGraph $ testGraph $ fromFoldable [fillgap1, fillgap2, wrapcat, wrapidx, diag]
    paths (routes (skill_ 4 3) graph) `shouldEqual`
      [ [ skill_ 2 4
        , skill_ 2 3
        , skill_ 3 3
        , skill_ 4 3
        ]
      ]
  it "multiple-path" do
    graph <- fromJustGraph $ testGraph $ fromFoldable [fillgap1, fillgap2, wrapcat, wrapidx, diag]
    sort (paths (routes (skill_ 2 2) graph)) `shouldEqual`
      [ [ skill_ 2 0
        , skill_ 2 1
        , skill_ 2 2
        ]
      , [ skill_ 2 4
        , skill_ 2 3
        , skill_ 2 2
        ]
      ]
    sort (paths (routes (skill_ 5 2) graph)) `shouldEqual`
      [ [ skill_ 0 5
        , skill_ 0 4
        , skill_ 0 3
        , skill_ 0 2
        , skill_ 5 2
        ]
      , [ skill_ 5 8
        , skill_ 5 9
        , skill_ 5 10
        , skill_ 5 0
        , skill_ 5 1
        , skill_ 5 2
        ]
      ]

fromJustGraph :: forall m. MonadThrow Error m => Maybe SkillGraph -> m SkillGraph
fromJustGraph graph = do
  graph `shouldSatisfy` isJust
  pure $ unsafePartial fromJust graph

testGraph :: Set Option -> Maybe SkillGraph
testGraph options = build skills $ toTransform options
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
