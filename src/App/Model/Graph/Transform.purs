module App.Model.Graph.Transform
  ( wrapCategory
  , wrapIndex
  , stretchGap
  , diagonalFrom
  ) where

import Prelude

import App.Model.Graph (Transform(..))
import App.Model.Skill (Skill(..), SkillCategoryGap, getCategory, getIndex, leftCategory, rightCategory)
import Data.Array (filter, null)

wrapCategory :: Transform
wrapCategory = Transform \skill _ edges ->
  let
    category = getCategory skill
    index = getIndex skill
  in edges
    <> if category == top
      then [{ cost: 2, to: Skill bottom index }]
      else []
    <> if category == bottom
      then [{ cost: 2, to: Skill top index }]
      else []

wrapIndex :: Transform
wrapIndex = Transform \skill _ edges ->
  let
    category = getCategory skill
    index = getIndex skill
  in edges
    <> if index == top
      then [{ cost: 1, to: Skill category bottom }]
      else []
    <> if index == bottom
      then [{ cost: 1, to: Skill category top }]
      else []

stretchGap :: Int -> SkillCategoryGap -> Transform
stretchGap delta gap = Transform \skill _ edges ->
  edges <#> \edge ->
    if getIndex skill == getIndex edge.to &&
        let
          from = getCategory skill
          to = getCategory edge.to
          l = leftCategory gap
          r = rightCategory gap
        in (from == l && to == r) || (from == r && to == l)
      then edge { cost = edge.cost + delta }
      else edge

diagonalFrom :: Skill -> Transform
diagonalFrom target = Transform \skill path edges ->
  edges <>
    if null path && skill == target
      then
        let
          category = getCategory skill
          index = getIndex skill
          edgeV = filter (\e -> getCategory e.to == category) edges
          edgeH = filter (\e -> getIndex e.to == index) edges
        in do
          v <- edgeV
          h <- edgeH
          let cost = v.cost + h.cost - 1
          let to = Skill (getCategory h.to) (getIndex v.to)
          pure { cost, to }
      else []