module App.Model.Graph.Transform
  ( betweenCategory
  , betweenIndex
  , wrapCategory
  , wrapIndex
  , stretchGap
  , diagonalFrom
  ) where

import Prelude

import App.Model.Graph (Transform(..))
import App.Model.Skill (Skill(..), SkillCategoryGap, getCategory, getIndex, leftCategory, rightCategory)
import Data.Array (catMaybes, filter)
import Data.Enum (pred, succ)
import Data.Maybe (Maybe(..))
import Data.Set (member)

betweenCategory :: Int -> Int -> Transform
betweenCategory priority cost = Transform \skill _ edges ->
  edges <>
    let
      category = getCategory skill
      index = getIndex skill
      makeEdge c = { priority, cost, to: Skill c index }
    in makeEdge <$> catMaybes [pred category, succ category]

betweenIndex :: Int -> Int -> Transform
betweenIndex priority cost = Transform \skill _ edges ->
  edges <>
    let
      category = getCategory skill
      index = getIndex skill
      makeEdge i = { priority, cost, to: Skill category i }
    in makeEdge <$> catMaybes [pred index, succ index]

wrapCategory :: Int -> Int -> Transform
wrapCategory priority cost = Transform \skill _ edges ->
  edges <>
    let
      category = getCategory skill
      index = getIndex skill
      makeEdge c = { priority, cost, to: Skill c index }
    in makeEdge <$> catMaybes
      [ if category == top then Just bottom else Nothing
      , if category == bottom then Just top else Nothing
      ]

wrapIndex :: Int -> Int -> Transform
wrapIndex priority cost = Transform \skill _ edges ->
  edges <>
    let
      category = getCategory skill
      index = getIndex skill
      makeEdge i = { priority, cost, to: Skill category i }
    in makeEdge <$> catMaybes
      [ if index == top then Just bottom else Nothing
      , if index == bottom then Just top else Nothing
      ]

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
diagonalFrom target = Transform \skill starts edges ->
  edges <>
    if skill == target && target `member` starts
      then
        let
          category = getCategory skill
          index = getIndex skill
          edgeV = filter (\e -> getCategory e.to == category) edges
          edgeH = filter (\e -> getIndex e.to == index) edges
        in do
          v <- edgeV
          h <- edgeH
          let priority = max v.priority h.priority + 1
          let cost = v.cost + h.cost - 1
          let to = Skill (getCategory h.to) (getIndex v.to)
          pure { priority, cost, to }
      else []