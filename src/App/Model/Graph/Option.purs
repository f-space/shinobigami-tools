module App.Model.Graph.Option
  ( Option(..)
  , toTransform
  ) where

import Prelude

import App.Model.Graph (Transform)
import App.Model.Graph.Transform (betweenCategory, betweenIndex, diagonalFrom, stretchGap, wrapCategory, wrapIndex)
import App.Model.Skill (Skill, SkillCategoryGap)
import Data.Array (catMaybes, mapMaybe)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Set (Set, member, toUnfoldable)
import Data.Tuple (Tuple(..), uncurry)

data Option
  = WrapCategory
  | WrapIndex
  | StretchGap Int SkillCategoryGap
  | DiagonalFrom Skill

derive instance eqOption :: Eq Option
derive instance ordOption :: Ord Option

toTransform :: Set Option -> Transform
toTransform options =
  let
    array = toUnfoldable options
    gaps = mapMaybe stretchedGaps array
    diagonal = mapMaybe diagonalTarget array
  in fold $
    catMaybes
      [ Just $ betweenCategory 1 2
      , Just $ betweenIndex 0 1
      , if WrapCategory `member` options then Just $ wrapCategory 1 2 else Nothing
      , if WrapIndex `member` options then Just $ wrapIndex 0 1 else Nothing
      ]
    <> (uncurry stretchGap <$> gaps)
    <> (diagonalFrom <$> diagonal)

stretchedGaps :: Option -> Maybe (Tuple Int SkillCategoryGap)
stretchedGaps (StretchGap delta gap) = Just $ Tuple delta gap
stretchedGaps _ = Nothing

diagonalTarget :: Option -> Maybe Skill
diagonalTarget (DiagonalFrom skill) = Just skill
diagonalTarget _ = Nothing