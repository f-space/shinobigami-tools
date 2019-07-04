module App.Model.Option
  ( Option(..)
  , toGraphOptions
  , isMakaikogaku
  , isMokuren
  , isYori
  , yoriSkill
  , hasMakaikogaku
  , hasMokuren
  , hasYori
  , yoriSkills
  , toggleOption
  ) where

import Prelude

import App.Model.Column (SkillColumn, collectKeys)
import App.Model.Graph.Option as G
import App.Model.Skill (Skill)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Set (Set, delete, filter, fromFoldable, insert, isEmpty, mapMaybe, member, toUnfoldable)

data Option
  = Makaikogaku
  | Mokuren
  | Yori Skill

derive instance eqOption :: Eq Option
derive instance ordOption :: Ord Option

isMakaikogaku :: Option -> Boolean
isMakaikogaku Makaikogaku = true
isMakaikogaku _ = false

isMokuren :: Option -> Boolean
isMokuren Mokuren = true
isMokuren _ = false

isYori :: Option -> Boolean
isYori (Yori _) = true
isYori _ = false

yoriSkill :: Option -> Maybe Skill
yoriSkill (Yori skill) = Just skill
yoriSkill _ = Nothing

hasMakaikogaku :: Set Option -> Boolean
hasMakaikogaku = member Makaikogaku

hasMokuren :: Set Option -> Boolean
hasMokuren = member Mokuren

hasYori :: Set Option -> Boolean
hasYori = not <<< isEmpty <<< filter isYori

yoriSkills :: Set Option -> Array Skill
yoriSkills = toUnfoldable <<< mapMaybe yoriSkill

toggleOption :: Option -> Set Option -> Set Option
toggleOption option options =
  if option `member` options
    then delete option options
    else insert option options

toGraphOptions :: SkillColumn Boolean -> SkillColumn Boolean -> Set Option -> Set G.Option
toGraphOptions gaps barriers options = fromFoldable $
    catMaybes
      [ if Makaikogaku `member` options then Just G.WrapCategory else Nothing
      , if Mokuren `member` options then Just G.WrapIndex else Nothing
      ]
    <> (G.StretchGap (-1) <$> collectKeys identity gaps)
    <> (G.StretchGap 1 <$> collectKeys identity barriers)
    <> (G.DiagonalFrom <$> yoriSkills options)