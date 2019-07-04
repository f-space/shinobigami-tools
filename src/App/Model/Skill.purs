module App.Model.Skill
  ( SkillCategory
  , SkillIndex
  , Skill(..)
  , SkillCategoryGap
  , categories
  , indices
  , getCategory
  , getIndex
  , shift
  , shiftWrap
  , leftGap
  , rightGap
  , leftCategory
  , rightCategory
  ) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, enumFromTo, fromEnum, pred, succ, toEnum)
import Data.Maybe (Maybe(..), fromMaybe)

newtype SkillCategory = SkillCategory Int

newtype SkillIndex = SkillIndex Int

data Skill = Skill SkillCategory SkillIndex

newtype SkillCategoryGap = SkillCategoryGap SkillCategory

derive newtype instance eqSkillCategory :: Eq SkillCategory

derive newtype instance ordSkillCategory :: Ord SkillCategory

instance enumSkillCategory :: Enum SkillCategory where
  pred = defaultPred toEnum fromEnum
  succ = defaultSucc toEnum fromEnum

instance boundedSkillCategory :: Bounded SkillCategory where
  top = SkillCategory (categorySize - 1)
  bottom = SkillCategory 0

instance boundedEnumSkillCategory :: BoundedEnum SkillCategory where
  cardinality = Cardinality categorySize
  toEnum i
    | i >= 0 && i < categorySize = Just $ SkillCategory i
    | otherwise = Nothing
  fromEnum (SkillCategory i) = i

instance showSkillCategory :: Show SkillCategory where
  show category = "category " <> (show $ fromEnum category)

derive newtype instance eqSkillIndex :: Eq SkillIndex

derive newtype instance ordSkillIndex :: Ord SkillIndex

instance enumSkillIndex :: Enum SkillIndex where
  pred = defaultPred toEnum fromEnum
  succ = defaultSucc toEnum fromEnum

instance boundedSkillIndex :: Bounded SkillIndex where
  top = SkillIndex (indexSize - 1)
  bottom = SkillIndex 0

instance boundedEnumSkillIndex :: BoundedEnum SkillIndex where
  cardinality = Cardinality indexSize
  toEnum i
    | i >= 0 && i < indexSize = Just $ SkillIndex i
    | otherwise = Nothing
  fromEnum (SkillIndex i) = i

instance showSkillIndex :: Show SkillIndex where
  show index = "index " <> (show $ fromEnum index)

derive instance eqSkill :: Eq Skill

derive instance ordSkill :: Ord Skill

instance showSkill :: Show Skill where
  show (Skill category index) =
    "skill " <> (show $ fromEnum category) <> " " <> (show $ fromEnum index)

derive newtype instance eqSkillCategoryGap :: Eq SkillCategoryGap

derive newtype instance ordSkillCategoryGap :: Ord SkillCategoryGap

derive newtype instance enumSkillCategoryGap :: Enum SkillCategoryGap

derive newtype instance boundedSkillCategoryGap :: Bounded SkillCategoryGap

derive newtype instance boundedEnumSkillCategoryGap :: BoundedEnum SkillCategoryGap

instance showSkillCategoryGap :: Show SkillCategoryGap where
  show gap = "gap " <> (show $ fromEnum gap)

categorySize :: Int
categorySize = 6

categories :: Array SkillCategory
categories = enumFromTo bottom top

indexSize :: Int
indexSize = 11

indices :: Array SkillIndex
indices = enumFromTo bottom top

getCategory :: Skill -> SkillCategory
getCategory (Skill category _) = category

getIndex :: Skill -> SkillIndex
getIndex (Skill _ index) = index

shift :: Skill -> Int -> Int -> Maybe Skill
shift (Skill category index) dx dy =
  let
    c = toEnum $ fromEnum category + dx
    i = toEnum $ fromEnum index + dy
  in Skill <$> c <*> i

shiftWrap :: Skill -> Int -> Int -> Skill
shiftWrap (Skill category index) dx dy =
  let
    c = SkillCategory $ (fromEnum category + dx) `mod` categorySize
    i = SkillIndex $ (fromEnum index + dy) `mod` indexSize
  in Skill c i

leftGap :: SkillCategory -> SkillCategoryGap
leftGap = SkillCategoryGap

rightGap :: SkillCategory -> SkillCategoryGap
rightGap = SkillCategoryGap <<< fromMaybe bottom <<< succ

leftCategory :: SkillCategoryGap -> SkillCategory
leftCategory (SkillCategoryGap category) = fromMaybe top $ pred category

rightCategory :: SkillCategoryGap -> SkillCategory
rightCategory (SkillCategoryGap category) = category