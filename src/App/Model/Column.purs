module App.Model.Column
  ( class ColumnKey
  , toCategory
  , fromCategory
  , SkillColumn
  , fill
  , fillWith
  , lookup
  , update
  , modify
  , mapWithKey
  , collectWithKey
  , collectKeys
  , toSkillTable
  , unsafeWrap
  , unwrap
  ) where

import Prelude

import App.Model.Skill (SkillCategory, SkillCategoryGap, SkillIndex, categories, leftGap, rightCategory)
import App.Model.Table (SkillTable)
import App.Model.Table as Table
import Data.Array (fromFoldable, modifyAt, replicate, unsafeIndex, updateAt, zipWith)
import Data.Enum (Cardinality, cardinality, fromEnum)
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype as Newtype
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafePartial)

class ColumnKey a where
  toCategory :: a -> SkillCategory
  fromCategory :: SkillCategory -> a

instance columnKeySkillCategory :: ColumnKey SkillCategory where
  toCategory = identity
  fromCategory = identity

instance columnKeySkillCategoryGap :: ColumnKey SkillCategoryGap where
  toCategory = rightCategory
  fromCategory = leftGap

newtype SkillColumn a = SkillColumn (Array a)

derive newtype instance eqSkillColumn :: Eq a => Eq (SkillColumn a)

derive newtype instance functorSkillColumn :: Functor SkillColumn

instance applySkillColumn :: Apply SkillColumn where
  apply (SkillColumn f) (SkillColumn x) = SkillColumn $ zipWith ($) f x

instance applicativeSkillColumn :: Applicative SkillColumn where
  pure = SkillColumn <<< replicate categorySize

derive newtype instance foldableSkillColumn :: Foldable SkillColumn

derive newtype instance traversableSkillColumn :: Traversable SkillColumn

categorySize :: Int
categorySize = Newtype.unwrap (cardinality :: Cardinality SkillCategory)

indexSize :: Int
indexSize = Newtype.unwrap (cardinality :: Cardinality SkillIndex)

decompose :: forall k a. ColumnKey k => k -> (Int -> (Partial => a)) -> a
decompose key f = unsafePartial $ f $ fromEnum $ toCategory key

fill :: forall a. a -> SkillColumn a
fill = pure

fillWith :: forall k a. ColumnKey k => (k -> a) -> SkillColumn a
fillWith f = SkillColumn $ categories <#> \c -> f $ fromCategory c

lookup :: forall k a. ColumnKey k => k -> SkillColumn a -> a
lookup key (SkillColumn array) = decompose key \c -> array `unsafeIndex` c

update :: forall k a. ColumnKey k => k -> a -> SkillColumn a -> SkillColumn a
update key value (SkillColumn array) = SkillColumn $ decompose key \c -> fromJust $ updateAt c value array

modify :: forall k a. ColumnKey k => k -> (a -> a) -> SkillColumn a -> SkillColumn a
modify key f (SkillColumn array) = SkillColumn $ decompose key \c -> fromJust $ modifyAt c f array

mapWithKey :: forall k a b. ColumnKey k => (k -> a -> b) -> SkillColumn a -> SkillColumn b
mapWithKey f column = fillWith \key -> f key $ lookup key column

collectWithKey :: forall k a b. ColumnKey k => (k -> a -> Maybe b) -> SkillColumn a -> Array b
collectWithKey f column = foldMap fromFoldable $ mapWithKey f column

collectKeys :: forall k a. ColumnKey k => (a -> Boolean) -> SkillColumn a -> Array k
collectKeys f = collectWithKey \key x -> if f x then Just key else Nothing

toSkillTable :: forall a. SkillColumn a -> SkillTable a
toSkillTable (SkillColumn array) = unsafePartial Table.unsafeWrap $ replicate indexSize <$> array

unsafeWrap :: forall a. Partial => Array a -> SkillColumn a
unsafeWrap = SkillColumn

unwrap :: forall a. SkillColumn a -> Array a
unwrap (SkillColumn array) = array