module App.Model.Table
  ( SkillTable
  , fill
  , fillWith
  , lookup
  , update
  , modify
  , mapWithKey
  , collectWithKey
  , collectKeys
  , unsafeWrap
  , unwrap
  ) where

import Prelude

import App.Model.Skill (Skill(..), SkillCategory, SkillIndex, categories, indices)
import Data.Array (fromFoldable, modifyAt, replicate, unsafeIndex, updateAt, zipWith)
import Data.Enum (Cardinality, cardinality, fromEnum)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype as Newtype
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Partial.Unsafe (unsafePartial)

newtype SkillTable a = SkillTable (Array (Array a))

derive newtype instance eqSkillTable :: Eq a => Eq (SkillTable a)

instance functorSkillTable :: Functor SkillTable where
  map f (SkillTable table) = SkillTable $ map f <$> table

instance applySkillTable :: Apply SkillTable where
  apply (SkillTable f) (SkillTable x) = SkillTable $ zipWith (zipWith ($)) f x

instance applicativeSkillTable :: Applicative SkillTable where
  pure = SkillTable <<< replicate categorySize <<< replicate indexSize

instance foldableSkillTable :: Foldable SkillTable where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (SkillTable table) = foldMap identity (foldMap f <$> table)
  
instance traverableSkillTable :: Traversable SkillTable where
  traverse = traverseDefault
  sequence (SkillTable table) = map SkillTable $ sequence $ sequence <$> table

categorySize :: Int
categorySize = Newtype.unwrap (cardinality :: Cardinality SkillCategory)

indexSize :: Int
indexSize = Newtype.unwrap (cardinality :: Cardinality SkillIndex)

decompose :: forall a. Skill -> (Int -> Int -> (Partial => a)) -> a
decompose (Skill category index) f = unsafePartial $ f (fromEnum category) (fromEnum index)

fill :: forall a. a -> SkillTable a
fill = pure

fillWith :: forall a. (Skill -> a) -> SkillTable a
fillWith f = SkillTable $ categories <#> \c -> indices <#> \i -> f $ Skill c i

lookup :: forall a. Skill -> SkillTable a -> a
lookup skill (SkillTable table) =
  decompose skill \c i -> table `unsafeIndex` c `unsafeIndex` i

update :: forall a. Skill -> a -> SkillTable a -> SkillTable a
update skill value (SkillTable table) =
  SkillTable $ decompose skill \c i -> fromJust $ modifyAt c (fromJust <<< updateAt i value) table

modify :: forall a. Skill -> (a -> a) -> SkillTable a -> SkillTable a
modify skill f (SkillTable table) =
  SkillTable $ decompose skill \c i -> fromJust $ modifyAt c (fromJust <<< modifyAt i f) table

mapWithKey :: forall a b. (Skill -> a -> b) -> SkillTable a -> SkillTable b
mapWithKey f table = fillWith \skill -> f skill $ lookup skill table

collectWithKey :: forall a b. (Skill -> a -> Maybe b) -> SkillTable a -> Array b
collectWithKey f table = foldMap fromFoldable $ mapWithKey f table

collectKeys :: forall a. (a -> Boolean) -> SkillTable a -> Array Skill
collectKeys f = collectWithKey \skill x -> if f x then Just skill else Nothing

unsafeWrap :: forall a. Partial => Array (Array a) -> SkillTable a
unsafeWrap = SkillTable

unwrap :: forall a. SkillTable a -> Array (Array a)
unwrap (SkillTable table) = table