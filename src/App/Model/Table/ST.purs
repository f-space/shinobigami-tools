module App.Model.Table.ST
  ( STSkillTable
  , fill
  , lookup
  , update
  , modify
  , freeze
  , thaw
  ) where

import Prelude

import App.Model.Skill (Skill(..), SkillCategory, SkillIndex)
import App.Model.Table (SkillTable, unsafeWrap, unwrap)
import Control.Monad.ST (ST)
import Data.Array (replicate, unsafeIndex)
import Data.Array.ST as ST
import Data.Enum (Cardinality, cardinality, fromEnum)
import Data.Maybe (fromJust)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

newtype STSkillTable r a = STSkillTable (Array (ST.STArray r a))

categorySize :: Int
categorySize = Newtype.unwrap (cardinality :: Cardinality SkillCategory)

indexSize :: Int
indexSize = Newtype.unwrap (cardinality :: Cardinality SkillIndex)

decompose :: forall a. Skill -> (Int -> Int -> (Partial => a)) -> a
decompose (Skill category index) f = unsafePartial $ f (fromEnum category) (fromEnum index)

fill :: forall r a. a -> ST r (STSkillTable r a)
fill x = map STSkillTable $ traverse ST.thaw $ replicate categorySize $ replicate indexSize x

lookup :: forall r a. Skill -> STSkillTable r a -> ST r a
lookup skill (STSkillTable table) =
  decompose skill \c i -> map fromJust $ ST.peek i $ table `unsafeIndex` c

update :: forall r a. Skill -> a -> STSkillTable r a -> ST r Unit
update skill value (STSkillTable table) =
  decompose skill \c i -> void $ ST.poke i value $ table `unsafeIndex` c

modify :: forall r a. Skill -> (a -> a) -> STSkillTable r a -> ST r Unit
modify skill f (STSkillTable table) =
  decompose skill \c i -> void $ ST.modify i f $ table `unsafeIndex` c

freeze :: forall r a. STSkillTable r a -> ST r (SkillTable a)
freeze (STSkillTable table) = unsafePartial unsafeWrap <$> traverse ST.freeze table

thaw :: forall r a. SkillTable a -> ST r (STSkillTable r a)
thaw table = map STSkillTable $ traverse ST.thaw $ unwrap table