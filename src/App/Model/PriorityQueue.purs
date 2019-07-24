module App.Model.PriorityQueue
  ( PriorityQueue
  , empty
  , null
  , push
  , pop
  , head
  , fromFoldable
  ) where

import Prelude

import Control.Monad.ST (ST, kind Region)
import Data.Foldable (class Foldable, for_)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import data PriorityQueue :: Region -> Type -> Type

foreign import empty :: forall r a. ST r (PriorityQueue r a)

foreign import null :: forall r a. PriorityQueue r a -> ST r Boolean

foreign import _push :: forall r a. Fn2 a (PriorityQueue r a) (ST r Unit)

foreign import _pop :: forall r a. Fn2 (Fn2 a a Int) (PriorityQueue r a) (ST r (Nullable a))

foreign import _head :: forall r a. Fn2 (Fn2 a a Int) (PriorityQueue r a) (ST r (Nullable a))

push :: forall r a. a -> PriorityQueue r a -> ST r Unit
push = runFn2 _push

pop :: forall r a. Ord a => PriorityQueue r a -> ST r (Maybe a)
pop queue = toMaybe <$> runFn2 _pop compareFn queue

head :: forall r a. Ord a => PriorityQueue r a -> ST r (Maybe a)
head queue = toMaybe <$> runFn2 _head compareFn queue

compareFn :: forall a. Ord a => Fn2 a a Int
compareFn = mkFn2 reversed
  where
    reversed :: a -> a -> Int
    reversed a b = case compare a b of
      LT -> 1
      GT -> -1
      EQ -> 0

fromFoldable :: forall r f a. Foldable f => f a -> ST r (PriorityQueue r a)
fromFoldable foldable = do
  queue <- empty
  for_ foldable \x -> push x queue
  pure queue