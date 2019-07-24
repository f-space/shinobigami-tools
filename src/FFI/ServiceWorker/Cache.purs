module FFI.ServiceWorker.Cache
  ( Cache
  , addAll
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Promise (Promise, toAff)

foreign import data Cache :: Type

foreign import _addAll :: Array String -> Cache -> Effect (Promise Unit)

addAll :: Array String -> Cache -> Aff Unit
addAll = map toAff <<< _addAll
