module FFI.ServiceWorker.ExtendableEvent
  ( ExtendableEvent
  , fromEvent
  , waitUntil
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Promise (Promise, fromAff)
import Web.Event.Event (Event)

foreign import data ExtendableEvent :: Type

foreign import _fromEvent :: Event -> Nullable ExtendableEvent

fromEvent :: Event -> Maybe ExtendableEvent
fromEvent = toMaybe <<< _fromEvent

foreign import _waitUntil :: Effect (Promise Unit) -> ExtendableEvent -> Effect Unit

waitUntil :: Aff Unit -> ExtendableEvent -> Effect Unit
waitUntil = _waitUntil <<< fromAff