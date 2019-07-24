module FFI.ServiceWorker.FetchEvent
  ( FetchEvent
  , fromEvent
  , request
  , respondWith
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Fetch (Request, Response)
import FFI.Promise (Promise, fromAff)
import Web.Event.Event (Event)

foreign import data FetchEvent :: Type

foreign import _fromEvent :: Event -> Nullable FetchEvent

fromEvent :: Event -> Maybe FetchEvent
fromEvent = toMaybe <<< _fromEvent

foreign import request :: FetchEvent -> Request

foreign import _respondWith :: Effect (Promise Response) -> FetchEvent -> Effect Unit

respondWith :: Aff Response -> FetchEvent -> Effect Unit
respondWith = _respondWith <<< fromAff