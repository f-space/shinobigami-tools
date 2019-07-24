module FFI.ServiceWorker.CacheStorage
  ( CacheStorage
  , match
  , open
  , delete
  , keys
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Fetch (Request, Response)
import FFI.Promise (Promise, toAff)
import FFI.ServiceWorker.Cache (Cache)

foreign import data CacheStorage :: Type

foreign import _match :: Request -> CacheStorage -> Effect (Promise (Nullable Response))

match :: Request -> CacheStorage -> Aff (Maybe Response)
match = map (map toMaybe <<< toAff) <<< _match

foreign import _open :: String -> CacheStorage -> Effect (Promise Cache)

open :: String -> CacheStorage -> Aff Cache
open = map toAff <<< _open

foreign import _delete :: String -> CacheStorage -> Effect (Promise Boolean)

delete :: String -> CacheStorage -> Aff Boolean
delete = map toAff <<< _delete

foreign import _keys :: CacheStorage -> Effect (Promise (Array String))

keys :: CacheStorage -> Aff (Array String)
keys = toAff <<< _keys