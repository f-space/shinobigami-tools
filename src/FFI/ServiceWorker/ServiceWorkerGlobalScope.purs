module FFI.ServiceWorker.ServiceWorkerGlobalScope
  ( ServiceWorkerGlobalScope
  , fromGlobalThis
  , toWorkerGlobalScope
  , toWindowOrWorkerGlobalScope
  , toEventTarget
  , caches
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import FFI.GlobalThis (GlobalThis)
import FFI.ServiceWorker.CacheStorage (CacheStorage)
import FFI.WorkerGlobalScope (WindowOrWorkerGlobalScope, WorkerGlobalScope)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)

foreign import data ServiceWorkerGlobalScope :: Type

foreign import _fromGlobalThis :: GlobalThis -> Nullable ServiceWorkerGlobalScope

fromGlobalThis :: GlobalThis -> Maybe ServiceWorkerGlobalScope
fromGlobalThis = toMaybe <<< _fromGlobalThis

toWorkerGlobalScope :: ServiceWorkerGlobalScope -> WorkerGlobalScope
toWorkerGlobalScope = unsafeCoerce

toWindowOrWorkerGlobalScope :: ServiceWorkerGlobalScope -> WindowOrWorkerGlobalScope
toWindowOrWorkerGlobalScope = unsafeCoerce

toEventTarget :: ServiceWorkerGlobalScope -> EventTarget
toEventTarget = unsafeCoerce

foreign import caches :: ServiceWorkerGlobalScope -> Effect CacheStorage