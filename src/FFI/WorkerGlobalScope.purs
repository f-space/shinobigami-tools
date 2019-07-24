module FFI.WorkerGlobalScope
  ( WorkerGlobalScope
  , WindowOrWorkerGlobalScope
  , toEventTarget
  , fromWindow
  , fromWorkerGlobalScope
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import FFI.GlobalThis (GlobalThis)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)
import Web.HTML (Window)

foreign import data WorkerGlobalScope :: Type

foreign import data WindowOrWorkerGlobalScope :: Type

foreign import _fromGlobalThis :: GlobalThis -> Nullable WorkerGlobalScope

fromGlobalThis :: GlobalThis -> Maybe WorkerGlobalScope
fromGlobalThis = toMaybe <<< _fromGlobalThis

toEventTarget :: WorkerGlobalScope -> EventTarget
toEventTarget = unsafeCoerce

fromWindow :: Window -> WindowOrWorkerGlobalScope
fromWindow = unsafeCoerce

fromWorkerGlobalScope :: WorkerGlobalScope -> WindowOrWorkerGlobalScope
fromWorkerGlobalScope = unsafeCoerce
