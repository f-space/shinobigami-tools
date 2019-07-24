module FFI.Fetch
  ( Request
  , Response
  , fetch
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Promise (Promise, toAff)
import FFI.WorkerGlobalScope (WindowOrWorkerGlobalScope)

foreign import data Request :: Type

foreign import data Response :: Type

foreign import _fetch :: Request -> WindowOrWorkerGlobalScope -> Effect (Promise Response)

fetch :: Request -> WindowOrWorkerGlobalScope -> Aff Response
fetch = map toAff <<< _fetch