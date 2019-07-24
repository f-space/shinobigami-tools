module FFI.GlobalThis
  ( GlobalThis
  , globalThis
  ) where

import Effect (Effect)

foreign import data GlobalThis :: Type

foreign import globalThis :: Effect GlobalThis