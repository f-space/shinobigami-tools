module FFI.CustomEvent
  ( CustomEvent
  , CustomEventInitFields
  , customEvent
  , unsafeFromEvent
  , toEvent
  , detail
  ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)

foreign import data CustomEvent :: Type -> Type

type CustomEventInitFields a =
  ( bubbles :: Boolean
  , cancelable :: Boolean
  , composed :: Boolean
  , detail :: a
  )

foreign import customEvent_ :: forall r a. Fn2 String (Record r) (Effect (CustomEvent a))

customEvent :: forall r rx a. Union r rx (CustomEventInitFields a) => String -> Record r -> Effect (CustomEvent a)
customEvent = runFn2 customEvent_

unsafeFromEvent :: forall a. Event -> CustomEvent a
unsafeFromEvent = unsafeCoerce

toEvent :: forall a. CustomEvent a -> Event
toEvent = unsafeCoerce

foreign import detail :: forall a. CustomEvent a -> a