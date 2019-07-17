module TableFFI
  ( CustomEvent
  , CustomEventInitFields
  , elementFromPoint
  , customEvent
  , unsafeFromEvent
  , toEvent
  , detail
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Document, Element)
import Web.Event.Event (Event)

foreign import data CustomEvent :: Type -> Type

type CustomEventInitFields a =
  ( bubbles :: Boolean
  , cancelable :: Boolean
  , composed :: Boolean
  , detail :: a
  )

foreign import elementFromPoint_ :: Fn3 Int Int Document (Effect (Nullable Element))

elementFromPoint :: Int -> Int -> Document -> Effect (Maybe Element)
elementFromPoint x y document = toMaybe <$> (runFn3 elementFromPoint_) x y document

foreign import customEvent_ :: forall r a. Fn2 String (Record r) (Effect (CustomEvent a))

customEvent :: forall r rx a. Union r rx (CustomEventInitFields a) => String -> Record r -> Effect (CustomEvent a)
customEvent = runFn2 customEvent_

unsafeFromEvent :: forall a. Event -> CustomEvent a
unsafeFromEvent = unsafeCoerce

toEvent :: forall a. CustomEvent a -> Event
toEvent = unsafeCoerce

foreign import detail :: forall a. CustomEvent a -> a