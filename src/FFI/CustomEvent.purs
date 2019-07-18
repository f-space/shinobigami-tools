module FFI.CustomEvent
  ( CustomEvent
  , CustomEventInitFields
  , eventType
  , customEvent
  , fromEvent
  , unsafeFromEvent
  , toEvent
  , detail
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Prim.Row (class Union)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..), type_)

foreign import data CustomEvent :: Symbol -> Type -> Type

type CustomEventInitFields a =
  ( bubbles :: Boolean
  , cancelable :: Boolean
  , composed :: Boolean
  , detail :: a
  )

eventType :: forall s a. IsSymbol s => Proxy (CustomEvent s a) -> EventType
eventType _ = EventType $ reflectSymbol (SProxy :: SProxy s)

foreign import customEvent_ :: forall r s a. Fn2 String (Record r) (Effect (CustomEvent s a))

customEvent
  :: forall r r' s a
   . Union r r' (CustomEventInitFields a)
  => IsSymbol s
  => Proxy (CustomEvent s a) -> Record r -> Effect (CustomEvent s a)
customEvent _ = runFn2 customEvent_ $ reflectSymbol (SProxy :: SProxy s)

fromEvent :: forall s a. IsSymbol s => Proxy (CustomEvent s a) -> Event -> Maybe (CustomEvent s a)
fromEvent _ event =
  if (unwrap $ type_ event) == reflectSymbol (SProxy :: SProxy s)
    then Just $ unsafeCoerce event
    else Nothing

unsafeFromEvent :: forall s a. Event -> CustomEvent s a
unsafeFromEvent = unsafeCoerce

toEvent :: forall s a. CustomEvent s a -> Event
toEvent = unsafeCoerce

foreign import detail :: forall s a. CustomEvent s a -> a