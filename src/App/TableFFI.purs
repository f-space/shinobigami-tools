module TableFFI
  ( CustomEvent
  , CustomEventInit
  , elementFromPoint
  , newCustomEvent
  , unsafeFromEvent
  , toEvent
  , defaultEventInit
  , detail
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Document, Element)
import Web.Event.Event (Event)

foreign import data CustomEvent :: Type -> Type

type CustomEventInit a =
  { bubbles :: Boolean
  , cancelable :: Boolean
  , composed :: Boolean
  , detail :: a
  }

foreign import elementFromPoint_ :: Fn3 Int Int Document (Effect (Nullable Element))

foreign import newCustomEvent_ :: forall a. Fn2 String (CustomEventInit a) (CustomEvent a)

foreign import detail :: forall a. CustomEvent a -> a

elementFromPoint :: Int -> Int -> Document -> Effect (Maybe Element)
elementFromPoint x y document = toMaybe <$> (runFn3 elementFromPoint_) x y document

newCustomEvent :: forall a. String -> CustomEventInit a -> CustomEvent a
newCustomEvent = runFn2 newCustomEvent_

unsafeFromEvent :: forall a. Event -> CustomEvent a
unsafeFromEvent = unsafeCoerce

toEvent :: forall a. CustomEvent a -> Event
toEvent = unsafeCoerce

defaultEventInit :: forall a. CustomEventInit a
defaultEventInit = unsafeCoerce {}