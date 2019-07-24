module FFI.Promise
  ( Promise
  , fromAff
  , toAff
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, runAff_)
import Effect.Exception (Error)

foreign import data Promise :: Type -> Type

foreign import _fromAff :: forall a. ((Error -> Effect Unit) -> (a -> Effect Unit) -> Effect Unit) -> Effect (Promise a)

fromAff :: forall a. Aff a -> Effect (Promise a)
fromAff aff = _fromAff \reject resolve ->
  aff # runAff_ case _ of
    Left reason -> reject reason
    Right value -> resolve value

foreign import _toAff
  :: forall a
   . (Error -> Effect Unit)
  -> (a -> Effect Unit)
  -> Effect (Promise a)
  -> Effect Unit

toAff :: forall a. Effect (Promise a) -> Aff a
toAff promise = makeAff \handler -> do
  _toAff (handler <<< Left) (handler <<< Right) promise
  pure nonCanceler