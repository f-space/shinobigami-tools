module FFI.Document
  ( elementFromPoint
  ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.DOM (Document, Element)

foreign import _elementFromPoint :: Fn3 Int Int Document (Effect (Nullable Element))

elementFromPoint :: Int -> Int -> Document -> Effect (Maybe Element)
elementFromPoint x y document = toMaybe <$> (runFn3 _elementFromPoint) x y document