module App.Home
  ( component
  , Query
  , Input
  , Message(..)
  , Slot
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Unit

data Action = Start

type Query = Const Void

type Input = Unit

data Message = Done

type Slot slot = H.Slot Query Message slot

type ChildSlots = ()

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

component :: H.Component HH.HTML Query Input Message MonadType
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState = const unit

render :: State -> ComponentHTML
render _ =
  HH.div
    [ HP.id_ "home" ]
    [ HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick \_ -> Just Start
      ]
      [ HH.text "開始" ]
    ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  Start -> do
    H.raise Done