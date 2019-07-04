module App.Home
  ( component
  , Message(..)
  , Slot
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Unit

data Action = Start

data Message = Done

type Slot index = H.Slot (Const Void) Message index

type ChildSlots = ()

component :: forall q i m. H.Component HH.HTML q i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState = const unit

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render _ =
  HH.div
    [ HP.id_ "home" ]
    [ HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick \_ -> Just Start
      ]
      [ HH.text "開始" ]
    ]

handleAction :: forall m. Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  Start -> do
    H.raise Done