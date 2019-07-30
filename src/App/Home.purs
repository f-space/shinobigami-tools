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
  HH.section
    [ HP.id_ "home"
    , HP.class_ $ H.ClassName "page"
    ]
    [ HH.h1
      [ HP.class_ $ H.ClassName "title" ]
      [ HH.text "シノビガミ 判定ツール"]
    , HH.div
      [ HP.class_ $ H.ClassName "start"
      , HE.onClick \_ -> Just Start
      ]
      [ HH.text "開始" ]
    , HH.div
      [ HP.class_ $ H.ClassName "recommendation" ]
      [ HH.text "横画面推奨" ]
    , HH.footer
      [ HP.class_ $ H.ClassName "information" ]
      [ HH.span_ [ HH.text "Version" ]
      , HH.span_ [ HH.text "1.0.0" ]
      , HH.span_ [ HH.text "Author" ]
      , HH.span_ 
        [ HH.text "F_"
        , HH.a
          [ HP.class_ $ H.ClassName "contact"
          , HP.href "https://twitter.com/fspace_"
          ]
          [ HH.text "@fspace_" ]
        ]
      , HH.span_ [ HH.text "Project" ]
      , HH.span_ [ HH.a [ HP.href "https://github.com/f-space/shinobigami-tools" ] [ HH.text "GitHub" ] ]
      , HH.span_ [ HH.text "Link" ]
      , HH.span_ [ HH.a [ HP.href "https://coc.f-sp.com/" ] [ HH.text "クトゥルフTRPG ツール" ] ]
      ]
    ]
  where
    label :: forall w i. String -> HH.HTML w i
    label text = HH.span [ HP.class_ $ H.ClassName "info-label" ] [ HH.text text ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  Start -> do
    H.raise Done