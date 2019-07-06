module App (component) where

import Prelude

import App.Config as Config
import App.Home as Home
import App.Model (Option, SkillColumn, SkillTable)
import App.View as View
import Data.Const (Const)
import Data.Enum (class Enum, pred, succ)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Page = Home | Config | View

type State =
  { page :: Page 
  , skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

data Action
  = Go Page
  | SetSkills (SkillTable Boolean)
  | SetGaps (SkillColumn Boolean)
  | SetOptions (Set Option)

type Query = Const Void

type Input = Unit

type Message = Unit

type Slot slot = H.Slot Query Message slot

type ChildSlots =
  ( home :: Home.Slot Unit
  , config :: Config.Slot Unit
  , view :: View.Slot Unit
  )

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

derive instance eqPage :: Eq Page
derive instance ordPage :: Ord Page
instance enumPage :: Enum Page where
  pred Home = Nothing
  pred Config = Just Home
  pred View = Just Config
  succ Home = Just Config
  succ Config = Just View
  succ View = Nothing

_home :: SProxy "home"
_home = SProxy

_config :: SProxy "config"
_config = SProxy

_view :: SProxy "view"
_view = SProxy

component :: H.Component HH.HTML Query Input Message MonadType
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { page: Home
  , skills: pure false
  , gaps: pure false
  , options: empty
  }

render :: State -> ComponentHTML
render state =
  HH.div
    [ HP.id_ "container" ]
    [ HH.div
      [ HP.id_ "track"
      , HP.attr (H.AttrName "data-page") $ pageId state.page
      ]
      [ HH.slot _home unit Home.component unit handleHomeMessage
      , HH.slot _config unit Config.component (configInpug state) handleConfigMessage
      , HH.slot _view unit View.component (viewInput state) $ const Nothing
      ]
    , HH.button
      [ HP.id_ "back"
      , HP.type_ HP.ButtonButton
      , HE.onClick \_ -> Go <$> pred state.page
      ]
      [ HH.text "戻る" ]
    ]
  where
    pageId :: Page -> String
    pageId = case _ of
      Home -> "home"
      Config -> "config"
      View -> "view"
    
    configInpug :: State -> Config.Input
    configInpug { skills, gaps, options } = { skills, gaps, options }

    viewInput :: State -> View.Input
    viewInput { skills, gaps, options } = { skills, gaps, options }

handleHomeMessage :: Home.Message -> Maybe Action
handleHomeMessage Home.Done = Go <$> succ Home

handleConfigMessage :: Config.Message -> Maybe Action
handleConfigMessage (Config.SkillChanged skills) = Just $ SetSkills skills
handleConfigMessage (Config.GapChanged gaps) = Just $ SetGaps gaps
handleConfigMessage (Config.OptionChanged options) = Just $ SetOptions options
handleConfigMessage Config.Done = Go <$> succ Config

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  Go p -> do
    H.modify_ (_ { page = p })
  SetSkills skills -> do
    H.modify_ (_ { skills = skills })
  SetGaps gaps -> do
    H.modify_ (_ { gaps = gaps })
  SetOptions options -> do
    H.modify_ (_ { options = options })
