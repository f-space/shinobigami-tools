module App (component) where

import Prelude

import App.Edit as Edit
import App.Help as Help
import App.Home as Home
import App.Model (Option, SkillColumn, SkillTable)
import App.View as View
import Data.Const (Const)
import Data.Enum (class Enum, pred, succ)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

data Page = Home | Edit | View

type State =
  { page :: Page 
  , skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  , health :: SkillColumn Boolean
  , paralyses :: SkillTable Boolean
  , barriers :: SkillColumn Boolean
  , help :: Boolean
  }

data Action
  = Go Page
  | SetSkills (SkillTable Boolean)
  | SetGaps (SkillColumn Boolean)
  | SetOptions (Set Option)
  | SetHealth (SkillColumn Boolean)
  | SetParalyses (SkillTable Boolean)
  | SetBarriers (SkillColumn Boolean)
  | SetHelpOpen Boolean
  | DoNothing

type Query :: forall k. k -> Type
type Query = Const Void

type Input = Unit

type Message = Unit

type Slot slot = H.Slot Query Message slot

type ChildSlots =
  ( home :: Home.Slot Unit
  , edit :: Edit.Slot Unit
  , view :: View.Slot Unit
  , help :: Help.Slot Unit
  )

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

derive instance eqPage :: Eq Page
derive instance ordPage :: Ord Page
instance enumPage :: Enum Page where
  pred Home = Nothing
  pred Edit = Just Home
  pred View = Just Edit
  succ Home = Just Edit
  succ Edit = Just View
  succ View = Nothing

goto :: Maybe Page -> Action
goto = maybe DoNothing Go

_home :: Proxy "home"
_home = Proxy

_edit :: Proxy "edit"
_edit = Proxy

_view :: Proxy "view"
_view = Proxy

_help :: Proxy "help"
_help = Proxy

component :: H.Component Query Input Message MonadType
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
  , health: pure true
  , paralyses: pure false
  , barriers: pure false
  , help: false
  }

render :: State -> ComponentHTML
render state @ { page } =
  HH.div
    [ HP.id "container" ]
    [ HH.div
      [ HP.id "track"
      , HP.attr (H.AttrName "data-page") $ pageId page
      ]
      [ HH.slot _home unit Home.component unit handleHomeMessage
      , HH.slot _edit unit Edit.component (editInput state) handleEditMessage
      , HH.slot _view unit View.component (viewInput state) handleViewMessage
      ]
    , HH.div
      [ HP.id "back"
      , HE.onClick \_ -> goto $ pred page
      ]
      [ HH.text "戻る" ]
    , HH.div
      [ HP.id "help-icon"
      , HE.onClick \_ -> SetHelpOpen true
      ]
      []
    , HH.slot _help unit Help.component (helpInput state) handleHelpMessage
    ]
  where
    pageId :: Page -> String
    pageId = case _ of
      Home -> "home"
      Edit -> "edit"
      View -> "view"
    
    editInput :: State -> Edit.Input
    editInput { skills, gaps, options } = { skills, gaps, options }

    viewInput :: State -> View.Input
    viewInput { skills, gaps, options, health, paralyses, barriers } =
      { skills, gaps, options, health, paralyses, barriers }

    helpInput :: State -> Help.Input
    helpInput { help } = { open: help }

handleHomeMessage :: Home.Message -> Action
handleHomeMessage Home.Done = maybe DoNothing Go $ succ Home

handleEditMessage :: Edit.Message -> Action
handleEditMessage (Edit.SkillChanged skills) = SetSkills skills
handleEditMessage (Edit.GapChanged gaps) = SetGaps gaps
handleEditMessage (Edit.OptionChanged options) = SetOptions options
handleEditMessage Edit.Done = goto $ succ Edit

handleViewMessage :: View.Message -> Action
handleViewMessage (View.HealthChanged health) = SetHealth health
handleViewMessage (View.ParalysisChanged paralyses) = SetParalyses paralyses
handleViewMessage (View.BarrierChanged barriers) = SetBarriers barriers

handleHelpMessage :: Help.Message -> Action
handleHelpMessage Help.Closed = SetHelpOpen false

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  Go page -> do
    H.modify_ (_ { page = page })
    case page of
      Edit -> void $ H.tell _edit unit Edit.Reset
      View -> void $ H.tell _view unit View.Reset
      _ -> pure unit
  SetSkills skills -> do
    H.modify_ (_ { skills = skills })
  SetGaps gaps -> do
    H.modify_ (_ { gaps = gaps })
  SetOptions options -> do
    H.modify_ (_ { options = options })
  SetHealth health -> do
    H.modify_ (_ { health = health })
  SetParalyses paralyses -> do
    H.modify_ (_ { paralyses = paralyses })
  SetBarriers barriers -> do
    H.modify_ (_ { barriers = barriers })
  SetHelpOpen open -> do
    H.modify_ (_ { help = open })
  DoNothing ->
    pure unit
