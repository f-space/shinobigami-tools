module App (component) where

import Prelude

import App.Edit as Edit
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

data Page = Home | Edit | View

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
  , edit :: Edit.Slot Unit
  , view :: View.Slot Unit
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

_home :: SProxy "home"
_home = SProxy

_edit :: SProxy "edit"
_edit = SProxy

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
      , HH.slot _edit unit Edit.component (editInput state) handleEditMessage
      , HH.slot _view unit View.component (viewInput state) $ const Nothing
      ]
    , HH.div
      [ HP.id_ "back"
      , HE.onClick \_ -> Go <$> pred state.page
      ]
      [ HH.text "戻る" ]
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
    viewInput { skills, gaps, options } = { skills, gaps, options }

handleHomeMessage :: Home.Message -> Maybe Action
handleHomeMessage Home.Done = Go <$> succ Home

handleEditMessage :: Edit.Message -> Maybe Action
handleEditMessage (Edit.SkillChanged skills) = Just $ SetSkills skills
handleEditMessage (Edit.GapChanged gaps) = Just $ SetGaps gaps
handleEditMessage (Edit.OptionChanged options) = Just $ SetOptions options
handleEditMessage Edit.Done = Go <$> succ Edit

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
