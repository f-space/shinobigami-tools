module App.Edit
  ( component
  , Query(..)
  , Input
  , Message(..)
  , Slot
  ) where

import Prelude

import App.Model (Option(..), Skill, SkillCategory, SkillCategoryGap, SkillColumn, SkillTable, display, leftGap, rightGap')
import App.Model.Column as MC
import App.Model.Option as MO
import App.Model.Table as MT
import App.Table as Table
import Data.Array (catMaybes, head)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty, filter, insert)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State =
  { selectionMode :: Boolean
  , skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

data Action
  = HandleInput Input
  | ToggleCategory SkillCategory
  | ToggleSkill Skill
  | ToggleGap SkillCategoryGap
  | ToggleOption Option
  | ToggleYori
  | SelectYoriSkill Skill
  | Clear
  | Complete
  | DoNothing

data Query a = Reset a

type Input =
  { skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

data Message
  = SkillChanged (SkillTable Boolean)
  | GapChanged (SkillColumn Boolean)
  | OptionChanged (Set Option)
  | Done

type Slot slot = H.Slot Query Message slot

type ChildSlots = ( table :: Table.Slot Unit )

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

_table :: Proxy "table"
_table = Proxy

component :: H.Component Query Input Message MonadType
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< HandleInput
      }
    }

initialState :: Input -> State
initialState { skills, gaps, options } =
  { selectionMode: false, skills, gaps, options }

render :: State -> ComponentHTML
render { selectionMode, skills, gaps, options } =
  HH.section
    [ HP.id "edit"
    , HP.classes $ H.ClassName <$> catMaybes
      [ Just "page"
      , if selectionMode then Just "selection-mode" else Nothing
      ]
    ]
    [ HH.h1 [ HP.class_ $ H.ClassName "heading" ] [ HH.text "設定" ]
    , HH.slot _table unit Table.component tableInput
        if selectionMode then handleSelectionMessage else handleMessage
    , HH.div
      [ HP.class_ $ H.ClassName "options"]
      [ renderOption "魔界工学" 0 (MO.hasMakaikogaku options) false $ ToggleOption Makaikogaku
      , renderOption "木蓮" 1 (MO.hasMokuren options) false $ ToggleOption Mokuren
      , renderOption "妖理" 2 (MO.hasYori options) selectionMode $ ToggleYori
      , MO.yoriSkills options # head # maybe (HH.text "") \skill ->
          HH.span
            [ HP.class_ $ H.ClassName "suboption" ]
            [ HH.text $ display skill ]
      ]
    , HH.div
      [ HP.class_ $ H.ClassName "clear"
      , HE.onClick \_ -> Clear
      ]
      [ HH.text "白紙化"]
    , HH.div
      [ HP.class_ $ H.ClassName "complete"
      , HE.onClick \_ -> Complete
      ]
      [ HH.text "完了" ]
    ]
  where
    renderOption :: String -> Int -> Boolean -> Boolean -> Action -> ComponentHTML
    renderOption label column checked target action =
      HH.span
        [ HP.classes $ H.ClassName <$> catMaybes
          [ Just "option"
          , Just $ "column-" <> show column
          , if checked then Just "checked" else Nothing
          , if target then Just "selection-target" else Nothing
          ]
        , HE.onClick \_ -> action
        ]
        [ HH.text label ]

    tableInput :: Table.Input
    tableInput = { categoryClasses, skillClasses, gapHeaderClasses, gapClasses }
  
    categoryClasses :: SkillColumn (Array String)
    categoryClasses = pure []
  
    skillClasses :: SkillTable (Array String)
    skillClasses = (if _ then ["acquired"] else []) <$> skills

    gapHeaderClasses :: SkillColumn (Array String)
    gapHeaderClasses = (if _ then ["filled"] else []) <$> gaps

    gapClasses :: SkillTable (Array String)
    gapClasses = MC.toSkillTable gapHeaderClasses

handleMessage :: Table.Message -> Action
handleMessage (Table.CategorySelected category) = ToggleCategory category
handleMessage (Table.SkillSelected skill) = ToggleSkill skill
handleMessage (Table.GapSelected gap) = ToggleGap gap

handleSelectionMessage :: Table.Message -> Action
handleSelectionMessage (Table.SkillSelected skill) = SelectYoriSkill skill
handleSelectionMessage _ = DoNothing

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput { skills, gaps, options } -> do
    H.modify_ \s -> s { skills = skills, gaps = gaps, options = options }
  ToggleCategory category -> do
    { gaps } <- H.get
    let left = leftGap category
    let right = rightGap' category
    let value = MC.fillWith \gap -> gap == left || Just gap == right
    H.raise $ GapChanged if value /= gaps then value else MC.fill false
  ToggleSkill skill -> do
    { skills } <- H.get
    H.raise $ SkillChanged $ MT.modify skill not skills
  ToggleGap gap -> do
    { gaps } <- H.get
    H.raise $ GapChanged $ MC.modify gap not gaps
  ToggleOption option -> do
    { options } <- H.get
    H.raise $ OptionChanged $ MO.toggleOption option options
  ToggleYori -> do
    { selectionMode, options } <- H.get
    if selectionMode
      then H.modify_ \s -> s { selectionMode = false }
      else
        if MO.hasYori options
          then H.raise $ OptionChanged $ filter (not <<< MO.isYori) options
          else H.modify_ \s -> s { selectionMode = true }
  SelectYoriSkill skill -> do
    { options } <- H.get
    H.modify_ \s -> s { selectionMode = false }
    H.raise $ OptionChanged $ insert (Yori skill) $ filter (not <<< MO.isYori) options
  Clear -> do
    H.modify_ \s -> s { selectionMode = false }
    H.raise $ SkillChanged $ MT.fill false
    H.raise $ GapChanged $ MC.fill false
    H.raise $ OptionChanged empty
  Complete -> do
    H.raise Done
  DoNothing ->
    pure unit

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message MonadType (Maybe a)
handleQuery = case _ of
  Reset next -> do
    H.modify_ \s -> s { selectionMode = false }
    pure $ Just next