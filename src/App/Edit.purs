module App.Edit
  ( component
  , Query
  , Input
  , Message(..)
  , Slot
  ) where

import Prelude

import App.Model (Option(..), Skill, SkillCategory, SkillCategoryGap, SkillColumn, SkillTable, display, leftGap, rightGap)
import App.Model.Column as MC
import App.Model.Option as MO
import App.Model.Table as MT
import App.Table as Table
import Data.Array (catMaybes, head)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty, filter, insert)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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

type Query = Const Void

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

type ComponentHTML = H.ComponentHTML Action ChildSlots Aff

_table :: SProxy "table"
_table = SProxy

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< HandleInput
      }
    }

initialState :: Input -> State
initialState { skills, gaps, options } =
  { selectionMode: false, skills, gaps, options }

render :: State -> ComponentHTML
render { selectionMode, skills, gaps, options } =
  HH.section
    [ HP.id_ "edit"
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
      , HE.onClick \_ -> Just Clear
      ]
      [ HH.text "白紙化"]
    , HH.div
      [ HP.class_ $ H.ClassName "complete"
      , HE.onClick \_ -> Just Complete
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
        , HE.onClick \_ -> Just action
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

handleMessage :: Table.Message -> Maybe Action
handleMessage (Table.CategorySelected category) = Just $ ToggleCategory category
handleMessage (Table.SkillSelected skill) = Just $ ToggleSkill skill
handleMessage (Table.GapSelected gap) = Just $ ToggleGap gap

handleSelectionMessage :: Table.Message -> Maybe Action
handleSelectionMessage (Table.SkillSelected skill) = Just $ SelectYoriSkill skill
handleSelectionMessage _ = Nothing

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  HandleInput { skills, gaps, options } -> do
    H.modify_ \s -> s { skills = skills, gaps = gaps, options = options }
  ToggleCategory category -> do
    { gaps } <- H.get
    let left = leftGap category
    let right = rightGap category
    let value = MC.fillWith \gap -> gap == left || gap == right
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
    H.raise $ SkillChanged $ MT.fill false
    H.raise $ GapChanged $ MC.fill false
    H.raise $ OptionChanged empty
  Complete -> do
    H.raise Done