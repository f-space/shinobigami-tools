module App.View
  ( component
  , Query(..)
  , Input
  , Message(..)
  , Slot
  ) where

import Prelude

import App.Model (Option, Skill(..), SkillCategory, SkillCategoryGap, SkillColumn, SkillTable, display, getCategory, getIndex, leftGap, rightCategory, rightGap)
import App.Model.Column as MC
import App.Model.Graph as MG
import App.Model.Graph.Option as MGO
import App.Model.Option as MO
import App.Model.Table as MT
import App.Table as Table
import Control.Monad.State (execState, modify_)
import Data.Array (catMaybes, concat, cons, drop, head, nub, singleton, snoc, zip)
import Data.Enum (pred, succ)
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.String (fromCodePointArray, toCodePointArray)
import Data.Tuple (uncurry)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State =
  { mode :: Mode
  , selection :: Maybe Skill
  , skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  , health :: SkillColumn Boolean
  , paralyses :: SkillTable Boolean
  , barriers :: SkillColumn Boolean
  }

data Mode = RouteMode | CostMode | SelectionMode

data Action
  = HandleInput Input
  | ChangeMode Mode
  | SelectSkill Skill
  | ToggleSelection Skill
  | ToggleHealth SkillCategory
  | ToggleParalysis Skill
  | ToggleBarrier SkillCategoryGap

data Query a = Reset a

type Input =
  { skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  , health :: SkillColumn Boolean
  , paralyses :: SkillTable Boolean
  , barriers :: SkillColumn Boolean
  }

data Message
  = HealthChanged (SkillColumn Boolean)
  | ParalysisChanged (SkillTable Boolean)
  | BarrierChanged (SkillColumn Boolean)

type Slot slot = H.Slot Query Message slot

type ChildSlots = ( table :: Table.Slot Unit )

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show RouteMode = "route-mode"
  show CostMode = "cost-mode"
  show SelectionMode = "selection-mode"

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
initialState { skills, gaps, options, health, paralyses, barriers } =
  { mode: RouteMode
  , selection: Nothing
  , health
  , paralyses
  , barriers
  , skills
  , gaps
  , options
  }

render :: State -> ComponentHTML
render { mode, selection, health, paralyses, barriers, skills, gaps, options } =
  HH.section
    [ HP.id "view"
    , HP.classes $ H.ClassName <$> ["page", show mode]
    ]
    [ HH.h1 [ HP.class_ $ H.ClassName "heading" ] [ HH.text "判定" ]
    , HH.slot _table unit Table.component tableInput handleMessage
    , HH.div
      [ HP.class_ $ H.ClassName "result" ]
      [ HH.span
        [ HP.class_ $ H.ClassName "result-label" ]
        ( HH.span_ <<< singleton <<< HH.text <<< fromCodePointArray <<< singleton <$> toCodePointArray "目標値" )
      , HH.span
        [ HP.class_ $ H.ClassName "result-value" ]
        [ HH.text $ maybe "無" (show <<< (_ + 5)) $ MG.cost <$> selection <*> graph ]
      ]
    , HH.div
      [ HP.class_ $ H.ClassName "modes" ]
      [ renderMode RouteMode "経路"
      , renderMode CostMode "分布"
      ]
    , HH.div
      [ HP.class_ $ H.ClassName "options" ]
      [ renderOption "魔界工学" 0 (MO.hasMakaikogaku options)
      , renderOption "木蓮" 1 (MO.hasMokuren options)
      , renderOption "妖理" 2 (MO.hasYori options)
      , MO.yoriSkills options # head # maybe (HH.text "") \skill ->
          HH.span
            [ HP.class_ $ H.ClassName "suboption" ]
            [ HH.text $ display skill ]
      , HH.span
        [ HP.classes $ H.ClassName <$> catMaybes
          [ Just "option"
          , Just "column-3"
          , Just "paralysis"
          , if foldl (||) false paralyses then Just "checked" else Nothing
          , if inSelectionMode then Just "selection-target" else Nothing
          ]
        , HE.onClick \_ -> ChangeMode if inSelectionMode then RouteMode else SelectionMode
        ]
        [ HH.text "マヒ" ]
      ]
    ]
  where
    inSelectionMode :: Boolean
    inSelectionMode = case mode of
      SelectionMode -> true
      _ -> false
  
    graph :: Maybe MG.SkillGraph
    graph =
      let
        enabled = (\a b -> a && not b) <$> MC.toSkillTable health <*> paralyses
        table = (&&) <$> skills <*> enabled
        transform = MGO.toTransform $ MO.toGraphOptions gaps barriers options
      in MG.build table transform

    routes :: MG.Routes
    routes = fromMaybe mempty $ MG.routes <$> selection <*> graph

    subpaths :: Array (Array Skill)
    subpaths = MG.subpaths routes

    routeSkills :: Array Skill
    routeSkills = nub $ concat subpaths

    costTable :: SkillTable Int
    costTable = maybe (MT.fill $ top) (MT.fillWith <<< flip MG.cost) graph
  
    renderMode :: Mode -> String -> ComponentHTML
    renderMode value label =
      HH.span
        [ HP.classes $ H.ClassName <$> cons "mode" if value == mode then ["checked"] else []
        , HE.onClick \_ -> ChangeMode value
        ]
        [ HH.text label ]
  
    renderOption :: String -> Int -> Boolean -> ComponentHTML
    renderOption label column checked =
      HH.span
        [ HP.classes $ H.ClassName <$> catMaybes
          [ Just "option"
          , Just $ "column-" <> show column
          , Just "readonly"
          , if checked then Just "checked" else Nothing
          ]
        ]
        [ HH.text label ]
    
    tableInput :: Table.Input
    tableInput = { categoryClasses, skillClasses, gapHeaderClasses, gapClasses }

    categoryClasses :: SkillColumn (Array String)
    categoryClasses = (if _ then [] else ["disabled"]) <$> health
  
    skillClasses :: SkillTable (Array String)
    skillClasses =
      let
        disabled = MC.toSkillTable $ (if _ then [] else ["disabled"]) <$> health
        paralyzed = (if _ then ["paralyzed"] else []) <$> paralyses
        acquired = (if _ then ["acquired"] else []) <$> skills
        cost = (singleton <<< append "cost-" <<< show) <$> costTable
        merged = foldl (apply <<< map append) disabled [ paralyzed, acquired, cost ]
      in merged # execState do
        for_ routeSkills \skill -> modify_ $ MT.modify skill (_ `snoc` "route")
        for_ selection \skill -> modify_ $ MT.modify skill (_ `snoc` "selected")
    
    gapHeaderClasses :: SkillColumn (Array String)
    gapHeaderClasses =
      let
        filled = (if _ then ["filled"] else []) <$> gaps
        widen = (if _ then ["widen"] else []) <$> barriers
      in (<>) <$> filled <*> widen

    gapClasses :: SkillTable (Array String)
    gapClasses = case mode of
      RouteMode ->
        MC.toSkillTable gapHeaderClasses # execState do
          for_ subpaths \subpath -> do
            let zipped = zip subpath (drop 1 subpath)
            for_ zipped $ uncurry \x y ->
              case onRoute (getCategory x) (getCategory y) of
                Just gap ->
                  let skill = Skill (rightCategory gap) (getIndex y)
                  in modify_ $ MT.modify skill (_ `snoc` "route")
                Nothing -> pure unit
      _ ->
        MC.toSkillTable gapHeaderClasses
      where
        onRoute :: SkillCategory -> SkillCategory -> Maybe SkillCategoryGap
        onRoute from to
          | to == fromMaybe top (pred from) = Just $ rightGap to
          | to == fromMaybe bottom (succ from) = Just $ leftGap to
          | otherwise = Nothing

handleMessage :: Table.Message -> Action
handleMessage (Table.CategorySelected category) = ToggleHealth category
handleMessage (Table.SkillSelected skill) = SelectSkill skill
handleMessage (Table.GapSelected gap) = ToggleBarrier gap

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput { skills, gaps, options, health, paralyses, barriers } -> do
    H.modify_ \s -> s
      { skills = skills
      , gaps = gaps
      , options = options
      , health = health
      , paralyses = paralyses
      , barriers = barriers
      }
  ChangeMode mode -> do
    H.modify_ \s -> s { mode = mode }
  SelectSkill skill -> do
    { mode } <- H.get
    case mode of
      SelectionMode -> do
        H.modify_ \s -> s { mode = RouteMode }
        handleAction $ ToggleParalysis skill
      _ ->
        handleAction $ ToggleSelection skill
  ToggleSelection skill -> do
    H.modify_ \s -> s { selection = if Just skill == s.selection then Nothing else Just skill }
  ToggleHealth category -> do
    { health } <- H.get
    H.raise $ HealthChanged $ MC.modify category not health
  ToggleParalysis skill -> do
    { paralyses } <- H.get
    H.raise $ ParalysisChanged $ MT.modify skill not paralyses
  ToggleBarrier gap -> do
    { gaps, barriers } <- H.get
    if not $ MC.lookup gap gaps
      then H.raise $ BarrierChanged $ MC.modify gap not barriers
      else pure unit

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message MonadType (Maybe a)
handleQuery = case _ of
  Reset next -> do
    H.modify_ \s -> s { mode = RouteMode, selection = Nothing }
    pure $ Just next