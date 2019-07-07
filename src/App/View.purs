module App.View
  ( component
  , Query
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
import Data.Array (concat, cons, drop, head, nub, snoc, zip)
import Data.Const (Const)
import Data.Enum (pred, succ)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Tuple (uncurry)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { mode :: Mode
  , selection :: Maybe Skill
  , health :: SkillColumn Boolean
  , paralysis :: SkillTable Boolean
  , barriers :: SkillColumn Boolean
  , skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

data Mode = RouteMode | CostMode

data Action
  = HandleInput Input
  | SelectMode Mode
  | SelectSkill Skill
  | ToggleHealth SkillCategory
  | ToggleParalysis Skill
  | ToggleBarrier SkillCategoryGap

type Query = Const Void

type Input =
  { skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

type Message = Unit

type Slot slot = H.Slot Query Message slot

type ChildSlots = ( table :: Table.Slot Unit )

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show RouteMode = "route"
  show CostMode = "cost"

_table :: SProxy "table"
_table = SProxy

component :: H.Component HH.HTML Query Input Message MonadType
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
  { mode: RouteMode
  , selection: Nothing
  , health: pure true
  , paralysis: pure false
  , barriers: pure false
  , skills
  , gaps
  , options
  }

render :: State -> ComponentHTML
render { mode, selection, health, paralysis, barriers, skills, gaps, options } =
  HH.div
    [ HP.id_ "view" ]
    [ HH.slot _table unit Table.component tableInput handleMessage
    , HH.div_
      [ HH.text $ "目標値: " <> (maybe "-" (show <<< (_ + 5)) $ MG.cost <$> selection <*> graph) ]
    , HH.div
      [ HP.class_ $ H.ClassName "modes" ]
      [ renderMode RouteMode "経路"
      , renderMode CostMode "分布"
      ]
    , HH.div
      [ HP.class_ $ H.ClassName "options"]
      [ HH.div_ [ renderOption (MO.hasMakaikogaku options) "魔界工学" ]
      , HH.div_ [ renderOption (MO.hasMokuren options) "木蓮" ]
      , HH.div_
        [ renderOption (MO.hasYori options) "妖理"
        , HH.text $ maybe "" display $ head $ MO.yoriSkills options
        ]
      ]
    ]
  where
    graph :: Maybe MG.SkillGraph
    graph =
      let
        enabled = (\a b -> a && not b) <$> MC.toSkillTable health <*> paralysis
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
        , HE.onClick \_ -> Just $ SelectMode value
        ]
        [ HH.text label ]
  
    renderOption :: Boolean -> String -> ComponentHTML
    renderOption value label =
      HH.span
        [ HP.classes $ H.ClassName <$> cons "option" if value then ["checked"] else [] ]
        [ HH.text label ]
    
    tableInput :: Table.Input
    tableInput = { categoryClasses, skillClasses, gapHeaderClasses, gapClasses }

    categoryClasses :: SkillColumn (Array String)
    categoryClasses = (if _ then [] else ["disabled"]) <$> health
  
    skillClasses :: SkillTable (Array String)
    skillClasses = case mode of
      RouteMode ->
        let
          disabled = (if _ then ["disabled"] else []) <$> paralysis
          acquired = (if _ then ["acquired"] else []) <$> skills
          merged = (<>) <$> MC.toSkillTable categoryClasses <*> ((<>) <$> disabled <*> acquired)
        in merged # execState do
          for_ routeSkills \skill -> modify_ $ MT.modify skill (_ `snoc` "route")
          for_ selection \skill -> modify_ $ MT.modify skill (_ `snoc` "selected")
      CostMode ->
        (\x -> ["cost-" <> (show $ min 8 x)]) <$> costTable
    
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
      CostMode ->
        MC.toSkillTable gapHeaderClasses
      where
        onRoute :: SkillCategory -> SkillCategory -> Maybe SkillCategoryGap
        onRoute from to
          | to == fromMaybe top (pred from) = Just $ rightGap to
          | to == fromMaybe bottom (succ from) = Just $ leftGap to
          | otherwise = Nothing

handleMessage :: Table.Message -> Maybe Action
handleMessage (Table.CategoryClicked category) = Just $ ToggleHealth category
handleMessage (Table.SkillClicked skill) = Just $ SelectSkill skill
handleMessage (Table.GapClicked gap) = Just $ ToggleBarrier gap
handleMessage (Table.SkillHeld skill) = Just $ ToggleParalysis skill

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput input -> do
    H.put $ initialState input
  SelectMode mode -> do
    H.modify_ \s -> s { mode = mode }
  SelectSkill skill -> do
    H.modify_ \s -> s { selection = if Just skill == s.selection then Nothing else Just skill }
  ToggleHealth category -> do
    H.modify_ \s -> s { health = MC.modify category not s.health }
  ToggleParalysis skill -> do
    H.modify_ \s -> s { paralysis = MT.modify skill not s.paralysis }
  ToggleBarrier gap -> do
    { gaps } <- H.get
    if not $ MC.lookup gap gaps
      then H.modify_ \s -> s { barriers = MC.modify gap not s.barriers }
      else pure unit