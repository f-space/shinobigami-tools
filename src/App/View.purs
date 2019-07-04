module App.View
  ( component
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
import Data.Array (cons, drop, head, snoc, zip)
import Data.Const (Const)
import Data.Enum (pred, succ)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Tuple (uncurry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { mode :: Mode
  , selection :: Maybe Skill
  , health :: SkillColumn Boolean
  , barriers :: SkillColumn Boolean
  , skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

data Mode = RouteMode | CostMode

data Action
  = HandleInput Input
  | SelectMode Mode
  | ToggleCategory SkillCategory
  | SelectSkill Skill
  | ToggleBarrier SkillCategoryGap

type Input =
  { skills :: SkillTable Boolean
  , gaps :: SkillColumn Boolean
  , options :: Set Option
  }

type Message = Unit

type Slot index = H.Slot (Const Void) Message index

type ChildSlots = ( table :: Table.Slot Unit )

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show RouteMode = "route"
  show CostMode = "cost"

_table :: SProxy "table"
_table = SProxy

component :: forall q m. H.Component HH.HTML q Input Message m
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
  , barriers: pure false
  , skills
  , gaps
  , options
  }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render { mode, selection, barriers, skills, gaps, options, health } =
  HH.div
    [ HP.id_ "view" ]
    [ HH.slot _table unit Table.component tableInput handleMessage
    , HH.div_
      [ HH.text $ "目標値: " <> (maybe "-" (show <<< (_ + 5)) $ MG.cost <$> graph <*> selection) ]
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
        table = (&&) <$> skills <*> MC.toSkillTable health
        transform = MGO.toTransform $ MO.toGraphOptions gaps barriers options
      in MG.build table transform

    route :: Array Skill
    route = fromMaybe [] $ MG.route <$> graph <*> selection

    costTable :: SkillTable Int
    costTable = maybe (MT.fill $ top) (MT.fillWith <<< MG.cost) graph
  
    renderMode :: forall w. Mode -> String -> HH.HTML w Action
    renderMode value label =
      HH.span
        [ HP.classes $ H.ClassName <$> cons "mode" if value == mode then ["checked"] else []
        , HE.onClick \_ -> Just $ SelectMode value
        ]
        [ HH.text label ]
  
    renderOption :: forall w i. Boolean -> String -> HH.HTML w i
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
          acquired = (if _ then ["acquired"] else []) <$> skills
          merged = (<>) <$> MC.toSkillTable categoryClasses <*> acquired
        in merged # execState do
          for_ route \skill -> modify_ $ MT.modify skill (_ `snoc` "route")
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
          let zipped = zip route (drop 1 route)
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
handleMessage (Table.CategoryClicked category) = Just $ ToggleCategory category
handleMessage (Table.SkillClicked skill) = Just $ SelectSkill skill
handleMessage (Table.GapClicked gap) = Just $ ToggleBarrier gap

handleAction :: forall m. Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  HandleInput input -> do
    H.put $ initialState input
  SelectMode mode -> do
    H.modify_ \s -> s { mode = mode }
  ToggleCategory category -> do
    H.modify_ \s -> s { health = MC.modify category not s.health }
  SelectSkill skill -> do
    H.modify_ \s -> s { selection = if Just skill == s.selection then Nothing else Just skill }
  ToggleBarrier gap -> do
    { gaps } <- H.get
    if not $ MC.lookup gap gaps
      then H.modify_ \s -> s { barriers = MC.modify gap not s.barriers }
      else pure unit