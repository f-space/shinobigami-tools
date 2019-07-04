module App.Table
  ( component
  , Input
  , Message(..)
  , Slot
  ) where

import Prelude

import App.Model (Skill(..), SkillCategory, SkillCategoryGap, SkillColumn, SkillIndex, SkillTable, categories, display, indices, leftGap)
import App.Model.Column as MC
import App.Model.Table as MT
import Data.Array (concatMap, cons)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Input

data Action
  = HandleInput Input
  | ClickCategory SkillCategory
  | ClickSkill Skill
  | ClickGap SkillCategoryGap

type Input =
  { categoryClasses :: SkillColumn (Array String)
  , skillClasses :: SkillTable (Array String)
  , gapHeaderClasses :: SkillColumn (Array String)
  , gapClasses :: SkillTable (Array String)
  }

data Message
  = CategoryClicked SkillCategory
  | SkillClicked Skill
  | GapClicked SkillCategoryGap

type Slot index = H.Slot (Const Void) Message index

type ChildSlots = ()

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
initialState = identity

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render { categoryClasses, skillClasses, gapHeaderClasses, gapClasses } =
  HH.table
    [ HP.class_ $ H.ClassName "table" ]
    [ HH.colgroup_ $ renderColumns categories
    , HH.thead_ [HH.tr_ $ renderHeaderRow categories]
    , HH.tbody_ $ HH.tr_ <<< flip renderRow categories <$> indices
    ]
  where
    renderColumns :: forall w i. Array SkillCategory -> Array (HH.HTML w i)
    renderColumns = concatMap \c -> [ renderGapColumn, renderSkillColumn ]
  
    renderGapColumn :: forall w i. HH.HTML w i
    renderGapColumn = HH.col [ HP.class_ $ H.ClassName "table-col-gap" ]
    
    renderSkillColumn :: forall w i. HH.HTML w i
    renderSkillColumn = HH.col [ HP.class_ $ H.ClassName "table-col-skill" ]

    renderHeaderRow :: forall w. Array SkillCategory -> Array (HH.HTML w Action)
    renderHeaderRow = concatMap \c -> [ renderGapHeader $ leftGap c, renderSkillHeader c ]

    renderGapHeader :: forall w. SkillCategoryGap -> HH.HTML w Action
    renderGapHeader gap =
      HH.th
        [ HP.classes $ H.ClassName <$> (cons "table-gap" $ MC.lookup gap gapHeaderClasses)
        , HE.onClick \_ -> Just $ ClickGap gap
        ]
        []
    
    renderSkillHeader :: forall w. SkillCategory -> HH.HTML w Action
    renderSkillHeader category =
      HH.th
        [ HP.classes $ H.ClassName <$> (cons "table-category" $ MC.lookup category categoryClasses)
        , HE.onClick \_ -> Just $ ClickCategory category
        ]
        [ HH.text $ display category ]

    renderRow :: forall w. SkillIndex -> Array SkillCategory -> Array (HH.HTML w Action)
    renderRow index = concatMap \c -> [ renderGapCell index c, renderSkillCell index c ]

    renderGapCell :: forall w. SkillIndex -> SkillCategory -> HH.HTML w Action
    renderGapCell index category =
      let skill = Skill category index
      in HH.td
        [ HP.classes $ H.ClassName <$> (cons "table-gap" $ MT.lookup skill gapClasses)
        , HE.onClick \_ -> Just $ ClickGap $ leftGap category
        ]
        []

    renderSkillCell :: forall w. SkillIndex -> SkillCategory -> HH.HTML w Action
    renderSkillCell index category = 
      let skill = Skill category index
      in HH.td
        [ HP.classes $ H.ClassName <$> (cons "table-skill" $ MT.lookup skill skillClasses)
        , HE.onClick \_ -> Just $ ClickSkill skill
        ]
        [ HH.text $ display skill ]

handleAction :: forall m. Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  HandleInput input -> do
    H.put input
  ClickCategory category -> do
    H.raise $ CategoryClicked category
  ClickSkill skill -> do
    H.raise $ SkillClicked skill
  ClickGap gap -> do
    H.raise $ GapClicked gap