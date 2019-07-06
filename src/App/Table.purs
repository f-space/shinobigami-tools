module App.Table
  ( component
  , Query
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
import Effect.Aff (Aff)
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

type Query = Const Void

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

type Slot slot = H.Slot Query Message slot

type ChildSlots = ()

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

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
initialState = identity

render :: State -> ComponentHTML
render { categoryClasses, skillClasses, gapHeaderClasses, gapClasses } =
  HH.table
    [ HP.class_ $ H.ClassName "table" ]
    [ HH.colgroup_ $ renderColumns categories
    , HH.thead_ [HH.tr_ $ renderHeaderRow categories]
    , HH.tbody_ $ HH.tr_ <<< flip renderRow categories <$> indices
    ]
  where
    renderColumns :: Array SkillCategory -> Array ComponentHTML
    renderColumns = concatMap \c -> [ renderGapColumn, renderSkillColumn ]
  
    renderGapColumn :: ComponentHTML
    renderGapColumn = HH.col [ HP.class_ $ H.ClassName "table-col-gap" ]
    
    renderSkillColumn :: ComponentHTML
    renderSkillColumn = HH.col [ HP.class_ $ H.ClassName "table-col-skill" ]

    renderHeaderRow :: Array SkillCategory -> Array ComponentHTML
    renderHeaderRow = concatMap \c -> [ renderGapHeader $ leftGap c, renderSkillHeader c ]

    renderGapHeader :: SkillCategoryGap -> ComponentHTML
    renderGapHeader gap =
      HH.th
        [ HP.classes $ H.ClassName <$> (cons "table-gap" $ MC.lookup gap gapHeaderClasses)
        , HE.onClick \_ -> Just $ ClickGap gap
        ]
        []
    
    renderSkillHeader :: SkillCategory -> ComponentHTML
    renderSkillHeader category =
      HH.th
        [ HP.classes $ H.ClassName <$> (cons "table-category" $ MC.lookup category categoryClasses)
        , HE.onClick \_ -> Just $ ClickCategory category
        ]
        [ HH.text $ display category ]

    renderRow :: SkillIndex -> Array SkillCategory -> Array ComponentHTML
    renderRow index = concatMap \c -> [ renderGapCell index c, renderSkillCell index c ]

    renderGapCell :: SkillIndex -> SkillCategory -> ComponentHTML
    renderGapCell index category =
      let skill = Skill category index
      in HH.td
        [ HP.classes $ H.ClassName <$> (cons "table-gap" $ MT.lookup skill gapClasses)
        , HE.onClick \_ -> Just $ ClickGap $ leftGap category
        ]
        []

    renderSkillCell :: SkillIndex -> SkillCategory -> ComponentHTML
    renderSkillCell index category = 
      let skill = Skill category index
      in HH.td
        [ HP.classes $ H.ClassName <$> (cons "table-skill" $ MT.lookup skill skillClasses)
        , HE.onClick \_ -> Just $ ClickSkill skill
        ]
        [ HH.text $ display skill ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput input -> do
    H.put input
  ClickCategory category -> do
    H.raise $ CategoryClicked category
  ClickSkill skill -> do
    H.raise $ SkillClicked skill
  ClickGap gap -> do
    H.raise $ GapClicked gap