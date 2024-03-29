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
import Data.Array (concatMap, cons, elem, fromFoldable, (..))
import Data.Const (Const)
import Data.Foldable (for_, traverse_)
import Data.Map (Map, delete, empty, insert, values)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import FFI.CustomEvent (CustomEvent, customEvent, detail, eventType, toEvent, unsafeFromEvent)
import FFI.Document (elementFromPoint)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.DOM.Element (Element, tagName, toEventTarget, toNode)
import Web.DOM.Node (contains)
import Web.Event.Event (preventDefault)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.TouchEvent.Touch as TET
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TEL
import Web.UIEvent.MouseEvent as ME

data Pointer = Mouse | Touch Int

data Cell
  = CategoryCell SkillCategory
  | SkillCell Skill
  | GapCell SkillCategoryGap

type State =
  { input :: Input
  , pointers :: Map Pointer Cell
  }

data Action
  = HandleInput Input
  | MouseDown ME.MouseEvent
  | MouseUp ME.MouseEvent
  | MouseMove ME.MouseEvent
  | MouseLeave ME.MouseEvent
  | TouchStart TE.TouchEvent
  | TouchEnd TE.TouchEvent
  | TouchMove TE.TouchEvent
  | TouchCancel TE.TouchEvent
  | Hover Int Int Pointer
  | Select Int Int Pointer
  | Cancel Pointer
  | SetPointerCell Pointer Cell
  | SelectCategory SkillCategory
  | SelectSkill Skill
  | SelectGap SkillCategoryGap

type Query :: forall k. k -> Type
type Query = Const Void

type Input =
  { categoryClasses :: SkillColumn (Array String)
  , skillClasses :: SkillTable (Array String)
  , gapHeaderClasses :: SkillColumn (Array String)
  , gapClasses :: SkillTable (Array String)
  }

data Message
  = CategorySelected SkillCategory
  | SkillSelected Skill
  | GapSelected SkillCategoryGap

type Slot slot = H.Slot Query Message slot

type ChildSlots :: forall k. Row k
type ChildSlots = ()

type MonadType = Aff

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

type HoverEvent = CustomEvent "table-hover" Pointer
type SelectEvent = CustomEvent "table-select" Unit

derive instance eqPointer :: Eq Pointer
derive instance ordPointer :: Ord Pointer

derive instance eqCell :: Eq Cell

_hover :: Proxy HoverEvent
_hover = Proxy

_select :: Proxy SelectEvent
_select = Proxy

component :: H.Component Query Input Message MonadType
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
initialState = { input: _, pointers: empty }

render :: State -> ComponentHTML
render { input: { categoryClasses, skillClasses, gapHeaderClasses, gapClasses }, pointers } =
  HH.table
    [ HP.class_ $ H.ClassName "table"
    , HP.ref (H.RefLabel "table")
    , HE.onMouseDown MouseDown
    , HE.onMouseUp MouseUp
    , HE.onMouseMove MouseMove
    , HE.onMouseLeave MouseLeave
    , HE.onTouchStart TouchStart
    , HE.onTouchEnd TouchEnd
    , HE.onTouchMove TouchMove
    , HE.onTouchCancel TouchCancel
    ]
    [ HH.colgroup_ $ renderColumns categories
    , HH.thead_ [HH.tr_ $ renderHeaderRow categories]
    , HH.tbody_ $ HH.tr_ <<< flip renderRow categories <$> indices
    ]
  where
    hoverTargets :: Array Cell
    hoverTargets = fromFoldable $ values pointers
  
    renderColumns :: Array SkillCategory -> Array ComponentHTML
    renderColumns = concatMap \_ -> [ renderGapColumn, renderSkillColumn ]
  
    renderGapColumn :: ComponentHTML
    renderGapColumn = HH.col [ HP.class_ $ H.ClassName "table-col-gap" ]
    
    renderSkillColumn :: ComponentHTML
    renderSkillColumn = HH.col [ HP.class_ $ H.ClassName "table-col-skill" ]

    renderHeaderRow :: Array SkillCategory -> Array ComponentHTML
    renderHeaderRow = concatMap \c -> [ renderGapHeader $ leftGap c, renderSkillHeader c ]

    renderGapHeader :: SkillCategoryGap -> ComponentHTML
    renderGapHeader gap =
      let
        baseClasses = cons "table-gap-header" $ MC.lookup gap gapHeaderClasses
        hoverClass = if GapCell gap `elem` hoverTargets then ["hover"] else []
      in HH.th
        [ HP.classes $ H.ClassName <$> baseClasses <> hoverClass
        , onTableHover \e -> SetPointerCell (detail e) $ GapCell gap
        , onTableSelect \_ -> SelectGap gap
        ]
        []
    
    renderSkillHeader :: SkillCategory -> ComponentHTML
    renderSkillHeader category =
      let
        baseClasses = cons "table-category" $ MC.lookup category categoryClasses
        hoverClass = if CategoryCell category `elem` hoverTargets then ["hover"] else []
      in HH.th
        [ HP.classes $ H.ClassName <$> baseClasses <> hoverClass
        , onTableHover \e -> SetPointerCell (detail e) $ CategoryCell category
        , onTableSelect \_ -> SelectCategory category
        ]
        [ HH.text $ display category ]

    renderRow :: SkillIndex -> Array SkillCategory -> Array ComponentHTML
    renderRow index = concatMap \c -> [ renderGapCell index c, renderSkillCell index c ]

    renderGapCell :: SkillIndex -> SkillCategory -> ComponentHTML
    renderGapCell index category =
      let
        skill = Skill category index
        gap = leftGap category
        baseClasses = cons "table-gap" $ MT.lookup skill gapClasses
        hoverClass = if GapCell gap `elem` hoverTargets then ["hover"] else []
      in HH.td
        [ HP.classes $ H.ClassName <$> baseClasses <> hoverClass
        , onTableHover \e -> SetPointerCell (detail e) $ GapCell gap
        , onTableSelect \_ -> SelectGap gap
        ]
        []

    renderSkillCell :: SkillIndex -> SkillCategory -> ComponentHTML
    renderSkillCell index category = 
      let
        skill = Skill category index
        baseClasses = cons "table-skill" $ MT.lookup skill skillClasses
        hoverClass = if SkillCell skill `elem` hoverTargets then ["hover"] else []
      in HH.td
        [ HP.classes $ H.ClassName <$> baseClasses <> hoverClass
        , onTableHover \e -> SetPointerCell (detail e) $ SkillCell skill
        , onTableSelect \_ -> SelectSkill skill
        ]
        [ HH.text $ display skill ]

    onTableHover :: forall r i. (HoverEvent -> i) -> HP.IProp r i
    onTableHover handler = HE.handler (eventType _hover) $ handler <<< unsafeFromEvent
    
    onTableSelect :: forall r i. (SelectEvent -> i) -> HP.IProp r i
    onTableSelect handler = HE.handler (eventType _select) $ handler <<< unsafeFromEvent

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput input -> do
    H.modify_ \s -> s { input = input }
  MouseDown event -> do
    H.liftEffect $ preventDefault $ ME.toEvent event
    handleAction $ Hover (ME.pageX event) (ME.pageY event) Mouse
  MouseUp event -> do
    H.liftEffect $ preventDefault $ ME.toEvent event
    handleAction $ Select (ME.pageX event) (ME.pageY event) Mouse
  MouseMove event -> do
    H.liftEffect $ preventDefault $ ME.toEvent event
    handleAction $ Hover (ME.pageX event) (ME.pageY event) Mouse
  MouseLeave event -> do
    H.liftEffect $ preventDefault $ ME.toEvent event
    handleAction $ Cancel Mouse
  TouchStart event -> do
    H.liftEffect $ preventDefault $ TE.toEvent event
    foreachTouch event \touch ->
      handleAction $ Hover (TET.pageX touch) (TET.pageY touch) (Touch $ TET.identifier touch)
  TouchEnd event -> do
    H.liftEffect $ preventDefault $ TE.toEvent event
    foreachTouch event \touch ->
      handleAction $ Select (TET.pageX touch) (TET.pageY touch) (Touch $ TET.identifier touch)
  TouchMove event -> do
    H.liftEffect $ preventDefault $ TE.toEvent event
    foreachTouch event \touch ->
      handleAction $ Hover (TET.pageX touch) (TET.pageY touch) (Touch $ TET.identifier touch)
  TouchCancel event -> do
    H.liftEffect $ preventDefault $ TE.toEvent event
    foreachTouch event \touch ->
      handleAction $ Cancel (Touch $ TET.identifier touch)
  Hover x y pointer -> do
    result <- getPointedElement x y
    case result of
      Just element ->
        H.liftEffect $ do
          event <- customEvent _hover { detail: pointer }
          void $ dispatchEvent (toEvent event) (toEventTarget element)
      Nothing ->
        H.modify_ \s -> s { pointers = delete pointer s.pointers }
  Select x y pointer -> do
    getPointedElement x y >>= traverse_ \element ->
      H.liftEffect $ do
        event <- customEvent _select {}
        void $ dispatchEvent (toEvent event) (toEventTarget element)
    H.modify_ \s -> s { pointers = delete pointer s.pointers }
  Cancel pointer -> do
    H.modify_ \s -> s { pointers = delete pointer s.pointers }
  SetPointerCell pointer cell ->
    H.modify_ \s -> s { pointers = insert pointer cell s.pointers }
  SelectCategory category -> do
    H.raise $ CategorySelected category
  SelectSkill skill -> do
    H.raise $ SkillSelected skill
  SelectGap gap -> do
    H.raise $ GapSelected gap

foreachTouch :: forall m. Applicative m => TE.TouchEvent -> (TET.Touch -> m Unit) -> m Unit
foreachTouch event f =
  let
    touches = TE.changedTouches event
    length = TEL.length touches
  in for_ (0 .. (length - 1)) \i -> traverse f $ TEL.item i touches

getPointedElement :: forall state action slots msg. Int -> Int -> H.HalogenM state action slots msg MonadType (Maybe Element)
getPointedElement x y =
  H.getRef (H.RefLabel "table") >>= map join <<< traverse \table ->
    H.liftEffect $ window
      >>= document
      >>= (elementFromPoint x y <<< toDocument)
      >>= map join <<< traverse \element ->
        let tag = tagName element
        in if tag == "TD" || tag == "TH"
          then (if _ then Just element else Nothing) <$> contains (toNode table) (toNode element)
          else pure Nothing