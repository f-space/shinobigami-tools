module App.Gesture
  ( component
  , Props
  , Renderer(..)
  , Query
  , Input
  , Message(..)
  , Slot
  , MonadType
  , Component
  ) where

import Prelude

import Data.Array ((..))
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Map (Map, delete, empty, insert, pop)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (preventDefault)
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.Touch as Touch
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.TouchEvent.TouchList as TouchList
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data PointerId = Mouse | Touch Int

type Props r =
  ( onMouseDown :: MouseEvent
  , onMouseUp :: MouseEvent
  , onMouseLeave :: MouseEvent
  , onTouchStart :: TouchEvent
  , onTouchEnd :: TouchEvent
  , onTouchCancel :: TouchEvent
  | r
  )

newtype Renderer = Renderer (forall w i. (forall r. Array (HP.IProp (Props r) i)) -> HH.HTML w i)

type State =
  { renderer :: Renderer
  , forkIds :: Map PointerId H.ForkId
  }

data Action
  = HandleInput Input
  | MouseDown MouseEvent
  | MouseUp MouseEvent
  | MouseLeave MouseEvent
  | TouchStart TouchEvent
  | TouchEnd TouchEvent
  | TouchCancel TouchEvent
  | Press PointerId
  | Release PointerId
  | Cancel PointerId

type Query = Const Void

type Input = Renderer

data Message
  = Tapped
  | LongTapped

type Slot slot = H.Slot Query Message slot

type ChildSlots = ()

type MonadType = Aff

type Component = H.Component HH.HTML Query Input Message MonadType

type ComponentHTML = H.ComponentHTML Action ChildSlots MonadType

derive instance eqPointerId :: Eq PointerId

derive instance ordPointerId :: Ord PointerId

component :: Component
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
initialState renderer = { renderer, forkIds: empty }

render :: State -> ComponentHTML
render { renderer: Renderer f } = f
    [ HE.onMouseDown \e -> MouseDown <$> mouseAction e
    , HE.onMouseUp \e -> MouseUp <$> mouseAction e
    , HE.onMouseLeave \e -> MouseLeave <$> mouseAction e
    , HE.onTouchStart \e -> Just $ TouchStart e
    , HE.onTouchEnd \e -> Just $ TouchEnd e
    , HE.onTouchCancel \e -> Just $ TouchCancel e
    ]
  where
    mouseAction :: MouseEvent -> Maybe MouseEvent
    mouseAction e = if MouseEvent.button e == 0 then Just e else Nothing

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput renderer -> do
    H.modify_ \s -> s { renderer = renderer }
  MouseDown e -> do
    H.liftEffect $ preventDefault $ MouseEvent.toEvent e
    handleAction $ Press Mouse
  MouseUp e -> do
    H.liftEffect $ preventDefault $ MouseEvent.toEvent e
    handleAction $ Release Mouse
  MouseLeave e -> do
    H.liftEffect $ preventDefault $ MouseEvent.toEvent e
    handleAction $ Cancel Mouse
  TouchStart e -> do
    H.liftEffect $ preventDefault $ TouchEvent.toEvent e
    foreachTouch e (handleAction <<< Press <<< Touch)
  TouchEnd e -> do
    H.liftEffect $ preventDefault $ TouchEvent.toEvent e
    foreachTouch e (handleAction <<< Release <<< Touch)
  TouchCancel e -> do
    H.liftEffect $ preventDefault $ TouchEvent.toEvent e
    foreachTouch e (handleAction <<< Cancel <<< Touch)
  Press id -> do
    forkId <- H.fork do
      H.liftAff $ delay $ Milliseconds 500.0
      H.modify_ \s -> s { forkIds = delete id s.forkIds }
      H.raise LongTapped
    H.modify_ \s -> s { forkIds = insert id forkId s.forkIds }
  Release id -> do
    { forkIds } <- H.get
    for_ (pop id forkIds) \(Tuple forkId rest) -> do
      H.kill forkId
      H.modify_ \s -> s { forkIds = rest }
      H.raise Tapped
  Cancel id -> do
    { forkIds } <- H.get
    for_ (pop id forkIds) \(Tuple forkId rest) -> do
      H.kill forkId
      H.modify_ \s -> s { forkIds = rest }
      
foreachTouch :: forall a m. Applicative m => TouchEvent -> (Int -> m a) -> m Unit
foreachTouch e f =
  let touches = TouchEvent.changedTouches e
  in for_ (0 .. (TouchList.length touches - 1)) \i ->
    let touch = unsafePartial fromJust $ TouchList.item i touches
    in f $ Touch.identifier touch