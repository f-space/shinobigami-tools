module App.Help
  ( component
  , Query
  , Input
  , Message(..)
  , Slot
  ) where

import Prelude

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
  | Close

type Query = Const Void

type Input = { open :: Boolean }

data Message = Closed

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
render { open } =
  HH.div
    [ HP.id_ "help-overlay"
    , HP.classes $ H.ClassName <$> if open then ["open"] else [] 
    , HE.onClick \_ -> Just Close
    ]
    [ HH.section
      [ HP.id_ "help-dialog" ]
      [ HH.h1_ [ HH.text "使い方" ]
      , HH.p_
        [ HH.text $
          "このアプリはトップ画面を除いて「設定」「判定」の二つの画面で構成されています。" <>
          "「設定」ではキャラクターの修得した特技や忍法などを設定します。" <>
          "「判定」では設定内容を元に指定特技に対する目標値の確認ができます。"
        ]
      , HH.h2_ [ HH.text "「設定」 画面" ]
      , HH.p_ [ HH.text "「設定」では次の設定ができます。" ]
      , HH.ul_
        [ HH.li_ [ HH.text "流派に対応したギャップ → 対応した分野を表から選択" ]
        , HH.li_ [ HH.text "修得特技 → 修得したい特技を表から選択" ]
        , HH.li_ [ HH.text "忍法『虚空蔵』 → 塗りつぶしたいギャップを表から選択" ]
        , HH.li_ [ HH.text "背景『異才』 → 塗りつぶしたいギャップを表から選択" ]
        , HH.li_ [ HH.text "忍法『魔界工学』 → 『魔界工学』を押す" ]
        , HH.li_ [ HH.text "忍法『木蓮』 → 『木蓮』を押す" ]
        , HH.li_ [ HH.text "忍法『妖理』 → 『妖理』を押して対象特技を表から選択" ]
        ]
      , HH.p_ [ HH.text "設定が終わったら『完了』を押して「判定」へと遷移します。" ]
      , HH.p_ [ HH.text "設定を最初からやり直したい場合には『白紙化』を押すと初期化できます。" ]
      , HH.h2_ [ HH.text "「判定」 画面" ]
      , HH.p_
        [ HH.text $
          "「判定」には【経路】と【分布】の二つのモードがあります。" <>
          "これらはそれぞれのボタンによって切り替えることができます。" <>
          "【経路】では指定特技に対して最小目標値の代用特技とその経路を表示します。" <>
          "【分布】では各特技の目標値の分布を一目で確認できます。" <>
          "どちらのモードであっても特技を選択するとその特技に対する最小目標値が表示されます。"
        ]
      , HH.p_ [ HH.text "「判定」では次の設定ができます。" ]
      , HH.ul_
        [ HH.li_ [ HH.text "生命力の減少・回復 → 対象の分野を表から選択" ]
        , HH.li_ [ HH.text "忍法『紐切』の設定・取消 → 対象のギャップを表から選択" ]
        , HH.li_ [ HH.text "変調『マヒ』の付加・回復 → 『マヒ』を押して対象特技を表から選択" ]
        ]
      ]
    ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message MonadType Unit
handleAction = case _ of
  HandleInput input -> do
    H.put input
  Close -> do
    H.raise Closed