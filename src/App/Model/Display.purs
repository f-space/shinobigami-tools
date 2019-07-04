module App.Model.Display (class Display, display) where

import App.Model.Column as Column
import App.Model.Skill (Skill, SkillCategory)
import App.Model.Table as Table
import Partial.Unsafe (unsafePartial)

class Display a where
  display :: a -> String

instance displaySkillCategory :: Display SkillCategory where
  display category = Column.lookup category categoryNames

instance displaySkill :: Display Skill where
  display skill = Table.lookup skill skillNames

categoryNames :: Column.SkillColumn String
categoryNames = unsafePartial Column.unsafeWrap
  ["器術", "体術", "忍術", "謀術", "戦術", "妖術"]

skillNames :: Table.SkillTable String
skillNames = unsafePartial Table.unsafeWrap
  [ ["絡繰術", "火術", "水術", "針術", "仕込み", "衣装術", "縄術", "登術", "拷問術", "壊器術", "掘削術" ] 
  , ["騎乗術", "砲術", "手裏剣術", "手練", "身体操術", "歩法", "走法", "飛術", "骨法術", "刀術", "怪力" ]
  , ["生存術", "潜伏術", "遁走術", "盗聴術", "腹話術", "隠形術", "変装術", "香術", "分身の術", "隠蔽術", "第六感" ]
  , ["医術", "毒術", "罠術", "調査術", "詐術", "対人術", "遊芸", "九ノ一の術", "傀儡の術", "流言の術", "経済力" ]
  , ["兵糧術", "鳥獣術", "野戦術", "地の利", "意気", "用兵術", "記憶術", "見敵術", "暗号術", "伝達術", "人脈" ]
  , ["異形化", "召喚術", "死霊術", "結界術", "封術", "言霊術", "幻術", "瞳術", "千里眼の術", "憑依術", "呪術" ]
  ]