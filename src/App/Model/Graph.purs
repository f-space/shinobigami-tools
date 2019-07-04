module App.Model.Graph
  ( SkillGraph
  , Transform(..)
  , Edge
  , build
  , route
  , cost
  ) where

import Prelude

import App.Model.PriorityQueue as PQ
import App.Model.Skill (Skill, shift)
import App.Model.Table (SkillTable)
import App.Model.Table as Table
import App.Model.Table.ST (STSkillTable)
import App.Model.Table.ST as ST
import Control.Monad.ST (ST, run, while)
import Data.Array (concat, singleton, snoc)
import Data.Array.ST (STArray, empty, push, unsafeFreeze)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)

newtype SkillGraph = SkillGraph (SkillTable RouteNode)
newtype Transform = Transform (Skill -> Array Edge -> Array Edge -> Array Edge)
type Edge = { cost :: Int, to :: Skill }

newtype RouteEdge = RouteEdge Edge
type RouteNode = { cost :: Int, edge :: Maybe RouteEdge }
type SearchGraph r = { table :: STSkillTable r (Maybe RouteNode), transform :: Transform }
type Entry = { node :: RouteNode, skill :: Skill }

instance showSkillGraph :: Show SkillGraph where
  show _ = "Graph"

instance semigroupTransform :: Semigroup Transform where
  append (Transform a) (Transform b) = Transform \x y -> a x y >>> b x y
instance monoidTransform :: Monoid Transform where
  mempty = Transform $ const $ const identity

derive newtype instance eqRouteEdge :: Eq RouteEdge
instance ordRouteEdge :: Ord RouteEdge where
  compare (RouteEdge a) (RouteEdge b) = compare b.cost a.cost <> compare a.to b.to

build :: SkillTable Boolean -> Transform -> Maybe SkillGraph
build skills transform = SkillGraph <$> run do
  graph <- newGraph transform
  queue <- initGraph graph $ Table.collectKeys identity skills
  while (not <$> PQ.null queue) $ updateGraph graph queue
  result <- ST.freeze graph.table
  pure $ sequence result

newGraph :: forall r. Transform -> ST r (SearchGraph r)
newGraph transform = do
  table <- ST.fill Nothing
  pure { table, transform }

initGraph :: forall r. SearchGraph r -> Array Skill -> ST r (PQ.PriorityQueue r Entry)
initGraph { table } skills = do
  let node = { cost: 0, edge: Nothing }
  for_ skills (\skill -> ST.update skill (Just node) table)
  PQ.fromFoldable $ ({ node, skill: _ }) <$> skills

updateGraph :: forall r. SearchGraph r -> PQ.PriorityQueue r Entry -> ST r Unit
updateGraph { table, transform } queue = do
  { node, skill } <- unsafePartial fromJust <$> PQ.pop queue
  node' <- unsafePartial fromJust <$> ST.lookup skill table
  if node == node'
    then do
      path <- trace table node.edge
      for_ (outEdges transform skill path) \edge -> do
        let next = { cost: node.cost + edge.cost, edge: Just $ RouteEdge edge { to = skill } }
        next' <- ST.lookup edge.to table
        if maybe true (next < _) next'
          then do
            ST.update edge.to (Just next) table
            PQ.push { node: next, skill: edge.to } queue
          else pure unit
    else pure unit

outEdges :: Transform -> Skill -> Array Edge -> Array Edge
outEdges (Transform f) skill path = f skill path $ defaultEdges skill

trace :: forall r. STSkillTable r (Maybe RouteNode) -> Maybe RouteEdge -> ST r (Array Edge)
trace table edge =
  do
    path <- empty
    step edge path
    unsafeFreeze path
  where
    step :: Maybe RouteEdge -> STArray r Edge -> ST r Unit
    step e xs =
      for_ e \(RouteEdge x) -> do
        _ <- push x xs
        node <- unsafePartial fromJust <$> ST.lookup x.to table
        step node.edge xs

defaultEdges :: Skill -> Array Edge
defaultEdges skill = concat
  [ mkEdge 1 0 (-1)
  , mkEdge 1 0 1
  , mkEdge 2 (-1) 0
  , mkEdge 2 1 0
  ]
  where
    mkEdge :: Int -> Int -> Int -> Array Edge
    mkEdge c dx dy = maybe [] (singleton <<< { cost: c, to: _ }) $ shift skill dx dy

route :: SkillGraph -> Skill -> Array Skill
route (graph @ (SkillGraph table)) skill =
  let node = Table.lookup skill table
  in case node.edge of
    Just (RouteEdge edge) -> snoc (route graph edge.to) skill
    Nothing -> [skill]

cost :: SkillGraph -> Skill -> Int
cost (SkillGraph table) = _.cost <<< (_ `Table.lookup` table)
