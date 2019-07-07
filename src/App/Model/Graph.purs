module App.Model.Graph
  ( SkillGraph
  , Transform(..)
  , Edge
  , Routes
  , build
  , routes
  , paths
  , subpaths
  , cost
  ) where

import Prelude

import App.Model.PriorityQueue as PQ
import App.Model.Skill (Skill)
import App.Model.Table (SkillTable)
import App.Model.Table as Table
import App.Model.Table.ST (STSkillTable)
import App.Model.Table.ST as ST
import Control.Monad.ST (ST, run, while)
import Data.Array (cons, fold, null, snoc, sortBy, uncons)
import Data.Foldable (for_)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set, difference, empty, insert, isEmpty, singleton, size, toUnfoldable, union)
import Data.Traversable (for)
import Partial.Unsafe (unsafePartial)

newtype SkillGraph = SkillGraph (SkillTable Node)

type Edge =
  { priority :: Int
  , cost :: Int
  , to :: Skill
  }

type Node =
  { cost :: Int
  , edges :: Array Edge
  , leaves :: Array Skill
  }

newtype Transform = Transform (Skill -> Set Skill -> Array Edge -> Array Edge)

newtype Routes = Routes (RouteTree Skill)

data RouteTree a = Branch a (RouteTree a) (RouteTree a) | Cons a (RouteTree a) | Nil

type SearchGraph r =
  { table :: STSkillTable r SearchNode
  , transform :: Transform
  }

type SearchNode =
  { cost :: Int
  , edges :: Set Edge
  , leaves :: Set Skill
  , closed :: Boolean
   }
  
type Entry =
  { cost :: Int
  , skill :: Skill
  }

instance showSkillGraph :: Show SkillGraph where
  show _ = "Graph"

instance semigroupTransform :: Semigroup Transform where
  append (Transform a) (Transform b) = Transform \x y -> a x y >>> b x y

instance monoidTransform :: Monoid Transform where
  mempty = Transform $ const $ const identity

instance semigroupRouteTree :: Semigroup (RouteTree a) where
  append a Nil = a
  append Nil b = b
  append (Cons x a) b = Branch x a b
  append (Branch x a b) c = Branch x a $ append b c
  
instance monoidRouteTree :: Monoid (RouteTree a) where
  mempty = Nil

derive newtype instance semigroupRoutes :: Semigroup Routes

derive newtype instance monoidRoutes :: Monoid Routes

build :: SkillTable Boolean -> Transform -> Maybe SkillGraph
build skills transform = SkillGraph <$> run do
  graph <- newGraph transform
  queue <- initGraph graph $ Table.collectKeys identity skills
  while (not <$> PQ.null queue) $ updateGraph graph queue
  result <- ST.freeze graph.table
  pure $ optimizeGraph result

newGraph :: forall r. Transform -> ST r (SearchGraph r)
newGraph transform = do
  table <- ST.fill
    { cost: top
    , edges: empty
    , leaves: empty
    , closed: false
    }
  pure { table, transform }

initGraph :: forall r. SearchGraph r -> Array Skill -> ST r (PQ.PriorityQueue r Entry)
initGraph { table } skills = do
  for_ skills \skill -> ST.modify skill (_ { cost = 0, leaves = singleton skill }) table
  PQ.fromFoldable $ { cost: 0, skill: _ } <$> skills

updateGraph :: forall r. SearchGraph r -> PQ.PriorityQueue r Entry -> ST r Unit
updateGraph { table, transform } queue = do
  { skill } <- unsafePartial fromJust <$> PQ.pop queue
  node <- ST.lookup skill table
  if node.closed
    then pure unit
    else do
      ST.modify skill (_ { closed = true }) table
      let edges = outEdges transform skill node.leaves
      for_ edges \edge -> do
        next <- ST.lookup edge.to table
        let total = node.cost + edge.cost
        case total `compare` next.cost of
          LT -> do
            table # ST.update edge.to
              { cost: total
              , edges: singleton $ edge { to = skill }
              , leaves: node.leaves
              , closed: false
              }
            PQ.push { cost: total, skill: edge.to } queue
          GT ->
            pure unit
          EQ -> do
            table # ST.modify edge.to \v -> v
              { edges = insert (edge { to = skill }) v.edges
              , leaves = union node.leaves v.leaves
              , closed = false
              }
            PQ.push { cost: total, skill: edge.to } queue

outEdges :: Transform -> Skill -> Set Skill -> Array Edge
outEdges (Transform transform) skill roots = transform skill roots []

optimizeGraph :: SkillTable SearchNode -> Maybe (SkillTable Node)
optimizeGraph table = for table \node ->
  if node.closed
    then Just
      { cost: node.cost
      , edges: pruneEdges table node.leaves $ toUnfoldable node.edges
      , leaves: toUnfoldable node.leaves
      }
    else Nothing

type EdgeLeaves = { edge :: Edge, leaves :: Set Skill }

pruneEdges :: SkillTable SearchNode -> Set Skill -> Array Edge -> Array Edge
pruneEdges table goals edges = step goals (defer \_ -> withLeaves <$> edges) []
  where
    step :: Set Skill -> Lazy (Array EdgeLeaves) -> Array Edge -> Array Edge
    step rest array acc =
      if isEmpty rest
        then acc
        else case uncons $ sortEdges $ force array of
          Just { head: x, tail: xs } ->
            step
              (difference rest x.leaves)
              (defer \_ -> xs <#> \y -> y { leaves = difference y.leaves x.leaves })
              (snoc acc x.edge)
          Nothing -> acc
    
    withLeaves :: Edge -> EdgeLeaves
    withLeaves edge = { edge, leaves: _.leaves $ Table.lookup edge.to table }

    sortEdges :: Array EdgeLeaves -> Array EdgeLeaves
    sortEdges = sortBy \a b -> compare (size b.leaves) (size a.leaves) <> compare b.edge a.edge

routes :: Skill -> SkillGraph -> Routes
routes to (SkillGraph table) = Routes $ walk to
  where
    walk :: Skill -> RouteTree Skill
    walk skill =
      let node @ { edges } = Table.lookup skill table
      in if null edges
        then Cons skill Nil
        else Cons skill $ fold $ walk <<< _.to <$> edges

paths :: Routes -> Array (Array Skill)
paths (Routes tree) = walk tree
  where
    walk :: forall a. RouteTree a -> Array (Array a)
    walk (Branch x a b) = walk (Cons x a) <> walk b
    walk (Cons x a) = (_ `snoc` x) <$> walk a
    walk Nil = [[]]

subpaths :: Routes -> Array (Array Skill)
subpaths (Routes tree) = walk [] tree
  where
    walk :: forall a. Array a -> RouteTree a -> Array (Array a)
    walk acc (Branch x a b) = walk acc (Cons x a) <> walk acc b
    walk acc (Cons x a @ (Branch _ _ _)) = walk [x] a <> walk acc (Cons x Nil)
    walk acc (Cons x a) = walk (cons x acc) a
    walk acc Nil = [acc]

cost :: Skill -> SkillGraph -> Int
cost skill (SkillGraph table) = _.cost $ Table.lookup skill table
