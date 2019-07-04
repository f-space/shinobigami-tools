module Test.PriorityQueue (spec) where

import Prelude

import App.Model.PriorityQueue (empty, head, null, pop, push)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.ST (run)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall m g. Monad m => MonadThrow Error g => SpecT g Unit m Unit
spec = describe "PriorityQueue" do
  it "empty-null" do
    (_ `shouldEqual` true) $ run do
      queue <- empty
      null queue
  it "push-null" do
    (_ `shouldEqual` false) $ run do
      queue <- empty
      push 42 queue
      null queue
  it "push-pop" do
    (_ `shouldEqual` Just 42) $ run do
      queue <- empty
      push 42 queue
      pop queue
  it "empty-pop" do
    (_ `shouldEqual` (Nothing :: Maybe Int)) $ run do
      queue <- empty
      pop queue
  it "push-pop-null" do
    (_ `shouldEqual` true) $ run do
      queue <- empty
      push 42 queue
      _ <- pop queue
      null queue
  it "push-head" do
    (_ `shouldEqual` Just 42) $ run do
      queue <- empty
      push 42 queue
      head queue
  it "empty-head" do
    (_ `shouldEqual` (Nothing :: Maybe Int)) $ run do
      queue <- empty
      head queue
  it "push-head-null" do
    (_ `shouldEqual` false) $ run do
      queue <- empty
      push 42 queue
      _ <- head queue
      null queue
  it "push-forward" do
    (_ `shouldEqual` Just [1, 2, 3]) $ run do
      queue <- empty
      push 1 queue
      push 2 queue
      push 3 queue
      a <- pop queue
      b <- pop queue
      c <- pop queue
      pure $ sequence [a, b, c]
  it "push-reverse" do
    (_ `shouldEqual` Just [1, 2, 3]) $ run do
      queue <- empty
      push 3 queue
      push 2 queue
      push 1 queue
      a <- pop queue
      b <- pop queue
      c <- pop queue
      pure $ sequence [a, b, c]