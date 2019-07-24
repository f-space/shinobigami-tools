module ServiceWorker where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Array (filter)
import Data.Foldable (foldMap, traverse_)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import FFI.Fetch (fetch)
import FFI.GlobalThis (globalThis)
import FFI.ServiceWorker.Cache (addAll)
import FFI.ServiceWorker.CacheStorage (delete, keys, match, open)
import FFI.ServiceWorker.ExtendableEvent as EE
import FFI.ServiceWorker.FetchEvent as FE
import FFI.ServiceWorker.ServiceWorkerGlobalScope as SW
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)

cacheName :: String
cacheName = "v1.0.0"

fileUrls :: Array String
fileUrls =
  [ "."
  , "manifest.json"
  , "index.css"
  , "index.js"
  , "res/icon_192x192.png"
  , "res/icon_256x256.png"
  , "res/icon_512x512.png"
  , "res/icon_maskable.png"
  ]

main :: Effect Unit
main = globalThis >>= SW.fromGlobalThis >>> traverse_ \self -> do
  let target = SW.toEventTarget self
  setExtendableEventHandler "install" (onInstall self) target
  setExtendableEventHandler "activate" (onActivate self) target
  setFetchEventHandler "fetch" (onFetch self) target

setEventHandler :: forall e. String -> (Event -> e) -> (e -> Effect Unit) -> EventTarget -> Effect Unit
setEventHandler type_ fromEvent handler target = do
  listener <- eventListener $ handler <<< fromEvent
  addEventListener (EventType type_) listener false target

setExtendableEventHandler :: String -> (EE.ExtendableEvent -> Effect Unit) -> EventTarget -> Effect Unit
setExtendableEventHandler = flip setEventHandler $ unsafePartial fromJust <<< EE.fromEvent

setFetchEventHandler :: String -> (FE.FetchEvent -> Effect Unit) -> EventTarget -> Effect Unit
setFetchEventHandler = flip setEventHandler $ unsafePartial fromJust <<< FE.fromEvent

onInstall :: SW.ServiceWorkerGlobalScope -> EE.ExtendableEvent -> Effect Unit
onInstall self = EE.waitUntil do
  caches <- liftEffect $ SW.caches self
  cache <- open cacheName caches
  addAll fileUrls cache

onActivate :: SW.ServiceWorkerGlobalScope -> EE.ExtendableEvent -> Effect Unit
onActivate self = EE.waitUntil do
  caches <- liftEffect $ SW.caches self
  names <- keys caches
  sequential $ names
    # filter (_ /= cacheName)
    # map (void <<< flip delete caches)
    # foldMap parallel

onFetch :: SW.ServiceWorkerGlobalScope -> FE.FetchEvent -> Effect Unit
onFetch self event = event # FE.respondWith do
  let request = FE.request event
  caches <- liftEffect $ SW.caches self
  result <- match request caches
  case result of
    Just response -> pure response
    Nothing -> fetch request $ SW.toWindowOrWorkerGlobalScope self
