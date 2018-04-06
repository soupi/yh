-- MySDL: some wrappers and utility functions around SDL

{-# LANGUAGE LambdaCase, TypeFamilies #-}

module Play.Engine.MySDL.MySDL where

import Data.Maybe
import Data.Either
import Data.Word (Word8)
import Data.Text (Text)
import Control.Exception
import Control.Monad (when)
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Concurrent.Async

import qualified Data.Word as Word
import qualified Foreign.C.Types as C
import qualified Data.Word as Word
import qualified Data.Map as M
import qualified Data.Text as T

import qualified SDL
import qualified SDL.Image as SDLI
import qualified SDL.Font as SDLF
import qualified Linear

import Debug.Trace

-- | Config window
myWindowConfig :: Linear.V2 C.CInt -> SDL.WindowConfig
myWindowConfig size = SDL.defaultWindow { SDL.windowInitialSize = size }

-- | Init SDL and create a Window and pass in as a parameter to function
withWindow :: MonadIO m => Text -> SDL.WindowConfig -> (SDL.Window -> m a) -> m a
withWindow title winConf go = do
  SDL.initializeAll
  SDLI.initialize [minBound..maxBound]
  SDLF.initialize

  window <- SDL.createWindow title winConf
  SDL.showWindow window

  result <- go window

  SDL.destroyWindow window
  SDLI.quit
  SDLF.quit
  SDL.quit

  pure result

withRenderer :: MonadIO m => SDL.Window -> ((SDL.Window, SDL.Renderer) -> m a) -> m a
withRenderer window go = do
  renderer <- SDL.createRenderer window (-1)
    $ SDL.RendererConfig
      { rendererType          = SDL.AcceleratedVSyncRenderer
      , rendererTargetTexture = False
      }
  go (window, renderer)


-- | App loop: takes the current world and functions that updates the world renders it
-- manage ticks, events and loop
apploop
  :: ResourcesT TVar
  -> TQueue Response
  -> SDL.Renderer
  -> a
  -> ([Response] -> [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> a -> IO (Either [String] ([Request], a)))
  -> (a -> IO ())
  -> IO a
apploop resources responsesQueue renderer world update render = do
  events <- collectEvents
  keyState <- SDL.getKeyboardState
  responses <- fmap (maybe [] (:[])) $ atomically $ tryReadTQueue responsesQueue
  update responses events keyState world >>= \case
    Left errs ->
      liftIO $ mapM (hPutStrLn stderr . ("*** Error: " ++)) errs >> pure world
    Right (reqs, newWorld) -> do
      render newWorld
      async $ mapConcurrently_ (runRequest resources responsesQueue renderer) reqs
      if checkEvent SDL.QuitEvent events
      then pure world
      else apploop resources responsesQueue renderer newWorld update render

setBGColor :: MonadIO m => Linear.V4 Word8 -> SDL.Renderer -> m SDL.Renderer
setBGColor color renderer = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  pure renderer

-- | Collect all events from inputs
collectEvents :: MonadIO m => m [SDL.EventPayload]
collectEvents = SDL.pollEvent >>= \case
    Nothing -> pure []
    Just e  -> (SDL.eventPayload e :) <$> collectEvents

-- | Checks if specific event happend
checkEvent :: SDL.EventPayload -> [SDL.EventPayload] -> Bool
checkEvent = elem

data Resource
  = RTexture SDL.Texture
  | RFont SDLF.Font

data ResourceType a
  = Texture a
  | Font a

data Request
  = Load ![(String, ResourceType FilePath)]
  | DestroyTexture SDL.Texture
  | MakeText (String, FilePath) T.Text

data Response
  = ResourcesLoaded Resources
  | NewText SDL.Texture
  | Exception String

data ResourcesT f
  = Resources
  { textures :: HKD f (M.Map FilePath SDL.Texture)
  , fonts :: HKD f (M.Map FilePath SDLF.Font)
  }

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

type Resources = ResourcesT Identity

initResources :: IO (ResourcesT TVar)
initResources =
  Resources
    <$> newTVarIO M.empty
    <*> newTVarIO M.empty

runRequest :: ResourcesT TVar -> TQueue Response -> SDL.Renderer -> Request -> IO ()
runRequest resources queue renderer req =
  flip catch (\(SomeException e) -> atomically $ writeTQueue queue $ Exception $ show e) $
    case req of
      Load files -> do
        results <-
          mapConcurrently
            (loadResource renderer resources)
            files
        atomically $ writeTQueue queue (resourcesToResponse results)
      DestroyTexture txt ->
        SDL.destroyTexture txt
      MakeText (n, p) txt -> do
        (_, RFont fnt) <- loadResource renderer resources (n, Font p)
        text <- SDL.createTextureFromSurface renderer =<< SDLF.solid fnt (Linear.V4 255 255 255 255) txt
        atomically $ writeTQueue queue $ NewText text

loadResource renderer resources (n, r) =
  case r of
    Texture f -> do
      mTxt <- atomically $ do
        txts <- readTVar (textures resources)
        pure $ M.lookup f txts
      (n,) . RTexture <$> case mTxt of
        Just txt ->
          pure txt
        Nothing -> do
          txt <- SDLI.loadTexture renderer f
          atomically $ do
            txts' <- readTVar (textures resources)
            writeTVar (textures resources) (M.insert f txt txts')
          pure txt
    Font f -> do
      mFont <- atomically $ do
        fnts <- readTVar (fonts resources)
        pure $ M.lookup f fnts
      (n,) . RFont <$> case mFont of
        Just fnt ->
          pure fnt
        Nothing -> do
          fnt <- SDLF.load f 18
          atomically $ do
            fnts' <- readTVar (fonts resources)
            writeTVar (fonts resources) (M.insert f fnt fnts')
          pure fnt

resourcesToResponse :: [(String, Resource)] -> Response
resourcesToResponse rs =
  ResourcesLoaded . foldr (flip f) initS $ rs
  where
    initS = Resources M.empty M.empty
    f s = \case
      (n, RTexture t) -> s { textures = M.insert n t (textures s) }
      (n, RFont f) -> s { fonts = M.insert n f (fonts s) }
