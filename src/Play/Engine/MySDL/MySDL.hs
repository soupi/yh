-- MySDL: some wrappers and utility functions around SDL

{-# LANGUAGE LambdaCase #-}

module Play.Engine.MySDL.MySDL where

import Data.Word (Word8)
import Data.Text (Text)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Word as Word
import qualified Foreign.C.Types as C
import qualified SDL
import qualified Linear
import System.IO
import Control.Concurrent

import Debug.Trace

-- | Config window
myWindowConfig :: Linear.V2 C.CInt -> SDL.WindowConfig
myWindowConfig size = SDL.defaultWindow { SDL.windowInitialSize = size }

-- | Init SDL and create a Window and pass in as a parameter to function
withWindow :: MonadIO m => Text -> SDL.WindowConfig -> (SDL.Window -> m a) -> m a
withWindow title winConf go = do
  SDL.initializeAll

  window <- SDL.createWindow title winConf
  SDL.showWindow window

  result <- go window

  SDL.destroyWindow window
  SDL.quit

  pure result

-- | Create a Surface and pass in as a parameter to function
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
  :: a
  -> ([SDL.EventPayload] -> (SDL.Scancode -> Bool) -> a -> IO (Either [String] a))
  -> (a -> IO ())
  -> IO a
apploop world update render = do
  events <- collectEvents
  keyState <- SDL.getKeyboardState
  update events keyState world >>= \case
    Left errs ->
      liftIO $ mapM (hPutStrLn stderr) errs >> pure world
    Right newWorld -> do
      render newWorld
      if checkEvent SDL.QuitEvent events
      then pure world
      else apploop newWorld update render

setBGColor :: MonadIO m => Linear.V4 Word8 -> SDL.Renderer -> m SDL.Renderer
setBGColor color renderer = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  pure renderer

-- | Update window
updateWindow :: SDL.Window -> IO ()
updateWindow = SDL.updateWindowSurface

-- | Collect all events from inputs
collectEvents :: MonadIO m => m [SDL.EventPayload]
collectEvents = SDL.pollEvent >>= \case
    Nothing -> pure []
    Just e  -> (SDL.eventPayload e :) <$> collectEvents

-- | Checks if specific event happend
checkEvent :: SDL.EventPayload -> [SDL.EventPayload] -> Bool
checkEvent = elem
