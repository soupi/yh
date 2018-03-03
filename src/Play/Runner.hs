{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Play.Runner where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import qualified SDL
import qualified Linear
import Control.Lens

import qualified Play.MySDL.MySDL as MySDL
import Play.Input
import Play.Types
import Play.Utils
import Play.Settings
import qualified Play.State as State

-----------
-- Logic --
-----------

runGame :: Stack State.State -> IO ()
runGame w = do
  putStrLn "Hello Game!"
  _ <- run def w
  putStrLn "Goodbye."

run :: Settings -> Stack State.State -> IO ()
run settings stack =
  void $ MySDL.withWindow "Game" (MySDL.myWindowConfig (Linear.V2 (winSize sW) (winSize sH))) $
    flip MySDL.withRenderer
      (setBGColorBlack >=> MySDL.apploop (settings, stack) update . (\ren -> render ren . snd))
  where
    winSize l = fromIntegral $ settings ^. windowSize . l

update
  :: [SDL.EventPayload]
  -> (SDL.Scancode -> Bool)
  -> (Settings, Stack State.State)
  -> IO (Either [String] (Settings, Stack State.State))
update payload keyPressed (settings, stack) =
  let
    events = makeEvents payload keyPressed (_keyMap settings)
  in pure
    . runResult settings
    $ State.updater (Input events) stack


render :: (SDL.Window, SDL.Renderer) -> Stack State.State -> IO ()
render (_, renderer) stack = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
--  drawRects (Lens.view CPU.gfx cpu) renderer
  State.renderer renderer stack
  SDL.present renderer

setBGColorBlack :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColorBlack sdlStuff@(_, renderer) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff

