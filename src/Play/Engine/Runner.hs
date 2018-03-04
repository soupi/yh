{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Play.Engine.Runner where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import qualified SDL
import qualified Linear
import Control.Lens
import Control.DeepSeq

import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Input
import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified Play.Engine.State as State

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
    keys = makeEvents (_keyStats settings) payload keyPressed (_keyMap settings)
  in pure
    . (keys `deepseq` runResult $! set keyStats keys settings)
    $ State.updater (Input keys) stack


render :: (SDL.Window, SDL.Renderer) -> Stack State.State -> IO ()
render (_, renderer) stack = do
  State.renderer renderer stack
  SDL.present renderer

setBGColorBlack :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColorBlack sdlStuff@(_, renderer) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff

