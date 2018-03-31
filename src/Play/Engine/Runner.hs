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
import Control.Concurrent.STM.TQueue

import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Input
import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified Play.Engine.State as State

-----------
-- Logic --
-----------

runGame :: Settings -> Stack State.State -> IO ()
runGame sets w = do
  putStrLn "Hello Game!"
  _ <- run sets w
  putStrLn "Goodbye."

run :: Settings -> Stack State.State -> IO ()
run settings stack = do
  responsesQueue <- newTQueueIO
  resources <- MySDL.initResources
  void $ MySDL.withWindow "Game" (MySDL.myWindowConfig (Linear.V2 (winSize x) (winSize y))) $
    flip MySDL.withRenderer
      (setBGColorBlack >=> \(window, ren) -> MySDL.apploop resources responsesQueue ren (settings, stack) update (render (window, ren) . snd))
  where
    winSize l = fromIntegral $ settings ^. windowSize . l

update
  :: [MySDL.Response]
  -> [SDL.EventPayload]
  -> (SDL.Scancode -> Bool)
  -> (Settings, Stack State.State)
  -> IO (Either [String] ([MySDL.Request], (Settings, Stack State.State)))
update responses payload keyPressed (settings, stack) =
  let
    keys = makeEvents (_keyStats settings) payload keyPressed (_keyMap settings)
  in pure
    . fmap (\(setts, (reqs, states)) -> (reqs, (setts, states)))
    . (keys `deepseq` runResult $! set keyStats keys settings)
    $ State.updater (Input keys responses) stack


render :: (SDL.Window, SDL.Renderer) -> Stack State.State -> IO ()
render (_, renderer) stack = do
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  State.renderer renderer stack
  SDL.present renderer

setBGColorBlack :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColorBlack sdlStuff@(_, renderer) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff

