
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module VN where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Data.Maybe
import Control.Monad.Except
import Control.Lens
import System.Random
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM
import qualified Data.Map as M

import qualified Script
import qualified Play.Engine.Sprite as Spr


data State
  = State
  { _bg :: Spr.Sprite
  , _resources :: MySDL.Resources
  , _script :: Script.Script
  , _camera :: Int
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "bg.png")
  ]

make :: Script.ScriptData -> State.State
make sd = Load.mkState 30 (wantedAssets ++ Script.assets sd) (mkState $ Script.script sd)

mkState
  :: (MySDL.Resources -> Script.Script)
  -> MySDL.Resources -> Result State.State
mkState scrpt rs = do
  state <- initState (scrpt rs) rs
  pure $ State.mkState
    state
    update
    render

initState :: Script.Script -> MySDL.Resources -> Result State
initState scrpt rs = do
  case M.lookup "bg" (MySDL.textures rs) of
    Nothing ->
      throwError ["Texture not found: bg"]
    Just bgt -> do
      pure $ State
        { _bg = fromJust $ Spr.make $ Spr.simpleArgs (Point 800 1000) bgt
        , _resources = rs
        , _script = scrpt
        , _camera = 0
        }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  _wSize <- _windowSize <$> SM.get

  (acts, script') <- Script.update input Nothing mempty (state ^. script)

  let
    newState =
      state'
        & set script script'
        & over camera
          (\c ->
             if
               | c <= 0 && Script.shake acts -> 60
               | c <= 0 -> 0
               | otherwise -> c - 1
          )
      where
        state' =
          if Script.stopTheWorld acts
            then
              state
            else
              state
                & over bg
                ( case Script.changeSprite acts of
                    Nothing -> Spr.update Nothing False
                    Just sp -> const sp
                )

  pure (Script.command acts, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  cam' <- Point <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) :: IO FPoint
  let cam = addPoint $ fmap (floor . (*) (fromIntegral $ state ^. camera `div` 3)) cam'
  Spr.render renderer cam (Point 0 0) (state ^. bg . size) (state ^. bg)
  Script.render renderer cam (state ^. script)

