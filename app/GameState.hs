
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except
import Control.Lens
import qualified Play.Engine.State as State
import qualified Play.Engine.LoadTextures as LT
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL


import Bullet
import qualified ShootingBox as SB
import qualified SimpleEnemy as Enemy
import qualified Play.Engine.ScrollingBackground as SBG

data State
  = State
  { _bg :: SBG.SBG
  , _mc :: SB.MainChar
  , _enemy :: Enemy.Enemy
  , _mcBullets :: DL.DList Bullet
  , _enemyBullets :: DL.DList Bullet
  , _textures :: [(String, SDL.Texture)]
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, FilePath)]
wantedAssets =
  [ ("bg", "assets/bg.png")
  ]
  ++ SB.wantedAssets
  ++ Enemy.wantedAssets

mkGameState :: State.State
mkGameState = LT.mkState wantedAssets mkState

mkState :: [(String, SDL.Texture)] -> Result State.State
mkState texts = do
  state <- initState texts
  pure $ State.mkState
    state
    update
    render

initState :: [(String, SDL.Texture)] -> Result State
initState ts = do
  case lookup "bg" ts of
    Nothing ->
      throwError ["Texture not found: bg"]
    Just bgt -> do
      mc' <- (SB.mkMainChar ts)
      enemy' <- (Enemy.mkEnemy (Point 200 (-180)) ts)
      pure $ State
        (SBG.mkSBG bgt 1 (Size 800 1000) (Point 0 0))
        mc'
        enemy'
        (DL.fromList [])
        (DL.fromList [])
        ts

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  (mc', addMCBullets) <- SB.update input (state ^. mc)
  (enemy', addEnemyBullets) <- Enemy.update input (state ^. enemy)
  let
    newState =
      state
        & set mc mc'
        & set enemy enemy'
        & over mcBullets (updateList (updateBullet wSize) . addMCBullets)
        & over enemyBullets (updateList (updateBullet wSize) . addEnemyBullets)
        & over bg SBG.updateSBG

  -- state stack manipulation
  if
    | keyReleased KeyB input -> do
      next <- mkState (state ^. textures)
      pure (State.Push next, state)
    | keyReleased KeyQuit input ->
      pure (State.Done, state)
    | otherwise ->
      pure (State.None, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  SBG.render renderer (state ^. bg)
  SB.render renderer (state ^. mc)
  Enemy.render renderer (state ^. enemy)
  forM_ (state ^. mcBullets) $ \bullet ->
    SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
  forM_ (state ^. enemyBullets) $ \bullet ->
    SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
