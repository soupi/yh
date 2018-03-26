
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
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Lens
import qualified Play.Engine.State as State
import qualified Play.Engine.LoadTextures as LT
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL


import Bullet hiding (update, render)
import qualified Bullet
import qualified ShootingBox as SB
import qualified Enemy as Enemy
import qualified Play.Engine.ScrollingBackground as SBG

data State
  = State
  { _bg :: SBG.SBG
  , _mc :: SB.MainChar
  , _enemies :: [Enemy.Enemy]
  , _mcBullets :: DL.DList Bullet
  , _enemyBullets :: DL.DList Bullet
  , _textures :: [(String, SDL.Texture)]
  , _enemyTimer :: Int
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
        (SBG.mkSBG bgt 1 (Point 800 1000) (Point 0 0))
        mc'
        ([enemy'])
        (DL.fromList [])
        (DL.fromList [])
        ts
        200

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  (mc', addMCBullets) <- SB.update input (state ^. mc)
  (enemies', addEnemiesBullets) <-
    bimap mconcat (foldr (.) id) . unzip <$> traverse (Enemy.update input) (state ^. enemies)
  let
    (mcBullets', enemiesHit) =
      updateListWith False (||) (Bullet.update wSize (state ^. enemies)) . addMCBullets $ state ^. mcBullets

    (enemyBullets', mcHit) =
      updateListWith False (||) (Bullet.update wSize [state ^. mc]) . addEnemiesBullets $ state ^. enemyBullets

    addEnemyM
      | state ^. enemyTimer == 0 && length (state ^. enemies) < 2
      = (:) <$> Enemy.mkEnemy (Point 200 (-180)) (state ^. textures)

      | otherwise
      = pure id

  addEnemy <- addEnemyM

  let
    newState =
      state
        & set mc (SB.checkHit (state ^. enemyBullets) mc')
        & set enemies enemies'
        & over enemies (addEnemy . map (Enemy.checkHit (state ^. mcBullets)))
        & set mcBullets mcBullets'
        & set enemyBullets enemyBullets'
        & over bg SBG.updateSBG
        & over enemyTimer (\t -> if t <= 0 then 300 else t - 1)

  -- state stack manipulation
  if
    | keyReleased KeyC input -> do
      next <- mkState (state ^. textures)
      pure (State.Push next, state)
    | keyReleased KeyD input ->
      pure (State.Done, state)
    | otherwise ->
      pure (State.None, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  SBG.render renderer (state ^. bg)
  SB.render renderer (state ^. mc)
  traverse (Enemy.render renderer) (state ^. enemies)
  forM_ (state ^. mcBullets) (Bullet.render renderer)
  forM_ (state ^. enemyBullets) (Bullet.render renderer)
