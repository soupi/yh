
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
import qualified Data.Map as M

import qualified Script
import qualified Script.Level1 as L1
import Bullet hiding (update, render)
import qualified Bullet
import qualified ShootingBox as SB
import qualified Enemy as Enemy
import qualified Enemy.SideToSideSpiral as SSE
import qualified Enemy.CrossDown as CD
import qualified Play.Engine.ScrollingBackground as SBG


data State
  = State
  { _bg :: SBG.SBG
  , _mc :: SB.MainChar
  , _enemies :: [Enemy.Enemy]
  , _mcBullets :: DL.DList Bullet
  , _enemyBullets :: DL.DList Bullet
  , _textures :: [(String, SDL.Texture)]
  , _script :: Script.Script
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, FilePath)]
wantedAssets =
  [ ("bg", "assets/imgs/bg.png")
  ]
  ++ SB.wantedAssets
  ++ SSE.wantedAssets

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
      pure $ State
        (SBG.mkSBG bgt 1 (Point 800 1000) (Point 0 0))
        mc'
        []
        (DL.fromList [])
        (DL.fromList [])
        ts
        (L1.level1 ts)

initEnemyTimer = 60

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get

  (acts, script') <- Script.update input (state ^. mc . pos) (state ^. enemies) (state ^. script)

  (mc', addMCBullets) <-
    SB.update
      (maybe input (dirToInput . dirToPlace (state ^. mc . pos)) (Script.moveMC acts))
      (state ^. mc)

  (enemies', addEnemiesBullets) <-
    bimap mconcat (foldr (.) id) . unzip <$> traverse (Enemy.update input) (state ^. enemies)
  let
    (mcBullets', enemiesHit) =
      updateListWith M.empty (const $ const M.empty) (Bullet.update wSize (state ^. enemies)) . addMCBullets $ state ^. mcBullets

    (enemyBullets', mcHit) =
      updateListWith M.empty (const $ const M.empty) (Bullet.update wSize [state ^. mc]) . addEnemiesBullets $ state ^. enemyBullets

  let
    newState =
      state
        & set mc (SB.checkHit enemyBullets' mc')
        & set enemies enemies'
        & over enemies ((++) (Script.spawn acts) . map (Enemy.checkHit mcBullets'))
        & set mcBullets mcBullets'
        & set enemyBullets enemyBullets'
        & over bg SBG.updateSBG
        & set script script'

  -- state stack manipulation
  if
    | keyReleased KeyC input -> do
      next <- mkState (state ^. textures)
      pure (State.Push next, state)
    | keyReleased KeyD input ->
      pure (State.Done, state)
    | otherwise ->
      pure (State.None, newState)

flipEnemyDir :: Either () () -> Either () ()
flipEnemyDir = \case
  Right () -> Left ()
  Left () -> Right ()

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  SBG.render renderer (state ^. bg)
  SB.render renderer (state ^. mc)
  traverse (Enemy.render renderer) (state ^. enemies)
  forM_ (state ^. mcBullets) (Bullet.render renderer)
  forM_ (state ^. enemyBullets) (Bullet.render renderer)

dirToInput :: IPoint -> Input
dirToInput dir =
  Input ks []
  where
    ks = M.fromList $ map (,Click)
      $  (if dir ^. x > 0 then [KeyRight] else if dir ^. x < 0 then [KeyLeft] else [])
      ++ (if dir ^. y > 0 then [KeyUp]    else if dir ^. y < 0 then [KeyDown] else [])
