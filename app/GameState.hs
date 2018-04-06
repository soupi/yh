
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
import Control.Monad
import Control.Monad.Except
import Control.Lens
import System.Random
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load
import qualified Control.Monad.State as SM
import qualified Data.DList as DL
import qualified Data.Map as M

import qualified Script
import qualified Script.Level1 as L1
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
  , _resources :: MySDL.Resources
  , _script :: Script.Script
  , _camera :: Int
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "assets/imgs/bg.png")
  ]
  ++ SB.wantedAssets

mkGameState :: Script.ScriptData -> State.State
mkGameState sd = Load.mkState (wantedAssets ++ Script.assets sd) (mkState $ Script.script sd)

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
      mc' <- (SB.mkMainChar $ MySDL.textures rs)
      pure $ State
        (SBG.mkSBG bgt 1 (Point 800 1000) (Point 0 0))
        mc'
        []
        (DL.fromList [])
        (DL.fromList [])
        rs
        scrpt
        0

initEnemyTimer :: Int
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
    (mcBullets', _enemiesHit) =
      updateListWith M.empty (const $ const M.empty) (Bullet.update wSize (state ^. enemies)) . addMCBullets $ state ^. mcBullets

    (enemyBullets', _mcHit) =
      updateListWith M.empty (M.union) (Bullet.update wSize [state ^. mc]) . addEnemiesBullets $ state ^. enemyBullets

  let
    newState =
      state'
        & set script script'
        & set mcBullets mcBullets'
        & set enemyBullets enemyBullets'
        & over bg SBG.updateSBG
      where
        state' =
          if Script.stopTheWorld acts
            then
              state
            else
              state
                & set mc (SB.checkHit enemyBullets' mc')
                & set enemies enemies'
                & over enemies ((++) (Script.spawn acts) . map (Enemy.checkHit mcBullets'))

  -- state stack manipulation
  if
    | keyReleased KeyC input -> do
      pure (State.Push $ mkGameState L1.level1, state)
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
  cam' <- Point <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) :: IO FPoint
  let cam = addPoint $ fmap (floor . (*) (fromIntegral $ state ^. camera)) cam'
  SBG.render renderer cam (state ^. bg)
  SB.render renderer cam (state ^. mc)
  void $ traverse (Enemy.render renderer cam) (state ^. enemies)
  forM_ (state ^. mcBullets) (Bullet.render renderer cam)
  forM_ (state ^. enemyBullets) (Bullet.render renderer cam)
  Script.render renderer cam (state ^. script)

dirToInput :: IPoint -> Input
dirToInput dir =
  Input ks []
  where
    ks = M.fromList $ map (,Click)
      $  (if dir ^. x > 0 then [KeyRight] else if dir ^. x < 0 then [KeyLeft] else [])
      ++ (if dir ^. y > 0 then [KeyUp]    else if dir ^. y < 0 then [KeyDown] else [])

