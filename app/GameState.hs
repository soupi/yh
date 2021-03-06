
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe
import Data.Foldable
import System.Random
import qualified Play.Engine.State as State
import qualified Play.Engine.Load as Load

import qualified Control.Monad.State as SM
import qualified Data.DList as DL
import qualified Data.Map as M

import qualified Script
import Bullet hiding (update, render)
import qualified Bullet
import qualified ShootingBox as SB
import qualified Enemy as Enemy
import qualified DecorationObject as DO
import qualified Play.Engine.ScrollingBackground as SBG


data State
  = State
  { _bg :: SBG.SBG
  , _mc :: SB.MainChar
  , _enemies :: [Enemy.Enemy]
  , _mcBullets :: DL.DList Bullet
  , _enemyBullets :: DL.DList Bullet
  , _decObjs :: DL.DList DO.DecorationObject
  , _resources :: MySDL.Resources
  , _script :: Script.Script
  , _camera :: Int
  , _restart :: State.State
  }

makeFieldsNoPrefix ''State

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("bg", MySDL.Texture "bg.png")
  , ("bga", MySDL.Texture "bga.png")
  ]
  ++ SB.wantedAssets

mkGameState :: Script.ScriptData -> State.State
mkGameState sd = Load.mkState 30 (wantedAssets ++ Script.assets sd) (mkState sd)

mkState
  :: Script.ScriptData
  -> MySDL.Resources -> Result State.State
mkState sd rs = do
  state <- initState sd (Script.script sd rs) rs
  pure $ State.mkState
    state
    update
    render

initState :: Script.ScriptData -> Script.Script -> MySDL.Resources -> Result State
initState sd scrpt rs = do
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
        (DL.fromList [])
        rs
        scrpt
        0
        (mkGameState sd)

initEnemyTimer :: Int
initEnemyTimer = 60

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get

  (acts, script') <- Script.update input (SB.get (state ^. mc) pos) (state ^. enemies) (state ^. script)

  (mc', addMCBullets) <-
    SB.update
      (maybe input (dirToInput . dirToPlace (state ^. mc . pos)) (Script.moveMC acts))
      (state ^. mc)

  (enemies', addEnemiesBullets, newDecObjs) <-
    (\(a, b, c) ->
       ( mconcat a
       , foldr (.) id b
       , foldr (\curr acc -> DL.fromList curr `DL.append` acc) DL.empty c
       )
    ) . unzip3
      <$> traverse (Enemy.update input) (state ^. enemies)
  let
    (mcBullets', _enemiesHit) =
      updateListWith M.empty (const $ const M.empty) (Bullet.update wSize (state ^. enemies)) $ state ^. mcBullets

    (enemyBullets', _mcHit) =
      updateListWith M.empty (M.union) (Bullet.update wSize (maybe [] (:[]) $ SB.get (state ^. mc) id)) $ state ^. enemyBullets

  updatedDecObjs <- updateDecObjs input (state ^. decObjs)

  let
    newState =
      state'
        & set script script'
        & over bg SBG.updateSBG
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
                & set mcBullets mcBullets'
                & set enemyBullets enemyBullets'
            else
              state
                & set mc (SB.checkHit enemyBullets' mc')
                & set enemies enemies'
                & over enemies ((++) (Script.spawn acts) . map (Enemy.checkHit mcBullets'))
                & set mcBullets (addMCBullets mcBullets')
                & set enemyBullets (addEnemiesBullets enemyBullets')
                & set decObjs (updatedDecObjs `DL.append` newDecObjs)
  if
    | keyReleased KeyC input -> do
      pure (State.Replace $ state ^. restart, state)
    | otherwise ->
      pure (Script.command acts, newState)


updateDecObjs :: Input -> DL.DList DO.DecorationObject -> Result (DL.DList DO.DecorationObject)
updateDecObjs input =
  foldrM
    ( \(DO.DecObj obj@(DO.DecObj'{..})) acc -> do
      (r, objs) <- _update input _state
      pure $ DL.append
        (DL.fromList $ maybeToList (fmap (DO.DecObj . flip (set DO.state) obj) r) ++ objs)
        acc
    )
    DL.empty


flipEnemyDir :: Either () () -> Either () ()
flipEnemyDir = \case
  Right () -> Left ()
  Left () -> Right ()

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  cam' <- Point <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) :: IO FPoint
  let cam = addPoint $ fmap (floor . (*) (fromIntegral $ state ^. camera `div` 3)) cam'
  SBG.render renderer cam (state ^. bg)
  SB.render renderer cam (state ^. mc)
  void $ traverse (Enemy.render renderer cam) (state ^. enemies)
  forM_ (state ^. decObjs) (\(DO.DecObj (DO.DecObj'{..})) -> _render renderer cam _state)
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

