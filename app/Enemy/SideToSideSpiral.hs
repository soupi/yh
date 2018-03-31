
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy.SideToSideSpiral where

import qualified SDL
import qualified SDL.Primitive as SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import Control.Monad.Except
import Control.Lens

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Settings

import Bullet
import Enemy
import qualified Attack as A
import qualified Attack.SpiralAttack as SA
import qualified Play.Engine.Movement as MV


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("moon", MySDL.Texture "assets/imgs/moon2.png")
  ]


make :: IPoint -> [(String, SDL.Texture)] -> Result Enemy
make posi ts = do
  let textName = "moon"
  case lookup textName ts of
    Nothing ->
      throwError ["Texture not found: " ++ textName]
    Just txt ->
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeMov = leftRightMovement
          , mkeHealth = 100
          , mkeDirChanger = changeDirection
          , mkeAtk = singleSpiralAttack txt
          , mkeEnemyTxt = txt
          }

leftRightMovement :: MV.Movement
leftRightMovement = MV.make (Point 0.1 0.1) (Point 1.5 1.5)

singleSpiralAttack :: SDL.Texture -> A.Attack
singleSpiralAttack = SA.make 2 0 (2, 1.2) (Point 3 3)

changeDirection :: Size -> Enemy -> FPoint
changeDirection wsize enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  = Point 0 1

  | enemy ^. pos . y >= 100
  , enemy ^. direction . y == 1
  = Point 1 0

  | enemy ^. pos . x > 2 * (wsize ^. x `div` 3) - enemy ^. size . x
  , enemy ^. direction . y == 0
  = Point (-1) 0

  | enemy ^. pos . x <= (wsize ^. x `div` 3)
  , enemy ^. direction . y == 0
  = Point 1 0

  | enemy ^. health <= 0
  = Point 0 0

  | otherwise
  = enemy ^. direction

