
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy.CrossDown where

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

make :: IPoint -> Either () () -> [(String, SDL.Texture)] -> Result Enemy
make posi dir ts = do
  let textName = "moon"
  case lookup textName ts of
    Nothing ->
      throwError ["Texture not found: " ++ textName]
    Just txt ->
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeMov = crossMovement dir
          , mkeHealth = 100
          , mkeDirChanger = changeDirection
          , mkeAtk = downAttack txt
          , mkeEnemyTxt = txt
          }

crossMovement :: Either () () -> MV.Movement
crossMovement dir = MV.make (Point (mul 0.1) 0.3) (Point 4 2.5)
  where
    mul = case dir of
      Left () -> (*) (-1)
      Right () -> (*) 1

downAttack :: SDL.Texture -> A.Attack
downAttack = SA.make 1 90 (10, 0) (Point 8 8)

changeDirection :: Size -> Enemy -> FPoint
changeDirection wsize enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  = Point 0 1

  | enemy ^. pos . y >= 0
  , enemy ^. direction . y == 1
  , enemy ^. direction . x == 0
  = Point 1 1

  | otherwise
  = enemy ^. direction

