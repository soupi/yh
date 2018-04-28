
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy.Static where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import qualified Data.Map as M
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
  [ ("moon", MySDL.Texture "moon2.png")
  , ("chikua", MySDL.Texture "chikua.png")
  ]

make :: IPoint -> FPoint -> Int -> M.Map String SDL.Texture -> Result Enemy
make posi dir targetY ts = do
  let textName = "moon"
  case (,) <$> M.lookup textName ts <*> M.lookup "chikua" ts of
    Nothing ->
      throwError ["Texture not found: chikua or " ++ textName]
    Just (bt, et) ->
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = Point 48 48
          , mkeMov = staticMovement dir
          , mkeHealth = 5
          , mkeDirChanger = changeDirection targetY
          , mkeAtk = sprayAttack bt
          , mkeAtkChanger = \_ _ -> Nothing
          , mkeEnemyTxt = et
          }

staticMovement :: FPoint -> MV.Movement
staticMovement dir = MV.make $ MV.defArgs
  { MV.maxspeed = Point 3 3
  , MV.accel = Point (dir ^. x * 0.1) (dir ^.y * 0.1)
  }

sprayAttack :: SDL.Texture -> A.Attack
sprayAttack = SA.make 5 0 (45, 15) $ SA.straight (Point 2 2)

changeDirection :: Int -> Size -> Enemy -> FPoint
changeDirection targetY _ enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  , enemy ^. pos . y < targetY
  = Point 1 1

  | enemy ^. pos . y >= targetY
  , enemy ^. direction . y == 1
  = Point 0 0

  | otherwise
  = enemy ^. direction

