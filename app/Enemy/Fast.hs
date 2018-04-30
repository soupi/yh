
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy.Fast where

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
  , ("saito2", MySDL.Texture "saito2.png")
  ]

make :: IPoint -> M.Map String SDL.Texture -> Result Enemy
make posi ts = do
  let textName = "moon"
  case (,) <$> M.lookup textName ts <*> M.lookup "saito2" ts of
    Nothing ->
      throwError ["Texture not found: saito2 or " ++ textName]
    Just (bt, et) ->
      pure . mkEnemy $
        MakeEnemy
          { mkePos = posi
          , mkeSize = Point 96 96
          , mkeMov = crossMovement (Right ())
          , mkeHealth = 300
          , mkeDirChanger = changeDirection
          , mkeAtk = attackPattern bt
          , mkeAtkChanger = \enemy atkId ->
              if
                | enemy ^. health < 200 && atkId < 1 -> pure $ attackPattern2 bt
                | enemy ^. health < 100 && atkId < 2 -> pure $ attackPattern3 bt
                | otherwise -> Nothing

          , mkeEnemyTxt = et
          }

crossMovement :: Either () () -> MV.Movement
crossMovement dir = MV.make $ MV.defArgs
  { MV.maxspeed = Point 1.8 1
  , MV.accel = Point (mul 0.4) 0.2
  }
  where
    mul = case dir of
      Left () -> (*) (-1)
      Right () -> (*) 1

attackPattern :: SDL.Texture -> A.Attack
attackPattern = SA.make 5 0 (1, 9) $ SA.fastGradualStart (Point 14 14)

attackPattern2 :: SDL.Texture -> A.Attack
attackPattern2 = SA.make 3 0 (1, 7) $ SA.fastGradualStart (Point 14 14)

attackPattern3 :: SDL.Texture -> A.Attack
attackPattern3 = SA.make 5 0 (3, 7) $ SA.gradualSlowdown (Point 8 10)


changeDirection :: Size -> Enemy -> FPoint
changeDirection wsize enemy
  | enemy ^. direction . x == 0
  , enemy ^. direction . y == 0
  = Point 0 1

  | enemy ^. pos . y >= 0
  , enemy ^. direction . y == 1
  , enemy ^. direction . x == 0
  = Point 1 1

  | enemy ^. pos . y <= 0
  = enemy ^. direction

  | otherwise
  = enemy ^. direction
    & set x
      (if
         | enemy ^. pos . x > (wsize ^. x * 2) `div` 3 -> -1
         | enemy ^. pos . x <= wsize ^. x `div` 3 -> 1
         | otherwise -> enemy ^. direction . x
      )
    & set y
      (if
         | enemy ^. pos . y > 250 -> -1
         | enemy ^. pos . y <= 50 -> 1
         | otherwise -> enemy ^. direction . y
      )
