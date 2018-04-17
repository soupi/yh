{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Attack.SpiralAttack where

import qualified SDL

import Play.Engine.Types
import Control.Lens
import Control.DeepSeq
import qualified Data.DList as DL

import Bullet
import qualified Attack as A
import qualified Play.Engine.Movement as MV


make :: Int -> Int -> (Int, Float) -> MV.Movement -> SDL.Texture -> A.Attack
make numOfOutPoints initAngle' (every, change) mv txt =
  A.make
    mv
    (mkAngles initAngle' numOfOutPoints)
    every
    txt
    spiralUpdate

  where
    spiralUpdate :: IPoint -> IPoint -> A.Attack -> (DL.DList Bullet, A.Attack)
    spiralUpdate posi sz attack =
      A.update posi sz attack
        & over (_2 . A.outAngles) (map $ (+) change)

    mkAngles :: Int -> Int -> [Float]
    mkAngles initAngle ((`mod` 200) -> n) =
      let
        m = 360 / fromIntegral n
      in
        force $ map ((+) (fromIntegral initAngle) . (*) m . fromIntegral) [1..n]


straight :: FPoint -> MV.Movement
straight speed = MV.make $ MV.defArgs
  { MV.startspeed = Point 3 3
  , MV.maxspeed = speed
  , MV.accel = Point 1 1
  }

fastGradualStart :: FPoint -> MV.Movement
fastGradualStart speed = MV.make $ MV.defArgs
  { MV.startspeed = Point 1.5 1.5
  , MV.maxspeed = speed
  , MV.accel = Point 0.2 0.2
  }

gradualSlowdown :: FPoint -> MV.Movement
gradualSlowdown speed = MV.make $ MV.defArgs
  { MV.startspeed = speed
  , MV.minspeed = fmap (/4) speed
  , MV.maxspeed = speed
  , MV.accel = Point (-0.08) (-0.08)
  }
