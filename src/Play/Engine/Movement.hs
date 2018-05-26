
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

module Play.Engine.Movement where

import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens
import Control.DeepSeq
import GHC.Generics
import Data.Typeable

data Movement
  = Movement
  { _posFloatPart :: !FPoint
  , _speed :: !FPoint
  , _minSpeed :: !FPoint
  , _maxSpeed :: !FPoint
  , _acceleration :: !FPoint
  , _accelerationTimer :: !Int
  }
  deriving (Show, Eq, Generic, Typeable, NFData)

makeFieldsNoPrefix ''Movement

data MakeArgs
  = MakeArgs
  { startspeed :: !FPoint
  , minspeed :: !FPoint
  , maxspeed :: !FPoint
  , accel :: !FPoint
  }

defArgs :: MakeArgs
defArgs = MakeArgs
  { startspeed = Point 0 0
  , minspeed = Point 0 0
  , maxspeed = Point 1 1
  , accel = Point 0.1 0.1
  }

make :: MakeArgs -> Movement
make MakeArgs{..} =
  Movement
  { _posFloatPart = Point 0 0
  , _speed = startspeed
  , _minSpeed = fmap abs minspeed
  , _maxSpeed = fmap abs maxspeed
  , _acceleration = accel
  , _accelerationTimer = 0
  }

updateMovement :: FPoint -> Movement -> Movement
updateMovement !direction !mv =
  let
    updateSpeedC c spd =
      let
        accel =
          if direction ^. c /= 0
            then (mv ^. acceleration . c) * (direction ^. c)
            else limit (abs spd) $ (mv ^. acceleration . c) * negate (normalize spd)
      in
        if mv ^. accelerationTimer == 0
          then
            let
              newSpd = limit (mv ^. maxSpeed . c) . (+ accel) $ spd
              norm = normalize newSpd * (mv ^. minSpeed . c)
            in
              if abs newSpd < abs norm
                then norm
                else newSpd
          else spd
  in
    force $ mv
    & over (speed . x) (updateSpeedC x)
    & over (speed . y) (updateSpeedC y)
    & over accelerationTimer (\t -> if t <= 0 then 1 else t - 1)

limit :: Float -> Float -> Float
limit !lim !n
  | abs n > lim = normalize n * lim
  | otherwise = n

normalize :: Float -> Float
normalize !n
  | n == 0 = 0
  | n > 0 = n / n
  | otherwise = (-1) * (n / n)

update :: FPoint -> Movement -> (Movement, IPoint)
update !dir !mv =
  let
    spd = (mv ^. speed) `addPoint` (mv ^. posFloatPart)
    addition = fmap (\s -> if s < 0 then ceiling s else floor s) spd
    pfp = spd `addPoint` fmap (fromIntegral . negate) addition
  in
    force (set posFloatPart pfp $ updateMovement dir mv, addition)


straight :: FPoint -> Movement
straight spd = make $ defArgs
  { startspeed = spd
  , maxspeed = spd
  , accel = Point 1 1 `mulPoint` fmap abs spd
  }

fastGradualStart :: FPoint -> Movement
fastGradualStart spd = make $ defArgs
  { startspeed = spd
  , maxspeed = fmap (*4) spd
  , accel = Point 0.11 0.11 `mulPoint` fmap abs spd
  }

gradualSlowdown :: FPoint -> Movement
gradualSlowdown spd = make $ defArgs
  { startspeed = spd
  , minspeed = fmap (/4) spd
  , maxspeed = spd
  , accel = Point (-0.02) (-0.02) `mulPoint` fmap abs spd
  }
