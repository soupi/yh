
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Movement where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Data.Maybe
import Control.Monad.Except
import Control.Lens
import qualified Play.Engine.State as State
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL




data Movement
  = Movement
  { _posFloatPart :: !FPoint
  , _speed :: !FPoint
  , _maxSpeed :: !FPoint
  , _acceleration :: !FPoint
  , _accelerationTimer :: !Int
  }

makeFieldsNoPrefix ''Movement

make :: FPoint -> FPoint -> Movement
make accel maxspeed =
  Movement
  { _posFloatPart = Point 0 0
  , _speed = Point 0 0
  , _maxSpeed = fmap abs maxspeed
  , _acceleration = accel
  , _accelerationTimer = 0
  }

updateMovement :: FPoint -> Movement -> Movement
updateMovement direction mv =
  let
    accelX = mv ^. acceleration . x * direction ^. x
    accelY = mv ^. acceleration . y * direction ^. y
  in mv
    & over (speed . x)
      ( if mv ^. accelerationTimer == 0
          then limit (mv ^. maxSpeed . x) . (+ accelX)
          else id
      )
    & over (speed . y)
      ( if mv ^. accelerationTimer == 0
          then limit (mv ^. maxSpeed . y) . (+ accelY)
          else id
      )
    & over accelerationTimer (\t -> if t <= 0 then 5 else t - 1)

limit :: Float -> Float -> Float
limit lim n
  | abs n > lim = normalize n * lim
  | otherwise = n

normalize :: Float -> Float
normalize n
  | n == 0 = 0
  | n > 0 = n / n
  | otherwise = (-1) * (n / n)

update :: FPoint -> Movement -> (Movement, IPoint)
update dir mv =
  let
    spd = (mv ^. speed) `addPoint` (mv ^. posFloatPart)
    addition = fmap (\s -> if s < 0 then ceiling s else floor s) spd
    pfp = spd `addPoint` fmap (fromIntegral . negate) addition
  in
    (set posFloatPart pfp $ updateMovement dir mv, addition)
