{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Bullet where

import qualified SDL

import Data.Word
import Data.Maybe
import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens

import Debug.Trace


data Bullet
  = Bullet
  { _pos :: !IPoint
  , _posFloatPart :: !FPoint
  , _size :: !Size
  , _speed :: !FPoint
  , _maxSpeed :: !FPoint
  , _acceleration :: !FPoint
  , _direction :: !FPoint
  , _damage :: !Int
  , _texture :: SDL.Texture
  , _transparency :: !Word8
  , _accelerationTimer :: !Int
  , _movementTimer :: !Int
  }

makeFieldsNoPrefix ''Bullet

mkBullet :: SDL.Texture -> FPoint -> FPoint -> FPoint -> Int -> Word8 -> IPoint -> Bullet
mkBullet txt dir accel maxspeed dmg transp position = Bullet
  { _pos = fmap fromIntegral position `addPoint` Point (-6) (-12)
  , _posFloatPart = Point 0 0
  , _size = Point 12 12
  , _speed = Point 0 0
  , _maxSpeed = fmap abs maxspeed
  , _acceleration = accel
  , _direction = dir
  , _damage = dmg
  , _texture = txt
  , _transparency = transp
  , _accelerationTimer = 0
  , _movementTimer = 15
  }

updateDir :: Bullet -> Bullet
updateDir bullet =
  let
    accelX = bullet ^. acceleration . x * bullet ^. direction ^. x
    accelY = bullet ^. acceleration . y * bullet ^. direction ^. y
  in bullet
    & over (speed . x)
      ( if bullet ^. accelerationTimer == 0
          then limit (bullet ^. maxSpeed . x) . (+ accelX)
          else id
      )
    & over (speed . y)
      ( if bullet ^. accelerationTimer == 0
          then limit (bullet ^. maxSpeed . y) . (+ accelY)
          else id
      )
    & over accelerationTimer (\t -> if t <= 0 then 5 else t - 1)
    -- & over movementTimer (\t -> if t <= 0 then 60 else t - 1)

limit :: Float -> Float -> Float
limit lim n
  | abs n > lim = normalize n * lim
  | otherwise = n

normalize :: Float -> Float
normalize n
  | n == 0 = 0
  | n > 0 = n / n
  | otherwise = (-1) * (n / n)

update wsize entities b
  | any (isJust . isTouching b) entities =
    ([], True)
  | b ^. pos . y < 0 || b ^. pos . y > wsize ^. y =
    ([], False)
  | otherwise =
    let
      spd = (b ^. speed) `addPoint` (b ^. posFloatPart)
      addition = fmap (\s -> if s < 0 then ceiling s else floor s) spd
      pfp = spd `addPoint` fmap (fromIntegral . negate) addition
    in
      ([set posFloatPart pfp $ over pos (`addPoint` addition) $ updateDir b], False)


render :: SDL.Renderer -> Bullet -> IO ()
render renderer bullet = do
  SDL.textureBlendMode (bullet ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (bullet ^. texture) SDL.$= (bullet ^. transparency)
  SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
