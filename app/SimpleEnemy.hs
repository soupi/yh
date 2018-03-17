{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module SimpleEnemy where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Data.Maybe
import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except
import Control.Lens
import qualified Play.Engine.State as State
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL


import Bullet
import qualified Play.Engine.ScrollingBackground as SBG


data Enemy
  = Enemy
  { _pos :: !Point
  , _size :: !Size
  , _speed :: !Int
  , _direction :: !Point
  , _movementTimer :: !Int
  , _bulletsTimer :: !Int
  , _hitTimer :: !Int
  , _texture :: SDL.Texture
  , _health :: !Int
  }

makeFieldsNoPrefix ''Enemy

wantedAssets :: [(String, FilePath)]
wantedAssets =
  [ ("moon", "assets/moon.png")
  ]

mkEnemy :: Point -> [(String, SDL.Texture)] -> Result Enemy
mkEnemy posi ts = do
  case lookup "moon" ts of
    Nothing ->
      throwError ["Texture not found: moon"]
    Just moont ->
      pure $
        Enemy
          { _pos = posi
          , _size = Size 96 96
          , _speed = 1
          , _texture = moont
          , _direction = Point 0 1
          , _bulletsTimer = 30
          , _movementTimer = 250
          , _hitTimer = -1
          , _health = 50
          }

update :: Input -> Enemy -> Result ([Enemy], DL.DList Bullet -> DL.DList Bullet)
update input enemy = do
  wsize <- _windowSize <$> SM.get
  let
    addBullets
      | enemy ^. bulletsTimer == 0
      , enemy ^. hitTimer < 0
      , enemy ^. direction . pY == 0 =
        DL.append $ DL.fromList
          [ mkBullet (enemy ^. texture) (-6) 5 255
            ((enemy ^. pos) `addPoint` Point (enemy ^. size . sW `div` 2) (enemy ^. size . sH))
          ]
      | otherwise = id

    move
      | (enemy ^. hitTimer) < 0
      = (enemy ^. direction) `mulPoint` Point (enemy ^. speed) (enemy ^. speed)
      | otherwise = Point 0 0

    changeDirection
      | enemy ^. movementTimer == 0
      , enemy ^. direction . pY == 1
      = const (Point 1 0)

      | enemy ^. movementTimer == 0
      , enemy ^. pos . pX > (wsize ^. sW `div` 2)
      = flip addPoint $ Point (-1) 0

      | enemy ^. movementTimer == 0
      , enemy ^. pos . pX <= (wsize ^. sW `div` 2)
      = flip addPoint $ Point 1 0

      | enemy ^. movementTimer == 0
      = flip addPoint $ enemy ^. direction
  
      | otherwise = id

    changeSpeed
      | enemy ^. movementTimer == 0
      = const 1

      | otherwise
      = id

    enemy' =
      enemy
      & over pos (`addPoint` move)
      -- & fixPos wsize
      & over bulletsTimer  (\t -> if t <= 0 then 60  else t - 1)
      & over movementTimer (\t -> if t <= 0 then 90 else if enemy ^. hitTimer < 0 then t - 1 else t)
      & over hitTimer (\t -> if t <= 0 then -1 else t - 1)
      & over direction changeDirection
      & over speed changeSpeed
  pure
    ( if enemy' ^. health <= 0 && enemy' ^. hitTimer < 0 then [] else pure enemy'
    , addBullets
    )

checkHit :: DL.DList Bullet -> Enemy -> Enemy
checkHit bullets enemy
  | any (isJust . isTouching enemy) bullets && enemy ^. health > 0
  = enemy
    & over health (flip (-) (DL.head bullets ^. damage))
    & \enemy' -> set hitTimer (if enemy' ^. health <= 0 then hitTimeout * 4 else hitTimeout) enemy'
  | otherwise
  = enemy

hitTimeout = 20

render :: SDL.Renderer -> Enemy -> IO ()
render renderer enemy = do
  let
    rect = toRect (enemy ^. pos) (enemy ^. size)
    h = fromIntegral $ enemy ^. health * 3
  if enemy ^. hitTimer > 0 && enemy ^. hitTimer `mod` 6 < 3
  then do
    SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 (255 - h) (255 - h) 255
    SDL.drawRect renderer (Just rect)
    SDL.fillRect renderer (Just rect)
  else
    SDL.copy renderer (enemy ^. texture) Nothing (Just rect)
