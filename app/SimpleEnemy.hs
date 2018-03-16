{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module SimpleEnemy where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

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
  , _texture :: SDL.Texture
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
          }

update :: Input -> Enemy -> Result (Enemy, DL.DList Bullet -> DL.DList Bullet)
update input enemy = do
  wsize <- _windowSize <$> SM.get
  let
    addBullets
      | enemy ^. bulletsTimer == 0
      , enemy ^. direction . pY == 0 =
        DL.append $ DL.fromList
          [ mkBullet (enemy ^. texture) (-6) 0 ((enemy ^. pos) `addPoint` Point (enemy ^. size . sW `div` 2) (enemy ^. size . sH))
          ]
      | otherwise = id

    move = (enemy ^. direction) `mulPoint` Point (enemy ^. speed) (enemy ^. speed)

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

  pure
    ( enemy
      & over pos (`addPoint` move)
      -- & fixPos wsize
      & over bulletsTimer  (\t -> if t <= 0 then 60  else t - 1)
      & over movementTimer (\t -> if t <= 0 then 90 else t - 1)
      & over direction changeDirection
      & over speed changeSpeed
    , addBullets
    )

render :: SDL.Renderer -> Enemy -> IO ()
render renderer enemy =
  SDL.copy renderer (enemy ^. texture) Nothing (Just $ toRect (enemy ^. pos) (enemy ^. size))
