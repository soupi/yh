{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy where

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
import qualified Movement as MV


data Enemy
  = Enemy
  { _pos :: !IPoint
  , _size :: !Size
  , _movement :: !MV.Movement
  , _direction :: !FPoint
  , _texture :: SDL.Texture
  , _degree :: !Float
  , _health :: !Int
  , _timers :: !EnemyTimers
  }

data EnemyTimers
  = EnemyTimers
  { _bulletsTimer :: !Int
  , _hitTimer :: !Int
  }

makeFieldsNoPrefix ''Enemy
makeFieldsNoPrefix ''EnemyTimers

wantedAssets :: [(String, FilePath)]
wantedAssets =
  [ ("moon", "assets/moon2.png")
  ]

mkEnemy :: IPoint -> [(String, SDL.Texture)] -> Result Enemy
mkEnemy posi ts = do
  let textName = "moon"
  case lookup textName ts of
    Nothing ->
      throwError ["Texture not found: " ++ textName]
    Just txt ->
      pure $
        Enemy
          { _pos = posi
          , _size = Point 96 96
          , _direction = Point 0 1
          , _movement = MV.make (Point 0.1 0.1) (Point 1.5 1.5)
          , _texture = txt
          , _degree = 90
          , _health = 100
          , _timers = initEnemyTimers
          }

initEnemyTimers :: EnemyTimers
initEnemyTimers = EnemyTimers
  { _bulletsTimer = 2
  , _hitTimer = -1
  }

update :: Input -> Enemy -> Result ([Enemy], DL.DList Bullet -> DL.DList Bullet)
update input enemy = do
  wsize <- _windowSize <$> SM.get
  let
    dir = changeDirection wsize enemy
    (mv, move) =
      MV.update dir
        $ (enemy ^. movement)

    enemy' =
      enemy
      & over pos (`addPoint` move)
      & set movement mv
      & over timers (updateTimers (initEnemyTimers ^. bulletsTimer))
      & set direction dir
      & over degree (\d -> if d >= 360 then 1 else d+1.5)
  pure
    ( if enemy' ^. health <= 0 && enemy' ^. timers . hitTimer < 0 then [] else pure enemy'
    , addBullets enemy
    )

changeDirection :: Size -> Enemy -> FPoint
changeDirection wsize enemy
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



addBullets :: Enemy -> DL.DList Bullet -> DL.DList Bullet 
addBullets enemy
  | enemy ^. timers . bulletsTimer == 0
  --, enemy ^. timers . hitTimer < 0
  , enemy ^. direction . y == 0
  = let
      Point w h = Point (enemy ^. size . x) (enemy ^. size . y)
      d = (enemy ^. degree) * (pi / 180)
      dir = Point (cos d) (sin d)
    in DL.append $ DL.fromList
      [ mkBullet (enemy ^. texture) dir (Point 1 1) (fmap (3*) dir) 5 255
        $ ((enemy ^. pos) `addPoint` Point (enemy ^. size . x `div` 2) (enemy ^. size . y `div` 2))
          `addPoint` fmap (floor . (30*)) dir
      ]
  | otherwise = id

updateTimers :: Int -> EnemyTimers -> EnemyTimers
updateTimers bv et =
  et
  & over bulletsTimer  (\t -> if t <= 0 then bv else t - 1)
  & over hitTimer (\t -> if t <= 0 then -1 else t - 1)


checkHit :: DL.DList Bullet -> Enemy -> Enemy
checkHit bullets enemy
  | any (isJust . isTouching enemy) bullets && enemy ^. health > 0
  = enemy
    & over health (flip (-) (DL.head bullets ^. damage))
    & \enemy' -> set (timers . hitTimer) (if enemy' ^. health <= 0 then hitTimeout * 4 else hitTimeout) enemy'
  | otherwise
  = enemy

hitTimeout = 20

render :: SDL.Renderer -> Enemy -> IO ()
render renderer enemy = do
  let
    rect = toRect (enemy ^. pos) (enemy ^. size)
    h = fromIntegral $ enemy ^. health * 3
  if enemy ^. timers . hitTimer > 0 && enemy ^. timers . hitTimer `mod` 6 < 3
  then do
    SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 (255 - h) (255 - h) 150
    SDL.drawRect renderer (Just rect)
    SDL.fillRect renderer (Just rect)
  else
    SDL.copy renderer (enemy ^. texture) Nothing (Just rect)
