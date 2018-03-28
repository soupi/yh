{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enemy where

import qualified SDL
import qualified SDL.Primitive as SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Data.Maybe
import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except
import Control.Lens
import Control.DeepSeq
import qualified Play.Engine.State as State
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL


import Bullet
import qualified Attack as A
import qualified Attack.SpiralAttack as SA
import qualified Movement as MV


data Enemy
  = Enemy
  { _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !Size
  , _movement :: {-# UNPACK #-} !MV.Movement
  , _direction :: {-# UNPACK #-} !FPoint
  , _texture :: SDL.Texture
  , _attack :: {-# UNPACK #-} !A.Attack
  , _health :: {-# UNPACK #-} !Int
  , _timers :: {-# UNPACK #-} !EnemyTimers
  }
data EnemyTimers
  = EnemyTimers
  { _hitTimer :: {-# UNPACK #-} !Int
  }


instance NFData Enemy where
  rnf (Enemy {_pos, _size, _movement, _health, _timers}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _movement
    `seq` rnf _timers
    `seq` rnf _health
    `seq` rnf _transparency

instance NFData EnemyTimers where
  rnf (EnemyTimers {_hitTimer}) =
    rnf _hitTimer

instance Eq Enemy where
  mc1 == mc2 =
    mc1 ^. pos == mc2 ^. pos
    && mc1 ^. size == mc2 ^. size

instance Ord Enemy where
  mc1 <= mc2 =
    mc1 ^. pos <= mc2 ^. pos
    && mc1 ^. size <= mc2 ^. size

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
          , _attack = SA.make 2 (3, 1) txt
          , _health = 100
          , _timers = initEnemyTimers
          }

initEnemyTimers :: EnemyTimers
initEnemyTimers = EnemyTimers
  { _hitTimer = -1
  }

update :: Input -> Enemy -> Result ([Enemy], DL.DList Bullet -> DL.DList Bullet)
update input enemy = do
  wsize <- _windowSize <$> SM.get
  let
    dir = changeDirection wsize enemy
    (mv, move) =
      MV.update dir
        $ (enemy ^. movement)

    (newBullets, attack') =
      (enemy ^. attack . A.attackUpdate) (enemy ^. pos) (enemy ^. size) (enemy ^. attack)

    enemy' =
      enemy
      & over pos (`addPoint` move)
      & set movement mv
      & over timers updateTimers
      & set direction dir
      & set attack attack'
  pure
    ( if enemy' ^. health <= 0 && enemy' ^. timers . hitTimer < 0 then [] else pure enemy'
    , if enemy' ^. health <= 0 && enemy' ^. timers . hitTimer < 0 then id else DL.append newBullets
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

updateTimers :: EnemyTimers -> EnemyTimers
updateTimers et =
  et
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
    h = fromIntegral $ 255 - max 0 (enemy ^. health * 2)
  if enemy ^. timers . hitTimer > 0 && enemy ^. timers . hitTimer `mod` 6 < 3
  then do
    let
      colour = Linear.V4 255 (255 - h) (255 - h) 150
      radius = fromIntegral $ enemy ^. size . x `div` 2
      center =
        Linear.V2
          (fromIntegral (enemy ^. pos . x) + radius)
          (fromIntegral (enemy ^. pos . y) + radius)
    SDL.circle renderer center radius colour
    SDL.fillCircle renderer center radius colour
  else
    SDL.copy renderer (enemy ^. texture) Nothing (Just rect)
