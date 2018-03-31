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
import qualified Play.Engine.Movement as MV


data Enemy
  = Enemy
  { _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !Size
  , _movement :: {-# UNPACK #-} !MV.Movement
  , _direction :: {-# UNPACK #-} !FPoint
  , _directionChanger :: Size -> Enemy -> FPoint
  , _texture :: SDL.Texture
  , _attack :: {-# UNPACK #-} !A.Attack
  , _health :: {-# UNPACK #-} !Int
  , _timers :: {-# UNPACK #-} !EnemyTimers
  }
data EnemyTimers
  = EnemyTimers
  { _hitTimer :: {-# UNPACK #-} !Int
  , _gracePeriodTimer :: {-# UNPACK #-} !Int
  }


instance NFData Enemy where
  rnf (Enemy {_pos, _size, _movement, _health, _timers}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _movement
    `seq` rnf _timers
    `seq` rnf _health
    `seq` rnf _attack
    `seq` rnf _transparency

instance NFData EnemyTimers where
  rnf (EnemyTimers {_hitTimer}) =
    rnf _hitTimer
    `seq` rnf _gracePeriodTimer

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

data MakeEnemy
  = MakeEnemy
  { mkePos :: {-# UNPACK #-} !IPoint
  , mkeMov :: {-# UNPACK #-} !MV.Movement
  , mkeHealth :: {-# UNPACK #-} !Int
  , mkeDirChanger :: Size -> Enemy -> FPoint
  , mkeAtk :: {-# UNPACK #-} !A.Attack
  , mkeEnemyTxt :: SDL.Texture
  }

mkEnemy :: MakeEnemy -> Enemy
mkEnemy MakeEnemy{..} =
  Enemy
    { _pos = mkePos
    , _size = Point 96 96
    , _direction = Point 0 0
    , _movement = mkeMov
    , _directionChanger = mkeDirChanger
    , _texture = mkeEnemyTxt
    , _attack = mkeAtk
    , _health = mkeHealth
    , _timers = initEnemyTimers
    }

initEnemyTimers :: EnemyTimers
initEnemyTimers = EnemyTimers
  { _hitTimer = -1
  , _gracePeriodTimer = 60 * 5
  }

update :: Input -> Enemy -> Result ([Enemy], DL.DList Bullet -> DL.DList Bullet)
update input enemy = do
  wsize <- _windowSize <$> SM.get
  let
    dir = (enemy ^. directionChanger) wsize enemy
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
    ( if enemy' ^. health <= 0 && enemy' ^. timers . hitTimer < 0
        || not (isInWindow wsize (enemy ^. pos) (enemy ^. size))
           && (enemy' ^. timers . gracePeriodTimer) < 0
        then []
        else pure enemy'
    , if enemy' ^. health <= 0 && enemy' ^. timers . hitTimer < 0
        || not (isInWindow wsize (enemy ^. pos) (Point 0 0))
        then id
        else DL.append newBullets
    )

updateTimers :: EnemyTimers -> EnemyTimers
updateTimers et =
  et
    & over hitTimer (\t -> if t <= 0 then -1 else t - 1)
    & over gracePeriodTimer (\t -> if t <= 0 then -1 else t - 1)


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
