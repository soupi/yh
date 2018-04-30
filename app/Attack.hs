{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Attack where

import qualified SDL

import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens
import qualified Data.DList as DL

import Bullet
import qualified Play.Engine.Movement as MV


data Attack
  = Attack
  { _movement :: !MV.Movement
  , _outAngles :: ![Float]
  , _texture :: SDL.Texture
  , _attackTimerInit :: !Int
  , _attackTimer :: !Int
  , _attackUpdate :: IPoint -> IPoint -> Attack -> (DL.DList Bullet, Attack)
  }


makeFieldsNoPrefix ''Attack

make :: MV.Movement -> [Float] -> Int -> SDL.Texture
  -> (IPoint -> IPoint -> Attack -> (DL.DList Bullet, Attack))
  -> Attack
make mv angles every txt updater =
  Attack
    { _movement = mv
    , _outAngles = angles
    , _texture = txt
    , _attackTimerInit = every
    , _attackTimer = 60
    , _attackUpdate = updater
    }

update :: IPoint -> IPoint -> Attack -> (DL.DList Bullet, Attack)
update posi sz attack =
  let
    bullets'
      | attack ^. attackTimer == 0 = addBullets posi sz attack
      | otherwise = DL.empty

    attack' =
      attack
        & over attackTimer (\t -> if t <= 0 then attack ^. attackTimerInit else t - 1)

  in
    ( bullets'
    , attack'
    )


addBullets :: IPoint -> IPoint -> Attack -> DL.DList Bullet 
addBullets posi sz attack =
  let
    Point w h = Point (sz ^. x) (sz ^. y)
    ds = map ((pi / 180) *) (attack ^. outAngles)
    dirs = map (\d -> Point (cos d) (sin d)) ds
  in DL.fromList $ map
    (\dir ->
       mkBullet
         (attack ^. texture)
         dir
         (attack ^. movement
           & over MV.maxSpeed (fmap abs . mulPoint dir)
           & over MV.minSpeed (fmap abs . mulPoint dir)
           & over MV.speed (mulPoint dir)
         )
         5
         255
        $ (posi `addPoint` Point (w `div` 2) (h `div` 2))
          `addPoint` fmap (floor . (*) (fromIntegral $ w `div` 2)) dir
    ) dirs

