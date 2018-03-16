{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}

module Bullet where

import qualified SDL

import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens

data Bullet
  = Bullet
  { _pos :: !Point
  , _size :: !Size
  , _speed :: !Int
  , _damage :: !Int
  , _texture :: SDL.Texture
  }

makeFieldsNoPrefix ''Bullet

mkBullet :: SDL.Texture -> Int -> Int -> Point -> Bullet
mkBullet txt spd dmg position = Bullet
  { _pos = position `addPoint` Point (-12) (-24)
  , _size = Size 24 24
  , _speed = spd
  , _damage = dmg
  , _texture = txt
  }

updateBullet b =
  if b ^. pos . pY < 5
    then []
    else [over (pos . pY) (flip (-) (b ^. speed)) b]
