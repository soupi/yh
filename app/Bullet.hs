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
import qualified Movement as MV

import Debug.Trace


data Bullet
  = Bullet
  { _pos :: !IPoint
  , _size :: !Size
  , _direction :: !FPoint
  , _movement :: !MV.Movement
  , _damage :: !Int
  , _texture :: SDL.Texture
  , _transparency :: !Word8
  }

makeFieldsNoPrefix ''Bullet

mkBullet :: SDL.Texture -> FPoint -> FPoint -> FPoint -> Int -> Word8 -> IPoint -> Bullet
mkBullet txt dir accel maxspeed dmg transp position = Bullet
  { _pos = fmap fromIntegral position `addPoint` Point (-6) (-12)
  , _movement = MV.make accel maxspeed
  , _size = Point 12 12
  , _direction = dir
  , _damage = dmg
  , _texture = txt
  , _transparency = transp
  }

update wsize entities b
  | any (isJust . isTouching b) entities =
    ([], True)
  | b ^. pos . y < 0 || b ^. pos . y > wsize ^. y =
    ([], False)
  | otherwise =
    let
      (mv, addition) = MV.update (b ^. direction) $ b ^. movement
    in
      ([set movement mv $ over pos (`addPoint` addition) $ b], False)


render :: SDL.Renderer -> Bullet -> IO ()
render renderer bullet = do
  SDL.textureBlendMode (bullet ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (bullet ^. texture) SDL.$= (bullet ^. transparency)
  SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
