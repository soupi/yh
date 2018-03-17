{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}

module Bullet where

import qualified SDL

import Data.Word
import Data.Maybe
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
  , _transparency :: !Word8
  }

makeFieldsNoPrefix ''Bullet

mkBullet :: SDL.Texture -> Int -> Int -> Word8 -> Point -> Bullet
mkBullet txt spd dmg trans position = Bullet
  { _pos = position `addPoint` Point (-12) (-24)
  , _size = Size 24 24
  , _speed = spd
  , _damage = dmg
  , _texture = txt
  , _transparency = trans
  }

update wsize entities b
  | any (isJust . isTouching b) entities =
    ([], True)
  | b ^. pos . pY < 0 || b ^. pos . pY > wsize ^. sH =
    ([], False)
  | otherwise =
    ([over (pos . pY) (flip (-) (b ^. speed)) b], False)


render :: SDL.Renderer -> Bullet -> IO ()
render renderer bullet = do
  SDL.textureBlendMode (bullet ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (bullet ^. texture) SDL.$= (bullet ^. transparency)
  SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
