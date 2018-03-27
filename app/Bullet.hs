{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Bullet where

import qualified SDL

import Data.Tuple
import Data.Word
import Data.Maybe
import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens
import Control.DeepSeq
import qualified Data.Map as M
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

instance Eq Bullet where
  b1 == b2 =
    b1 ^. pos == b2 ^. pos
    && b1 ^. size == b2 ^. size
    && b1 ^. damage == b2 ^. damage

instance Ord Bullet where
  b1 <= b2 =
    b1 ^. pos <= b2 ^. pos
    && b1 ^. size <= b2 ^. size
    && b1 ^. damage <= b2 ^. damage

instance NFData Bullet where
  rnf (Bullet {_pos, _size, _direction, _movement, _damage, _transparency}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _direction
    `seq` rnf _movement
    `seq` rnf _damage
    `seq` rnf _transparency

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

update wsize entities b =
  case mapMaybe (isTouching b) entities of
    []
      | b ^. pos . y < 0 || b ^. pos . y > wsize ^. y
      -> ([], M.empty)
      | otherwise ->
        let
          (mv, addition) = MV.update (b ^. direction) $ b ^. movement
        in
          ([set movement mv $ over pos (`addPoint` addition) $ b], M.empty)
    es ->
      ([], fromListConcat $ map swap es)

fromListConcat :: Ord k => [(k, a)] -> M.Map k [a]
fromListConcat = foldr (uncurry $ M.insertWith (++)) M.empty . map (fmap (:[]))

render :: SDL.Renderer -> Bullet -> IO ()
render renderer bullet = do
  SDL.textureBlendMode (bullet ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (bullet ^. texture) SDL.$= (bullet ^. transparency)
  SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
