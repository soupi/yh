
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Play.Engine.Sprite where

import qualified SDL

import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens
import GHC.Generics
import Data.Typeable

import qualified Linear
import qualified Linear.Affine as Linear
import qualified Data.Map as M
import qualified Data.Text as T

data Sprite
  = Sprite
  { _actionmap :: M.Map T.Text Int
  , _action :: Int
  , _texture :: SDL.Texture
  , _size :: !Size
  , _initSpeed :: !Int
  , _speed :: !Int
  , _pos :: !Int
  , _maxPos :: !Int
  }
  deriving (Eq, Generic, Typeable)

makeFieldsNoPrefix ''Sprite


data MakeArgs
  = MakeArgs
  { mkActionmap :: M.Map T.Text Int
  , mkAction :: !T.Text
  , mkSpeed :: !Int
  , mkTexture :: !SDL.Texture
  , mkSize :: !Size
  , mkMaxPos :: !Int
  }

simpleArgs :: Size -> SDL.Texture -> MakeArgs
simpleArgs sz t = MakeArgs
  { mkActionmap = M.fromList [("normal", 0)]
  , mkAction = "normal"
  , mkSpeed = 0
  , mkTexture = t
  , mkSize = sz
  , mkMaxPos = 1
  }

make :: MakeArgs -> Maybe Sprite
make MakeArgs{..} = do
  act <- M.lookup mkAction mkActionmap
  pure $ Sprite
    { _actionmap = mkActionmap
    , _action = act
    , _texture = mkTexture
    , _initSpeed = mkSpeed
    , _speed = mkSpeed
    , _size = mkSize
    , _pos = 0
    , _maxPos = mkMaxPos
    }


update :: Maybe T.Text -> Bool -> Sprite -> Sprite
update !act !restart !sprite =
  sprite
    & over speed (\s -> if s < 0 || restart then sprite ^. initSpeed else s - 1)
    & over action (\a -> maybe a id $ flip M.lookup (sprite ^. actionmap) =<< act)
    & over pos (\p -> if restart then 0 else if sprite ^. speed == 0 then (p + 1) `mod` (sprite ^. maxPos) else p)

render :: SDL.Renderer -> Camera -> IPoint -> Size -> Sprite -> IO ()
render renderer cam position sz sprite = do
  let
    rect = toRect (cam $ position) sz
    ssz =  (sprite ^. size . y, sprite ^. size . y)
    clip = SDL.Rectangle
      (Linear.P $ Linear.V2 (fst ssz * sprite ^. pos) (snd ssz * sprite ^. action))
      (uncurry Linear.V2 ssz)
  SDL.textureBlendMode (sprite ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (sprite ^. texture) SDL.$= 255
  SDL.copy renderer (sprite ^. texture) (Just $ fmap fromIntegral clip) (Just rect)
